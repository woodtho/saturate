/* saturate — coding interaction
 *
 * Handles:
 *  1. Text-selection offset capture (mouseup on .qc-text-display)
 *  2. Keyboard shortcuts for the coding panel
 *  3. Scroll-to-character-position for segment navigation
 *  4. Scroll-sync for the compare panel
 *  5. TTS with reading cursor and start-from-line support
 */
(function () {
  'use strict';

  window._qc_ns          = '';
  window._qc_sync_scroll = false;

  var _tts = {
    supported: !!(window.speechSynthesis && window.SpeechSynthesisUtterance),
    runId: 0,
    isSpeaking: false,
    isPaused: false,
    currentMode: 'idle',
    currentText: '',
    currentDocumentText: '',
    currentUtterance: null,
    lineMap: [],       // [{lineIdx, charStart, charEnd}] in speech-text coords
    chunkOffsets: [],  // charOffset of each chunk's start in the full speech text
    activeLineIdx: -1, // index into lineMap of the currently highlighted line
    clickOffset: null,       // DOM char offset of last user click in the document
    clickLineIdx: null,      // .qc-line index of last user click (line-numbers mode only)
    speechTextOffset: 0      // char offset into full docText where current speech starts
  };

  // ── Timestamp jump ─────────────────────────────────────────────────────────
  function _tsToSecs(str) {
    var p = str.trim().split(':').map(Number);
    if (p.some(isNaN)) return NaN;
    if (p.length === 3) return p[0] * 3600 + p[1] * 60 + p[2];
    if (p.length === 2) return p[0] * 60 + p[1];
    return NaN;
  }

  function jumpToTime(timeStr) {
    var target = _tsToSecs(timeStr);
    if (isNaN(target)) return;
    var container = document.querySelector('.qc-text-display');
    if (!container) return;
    var markers = Array.from(container.querySelectorAll('[data-ts]'));
    if (!markers.length) return;

    var best = markers[0], bestDiff = Infinity;
    markers.forEach(function(m) {
      var diff = Math.abs(_tsToSecs(m.dataset.ts) - target);
      if (diff < bestDiff) { bestDiff = diff; best = m; }
    });

    best.scrollIntoView({ behavior: 'smooth', block: 'center' });
    best.classList.remove('qc-ts-jump-flash');
    void best.offsetWidth; // reflow to restart animation
    best.classList.add('qc-ts-jump-flash');
    setTimeout(function() { best.classList.remove('qc-ts-jump-flash'); }, 1000);
  }

  // ── Namespace setup ────────────────────────────────────────────────────────
  Shiny.addCustomMessageHandler('qc_set_ns', function (msg) {
    window._qc_ns = msg.ns_prefix;
    setTimeout(_syncTtsControls, 0);
  });

  // ── Scroll to character position ───────────────────────────────────────────
  Shiny.addCustomMessageHandler('qc_scroll_to', function (msg) {
    setTimeout(function () {
      var pos       = msg.pos;
      var container = document.querySelector('.qc-text-display');
      if (!container) return;

      var marks = container.querySelectorAll('mark[data-selfirst]');
      var best  = null;
      var bestDist = Infinity;
      marks.forEach(function (m) {
        var sf   = parseInt(m.dataset.selfirst, 10);
        var dist = Math.abs(sf - pos);
        if (dist < bestDist) { bestDist = dist; best = m; }
      });
      if (best && bestDist < 500) {
        best.scrollIntoView({ behavior: 'smooth', block: 'center' });
        return;
      }

      var walker = document.createTreeWalker(
        container, NodeFilter.SHOW_TEXT, null, false);
      var n, count = 0;
      while ((n = walker.nextNode())) {
        var next = count + n.length;
        if (next >= pos) {
          try {
            var range = document.createRange();
            range.setStart(n, Math.min(pos - count, n.length));
            range.collapse(true);
            var rect  = range.getBoundingClientRect();
            var cRect = container.getBoundingClientRect();
            container.scrollTo({
              top: container.scrollTop + (rect.top - cRect.top) - cRect.height / 2,
              behavior: 'smooth'
            });
          } catch (e) {
            _scrollProportional(container, pos);
          }
          return;
        }
        count = next;
      }

      _scrollProportional(container, pos);
    }, 120);
  });

  function _scrollProportional(container, pos) {
    var total = container.textContent.length;
    if (total > 0) {
      container.scrollTop =
        Math.floor((pos / total) * (container.scrollHeight - container.clientHeight));
    }
  }

  // ── Compare-panel scroll sync ──────────────────────────────────────────────
  function _getTtsContainer() {
    if (!window._qc_ns) return null;
    var host = document.getElementById(window._qc_ns + 'text_display');
    return host ? host.querySelector('.qc-text-display') : null;
  }

  function _normalizeSpeechText(text) {
    var timestampPrefix = /^[ \t]*(?:\[(?:\d{1,2}:)?\d{1,2}:\d{2}(?:[.,]\d{1,3})?\]|(?:\d{1,2}:)?\d{1,2}:\d{2}(?:[.,]\d{1,3})?)[ \t-]*/gm;
    return (text || '')
      .replace(timestampPrefix, '')
      .replace(/ /g, ' ')
      .replace(/[ \t]+\n/g, '\n')
      .replace(/\n[ \t]+/g, '\n')
      .replace(/\n{3,}/g, '\n\n')
      .replace(/[ \t]{2,}/g, ' ')
      .trim();
  }

  function _getSelectedSpeechText(container) {
    var sel = window.getSelection();
    if (!sel || sel.isCollapsed || sel.rangeCount === 0) return '';
    var range = sel.getRangeAt(0);
    if (!container.contains(range.commonAncestorContainer)) return '';
    return _normalizeSpeechText(sel.toString());
  }

  function _getDocumentSpeechText(container) {
    if (!container) return '';
    var clone = container.cloneNode(true);
    clone.querySelectorAll('.qc-line-num, .qc-memo-icon').forEach(function (node) {
      node.remove();
    });
    if (clone.classList.contains('qc-line-numbers-on')) {
      var lines = [];
      clone.querySelectorAll('.qc-line').forEach(function (line) {
        var textNode = line.querySelector('.qc-line-text');
        lines.push(textNode ? textNode.textContent : line.textContent);
      });
      return _normalizeSpeechText(lines.join('\n'));
    }
    return _normalizeSpeechText(clone.textContent);
  }

  // ── TTS line map: maps char positions in speech text → .qc-line elements ──

  function _buildLineMap(container, fromLineIdx) {
    var lineEls = container ? container.querySelectorAll('.qc-line') : [];
    if (!lineEls.length) return [];
    var from = (typeof fromLineIdx === 'number' && fromLineIdx > 0) ? fromLineIdx : 0;
    var map = [];
    var charPos = 0;

    for (var i = from; i < lineEls.length; i++) {
      var clone = lineEls[i].cloneNode(true);
      clone.querySelectorAll('.qc-line-num, .qc-memo-icon').forEach(function (n) { n.remove(); });
      var textEl = clone.querySelector('.qc-line-text');
      var lineText = _normalizeSpeechText(textEl ? textEl.textContent : clone.textContent);

      map.push({
        lineIdx: i,
        charStart: charPos,
        charEnd: charPos + lineText.length
      });
      charPos += lineText.length + 1; // +1 for '\n' joining
    }
    return map;
  }

  function _findMapEntryAtPos(pos) {
    var map = _tts.lineMap;
    if (!map.length) return -1;
    var lo = 0, hi = map.length - 1;
    while (lo <= hi) {
      var mid = Math.floor((lo + hi) / 2);
      if (pos < map[mid].charStart) { hi = mid - 1; }
      else if (pos > map[mid].charEnd) { lo = mid + 1; }
      else { return mid; }
    }
    return Math.min(lo, map.length - 1);
  }

  // ── Click-start marker ────────────────────────────────────────────────────

  function _getOrCreateClickMark(container) {
    var m = container.querySelector('.qc-tts-click-mark');
    if (!m) {
      m = document.createElement('div');
      m.className = 'qc-tts-click-mark';
      m.setAttribute('aria-hidden', 'true');
      container.appendChild(m);
    }
    return m;
  }

  function _showClickMark(container, range) {
    try {
      var rect  = range.getBoundingClientRect();
      var cRect = container.getBoundingClientRect();
      var top   = container.scrollTop + (rect.top - cRect.top);
      var lineH = rect.height || parseFloat(getComputedStyle(container).lineHeight) || 24;
      var m = _getOrCreateClickMark(container);
      m.style.top    = Math.max(0, top) + 'px';
      m.style.height = lineH + 'px';
      m.style.display = 'block';
    } catch (e) {}
  }

  function _clearClickMark(container) {
    _tts.clickOffset  = null;
    _tts.clickLineIdx = null;
    if (!container) return;
    var m = container.querySelector('.qc-tts-click-mark');
    if (m) m.style.display = 'none';
  }

  // ── Start-position helpers ─────────────────────────────────────────────────

  // First .qc-line element whose top edge is at or below the container viewport.
  function _firstVisibleLine(container) {
    var lineEls = container.querySelectorAll('.qc-line');
    var cRect   = container.getBoundingClientRect();
    for (var i = 0; i < lineEls.length; i++) {
      if (lineEls[i].getBoundingClientRect().top >= cRect.top - 2) return i;
    }
    return 0;
  }

  // DOM char offset of the first text node visible at the container's top edge.
  function _findScrollStartOffset(container) {
    if (!container) return 0;
    var cRect = container.getBoundingClientRect();
    var filter = {
      acceptNode: function (node) {
        var p = node.parentNode;
        if (p && p.classList &&
            (p.classList.contains('qc-line-num') || p.classList.contains('qc-memo-icon'))) {
          return NodeFilter.FILTER_REJECT;
        }
        return NodeFilter.FILTER_ACCEPT;
      }
    };
    var walker = document.createTreeWalker(container, NodeFilter.SHOW_TEXT, filter, false);
    var n, count = 0;
    while ((n = walker.nextNode())) {
      if (!n.length) continue;
      try {
        var range = document.createRange();
        range.setStart(n, 0);
        range.collapse(true);
        if (range.getBoundingClientRect().top >= cRect.top - 2) return count;
      } catch (e) {}
      count += n.length;
    }
    return 0;
  }

  // ── Reading cursor ─────────────────────────────────────────────────────────

  // Returns (or creates) the needle <div> inside the container.
  // The needle is position:absolute so the container must be position:relative.
  function _getOrCreateNeedle(container) {
    var needle = container.querySelector('.qc-tts-needle');
    if (!needle) {
      needle = document.createElement('div');
      needle.className = 'qc-tts-needle';
      needle.setAttribute('aria-hidden', 'true');
      container.appendChild(needle);
    }
    return needle;
  }

  // Walk live text nodes to find the geometry of absolutePos.
  // Returns { top, height } in scroll-relative px, skipping line-number
  // and memo-icon nodes (same exclusions as the speech-text extractor).
  function _needleGeomForPos(container, absolutePos) {
    var filter = {
      acceptNode: function (node) {
        var p = node.parentNode;
        if (!p) return NodeFilter.FILTER_ACCEPT;
        if (p.classList &&
            (p.classList.contains('qc-line-num') ||
             p.classList.contains('qc-memo-icon'))) {
          return NodeFilter.FILTER_REJECT;
        }
        return NodeFilter.FILTER_ACCEPT;
      }
    };
    var walker = document.createTreeWalker(container, NodeFilter.SHOW_TEXT, filter, false);
    var n, count = 0;
    while ((n = walker.nextNode())) {
      var next = count + n.length;
      if (next >= absolutePos) {
        try {
          var range = document.createRange();
          range.setStart(n, Math.min(absolutePos - count, n.length));
          range.collapse(true);
          var rect  = range.getBoundingClientRect();
          var cRect = container.getBoundingClientRect();
          var top   = container.scrollTop + (rect.top - cRect.top);
          // rect.height from a collapsed range is the line height
          var lineH = rect.height || parseFloat(getComputedStyle(container).lineHeight) || 24;
          return { top: top, height: lineH };
        } catch (e) { break; }
      }
      count = next;
    }
    // Fallback: proportional estimate, no height info
    var total = container.textContent.length;
    var fallbackTop = total > 0 ? (absolutePos / total) * container.scrollHeight : 0;
    return { top: fallbackTop, height: 24 };
  }

  function _updateReadingCursor(absolutePos) {
    var container = _getTtsContainer();
    if (!container) return;

    // Line-mode: highlight the active .qc-line element
    if (_tts.lineMap.length > 0) {
      var mapIdx = _findMapEntryAtPos(absolutePos);
      if (mapIdx < 0) return;
      var lineIdx = _tts.lineMap[mapIdx].lineIdx;
      if (lineIdx !== _tts.activeLineIdx) {
        _tts.activeLineIdx = lineIdx;
        var lineEls = container.querySelectorAll('.qc-line');
        lineEls.forEach(function (el) { el.classList.remove('qc-tts-active-line'); });
        if (lineEls[lineIdx]) {
          lineEls[lineIdx].classList.add('qc-tts-active-line');
          lineEls[lineIdx].scrollIntoView({ behavior: 'smooth', block: 'nearest' });
        }
      }
      // Also move the needle so there's a visible indicator even in line mode
      var needle = _getOrCreateNeedle(container);
      if (container.querySelectorAll('.qc-line')[lineIdx]) {
        var el = container.querySelectorAll('.qc-line')[lineIdx];
        needle.style.top    = (el.offsetTop) + 'px';
        needle.style.height = el.offsetHeight + 'px';
        needle.classList.add('qc-tts-needle--line');
      }
      needle.style.display = 'block';
      return;
    }

    // No line structure: use Range geometry to position the needle
    var geom   = _needleGeomForPos(container, absolutePos + _tts.speechTextOffset);
    var needle = _getOrCreateNeedle(container);
    needle.style.top     = Math.max(0, geom.top) + 'px';
    needle.style.height  = geom.height + 'px';
    needle.style.display = 'block';
    needle.classList.remove('qc-tts-needle--line');

    // Scroll to keep needle visible
    var visible = container.clientHeight;
    var relTop  = geom.top - container.scrollTop;
    if (relTop < 40 || relTop > visible - 60) {
      container.scrollTo({ top: Math.max(0, geom.top - visible / 2), behavior: 'smooth' });
    }
  }

  function _clearReadingCursor() {
    _tts.activeLineIdx = -1;
    var container = _getTtsContainer();
    if (!container) return;
    container.querySelectorAll('.qc-tts-active-line').forEach(function (el) {
      el.classList.remove('qc-tts-active-line');
    });
    var needle = container.querySelector('.qc-tts-needle');
    if (needle) needle.style.display = 'none';
  }

  // ── Speech text chunking with offset tracking ──────────────────────────────

  function _splitIntoSentences(text) {
    var matches = text.match(/[^.!?]+[.!?]*\s*/g);
    if (!matches || !matches.length) return [text];
    return matches.map(function (part) {
      return _normalizeSpeechText(part);
    }).filter(Boolean);
  }

  function _appendWordChunks(chunks, text, maxChars) {
    var words = text.split(/\s+/);
    var current = '';
    words.forEach(function (word) {
      if (!word) return;
      var candidate = current ? current + ' ' + word : word;
      if (candidate.length <= maxChars) { current = candidate; return; }
      if (current) chunks.push(current);
      current = word;
      while (current.length > maxChars) {
        chunks.push(current.slice(0, maxChars));
        current = current.slice(maxChars).trim();
      }
    });
    if (current) chunks.push(current);
  }

  function _appendSpeechChunk(chunks, text, maxChars) {
    var cleanText = _normalizeSpeechText(text);
    if (!cleanText) return;
    if (cleanText.length <= maxChars) { chunks.push(cleanText); return; }
    var sentences = _splitIntoSentences(cleanText);
    if (sentences.length > 1) {
      var current = '';
      sentences.forEach(function (sentence) {
        var candidate = current ? current + ' ' + sentence : sentence;
        if (candidate.length <= maxChars) { current = candidate; return; }
        if (current) chunks.push(current);
        if (sentence.length <= maxChars) { current = sentence; }
        else { _appendWordChunks(chunks, sentence, maxChars); current = ''; }
      });
      if (current) chunks.push(current);
      return;
    }
    _appendWordChunks(chunks, cleanText, maxChars);
  }

  function _splitSpeechText(text) {
    var maxChars = 1200;
    var chunks = [];
    text.split(/\n{2,}/).forEach(function (paragraph) {
      _appendSpeechChunk(chunks, paragraph, maxChars);
    });
    return chunks.length ? chunks : [_normalizeSpeechText(text)];
  }

  // Returns { chunks, offsets } where offsets[i] is the char position of
  // chunk i in `text`. Used to map onboundary.charIndex → line map.
  function _splitSpeechTextWithOffsets(text) {
    var chunks = _splitSpeechText(text);
    var offsets = [];
    var searchFrom = 0;
    chunks.forEach(function (chunk) {
      var idx = text.indexOf(chunk, searchFrom);
      if (idx >= 0) {
        offsets.push(idx);
        searchFrom = idx + chunk.length;
      } else {
        offsets.push(searchFrom);
        searchFrom += chunk.length;
      }
    });
    return { chunks: chunks, offsets: offsets };
  }

  // ── Voice selection ────────────────────────────────────────────────────────

  function _pickSpeechVoice() {
    if (!window.speechSynthesis || !window.speechSynthesis.getVoices) return null;
    var voices = window.speechSynthesis.getVoices() || [];
    if (!voices.length) return null;

    var preferred = (document.documentElement.getAttribute('data-sat-tts-voice') || '').trim();
    if (preferred && preferred !== 'auto') {
      var match = voices.find(function(v) {
        return (v.voiceURI || v.name) === preferred;
      });
      if (match) return match;
    }

    var lang = (document.documentElement.lang || navigator.language || 'en-US').toLowerCase();
    var baseLang = lang.split('-')[0];
    return voices.find(function (v) { return v.lang && v.lang.toLowerCase() === lang; }) ||
           voices.find(function (v) { return v.lang && v.lang.toLowerCase().indexOf(baseLang) === 0; }) ||
           voices[0];
  }

  // ── Controls sync ──────────────────────────────────────────────────────────

  function _syncTtsControls() {
    if (!window._qc_ns) return;
    var ppBtn   = document.getElementById(window._qc_ns + 'tts_playpause');
    var stopBtn = document.getElementById(window._qc_ns + 'tts_stop');
    var container = _getTtsContainer();
    var hasText = !!_getDocumentSpeechText(container);

    if (ppBtn) {
      ppBtn.disabled = !_tts.supported || !hasText;
      var playing = _tts.isSpeaking && !_tts.isPaused;
      ppBtn.textContent = playing ? '⏸' : '▶';
      var label = playing ? 'Pause' : _tts.isPaused ? 'Resume' : 'Read aloud';
      ppBtn.title = label;
      ppBtn.setAttribute('aria-label', label);
    }
    if (stopBtn) stopBtn.disabled = !_tts.supported || !_tts.isSpeaking;
  }

  // ── Core TTS control ───────────────────────────────────────────────────────

  function _cancelTts() {
    _tts.runId += 1;
    if (window.speechSynthesis &&
        (window.speechSynthesis.speaking || window.speechSynthesis.pending || window.speechSynthesis.paused)) {
      window.speechSynthesis.cancel();
    }
    _tts.isSpeaking        = false;
    _tts.isPaused          = false;
    _tts.currentMode       = 'idle';
    _tts.currentText       = '';
    _tts.currentDocumentText = '';
    _tts.currentUtterance  = null;
    _tts.lineMap           = [];
    _tts.chunkOffsets      = [];
    _tts.speechTextOffset  = 0;
    _clearReadingCursor();
  }

  function _finishTts() {
    _tts.isSpeaking        = false;
    _tts.isPaused          = false;
    _tts.currentMode       = 'idle';
    _tts.currentText       = '';
    _tts.currentDocumentText = '';
    _tts.currentUtterance  = null;
    _tts.lineMap           = [];
    _tts.chunkOffsets      = [];
    _tts.speechTextOffset  = 0;
    _clearReadingCursor();
    _syncTtsControls();
  }

  function _speakChunk(runId, chunks, index) {
    if (!_tts.supported || runId !== _tts.runId) return;
    if (index >= chunks.length) { _finishTts(); return; }

    var utterance = new SpeechSynthesisUtterance(chunks[index]);
    var voice = _pickSpeechVoice();
    if (voice) utterance.voice = voice;
    utterance.lang  = (voice && voice.lang) || document.documentElement.lang || navigator.language || 'en-US';
    var ttsRate = parseFloat(document.documentElement.getAttribute('data-sat-tts-rate') || '1');
    utterance.rate  = (isFinite(ttsRate) && ttsRate >= 0.6 && ttsRate <= 1.8) ? ttsRate : 1;
    utterance.pitch = 1;

    // Reading cursor via boundary events (not supported in all browsers — degrades gracefully)
    var chunkOffset = _tts.chunkOffsets[index] || 0;
    utterance.onboundary = function (event) {
      if (runId !== _tts.runId) return;
      if (_tts.currentMode === 'selection') return;
      if (event.name !== 'word' && event.name !== 'sentence') return;
      _updateReadingCursor(chunkOffset + (event.charIndex || 0));
      _syncTtsControls();
    };

    utterance.onend = function () {
      if (runId !== _tts.runId) return;
      _tts.currentUtterance = null;
      _speakChunk(runId, chunks, index + 1);
    };
    utterance.onerror = function () {
      if (runId !== _tts.runId) return;
      _cancelTts();
      _syncTtsControls();
    };
    _tts.currentUtterance = utterance;
    window.speechSynthesis.speak(utterance);
  }

  function _launchTts(text, mode, lineMap, textOffset) {
    _cancelTts();
    var container = _getTtsContainer();

    _tts.isSpeaking          = true;
    _tts.isPaused            = false;
    _tts.currentMode         = mode;
    _tts.currentText         = text;
    _tts.currentDocumentText = _getDocumentSpeechText(container);
    _tts.lineMap             = lineMap || [];
    _tts.activeLineIdx       = -1;
    _tts.speechTextOffset    = textOffset || 0;

    var split = _splitSpeechTextWithOffsets(text);
    _tts.chunkOffsets = split.offsets;

    var runId  = _tts.runId;
    var chunks = split.chunks;
    _syncTtsControls();
    window.setTimeout(function () { _speakChunk(runId, chunks, 0); }, 0);
  }

  // Play button: selection → click point → scroll position → full document.
  function _startTts() {
    if (!_tts.supported) { _syncTtsControls(); return; }
    var container    = _getTtsContainer();
    var selectedText = container ? _getSelectedSpeechText(container) : '';
    var docText      = _getDocumentSpeechText(container);

    // Priority 1: highlighted text
    if (selectedText) {
      _clearClickMark(container);
      _launchTts(selectedText, 'selection', []);
      return;
    }

    if (!docText) { _syncTtsControls(); return; }

    // Priority 2 (line mode): click point or first visible line
    if (container && container.classList.contains('qc-line-numbers-on')) {
      var fromLine = (_tts.clickLineIdx !== null && _tts.clickLineIdx >= 0)
        ? _tts.clickLineIdx
        : _firstVisibleLine(container);
      _startTtsFromLine(fromLine);
      return;
    }

    // Priority 2 (free-text mode): click offset or scroll position → slice text from that point
    var domTotal = container.textContent.length;
    var speechOff;
    if (_tts.clickOffset !== null) {
      speechOff = domTotal > 0
        ? Math.round((_tts.clickOffset / domTotal) * docText.length)
        : 0;
    } else {
      var scrollDomOff = _findScrollStartOffset(container);
      speechOff = domTotal > 0
        ? Math.round((scrollDomOff / domTotal) * docText.length)
        : 0;
    }

    var sliced    = speechOff > 0 ? docText.slice(speechOff) : '';
    var startText = sliced.trim();
    var ttsText   = startText || docText;
    var ttsOffset = (ttsText === docText) ? 0 : speechOff;
    _clearClickMark(container);
    _launchTts(ttsText, 'document', [], ttsOffset);
  }

  // Click on a line number: read from that line to end of document
  function _startTtsFromLine(lineIdx) {
    if (!_tts.supported) { _syncTtsControls(); return; }
    var container = _getTtsContainer();
    if (!container || !container.classList.contains('qc-line-numbers-on')) {
      _startTts();
      return;
    }

    var lineEls = container.querySelectorAll('.qc-line');
    if (!lineEls.length || lineIdx >= lineEls.length) { _startTts(); return; }

    // Build text and line map starting from lineIdx
    var parts   = [];
    var lineMap = [];
    var charPos = 0;

    for (var i = lineIdx; i < lineEls.length; i++) {
      var clone = lineEls[i].cloneNode(true);
      clone.querySelectorAll('.qc-line-num, .qc-memo-icon').forEach(function (n) { n.remove(); });
      var textEl = clone.querySelector('.qc-line-text');
      var lineText = _normalizeSpeechText(textEl ? textEl.textContent : clone.textContent);
      lineMap.push({ lineIdx: i, charStart: charPos, charEnd: charPos + lineText.length });
      parts.push(lineText);
      charPos += lineText.length + 1;
    }

    var text = parts.join('\n');
    if (!text) { _syncTtsControls(); return; }
    _launchTts(text, 'document', lineMap);
  }

  function _toggleTtsPause() {
    if (!_tts.supported || !_tts.isSpeaking) return;
    if (_tts.isPaused) {
      window.speechSynthesis.resume();
      _tts.isPaused = false;
    } else {
      window.speechSynthesis.pause();
      _tts.isPaused = true;
    }
    _syncTtsControls();
  }

  function _stopTts() {
    if (!_tts.supported) { _syncTtsControls(); return; }
    _cancelTts();
    _syncTtsControls();
  }

  function _handleTtsOutputUpdate() {
    var container    = _getTtsContainer();
    var documentText = _getDocumentSpeechText(container);
    if (_tts.isSpeaking && documentText !== _tts.currentDocumentText) {
      _cancelTts();
    }
    _clearClickMark(container);
    _syncTtsControls();
  }

  // ── Compare-panel scroll sync ──────────────────────────────────────────────
  Shiny.addCustomMessageHandler('qc_compare_sync', function (msg) {
    window._qc_sync_scroll = !!msg.enabled;
    if (msg.enabled) _attachSyncListeners();
  });

  var _syncing = false;
  function _onSyncScroll() {
    if (!window._qc_sync_scroll || _syncing) return;
    _syncing = true;
    var top = this.scrollTop;
    document.querySelectorAll('.qc-compare-panel .qc-text-display').forEach(
      function (p) { p.scrollTop = top; }
    );
    _syncing = false;
  }

  function _attachSyncListeners() {
    document.querySelectorAll('.qc-compare-panel .qc-text-display').forEach(function (p) {
      p.removeEventListener('scroll', _onSyncScroll);
      p.addEventListener('scroll', _onSyncScroll);
    });
  }

  $(document).on('shiny:value', function (e) {
    if (window._qc_sync_scroll && e.name && e.name.indexOf('text') !== -1) {
      setTimeout(_attachSyncListeners, 150);
    }
    if (e.name === window._qc_ns + 'text_display') {
      // shiny:value fires before renderUI replaces the DOM — snapshot scroll now
      var _sc = document.querySelector('.qc-text-display');
      var _prevTop = _sc ? _sc.scrollTop : 0;
      var _prevLen = _sc ? _sc.textContent.length : 0;
      setTimeout(_handleTtsOutputUpdate, 75);
      // Restore scroll after DOM update, but only for same document (same text length)
      setTimeout(function () {
        var c = document.querySelector('.qc-text-display');
        if (c && c.textContent.length === _prevLen) c.scrollTop = _prevTop;
      }, 0);
    }
  });

  // ── Click on a coding mark → open edit panel ──────────────────────────────
  var _qc_mark_pending = false;

  $(document).on('mousedown', '.qc-text-display mark[data-coding-ids]', function () {
    _qc_mark_pending = true;
  });

  $(document).on('mouseup', '.qc-text-display mark[data-coding-ids]', function () {
    if (!_qc_mark_pending) return;
    _qc_mark_pending = false;
    var sel = window.getSelection();
    if (sel && !sel.isCollapsed) return;
    if (!window._qc_ns) return;
    var ids = (this.dataset.codingIds || '').split(',').map(Number).filter(Boolean);
    if (!ids.length) return;
    Shiny.setInputValue(
      window._qc_ns + 'clicked_coding',
      { coding_ids: ids, ts: Date.now() },
      { priority: 'event' }
    );
  });

  // ── Click a line number → start TTS from that line ────────────────────────
  $(document).on('click', '.qc-text-display .qc-line-num', function (e) {
    if (!_tts.supported) return;
    e.preventDefault();
    e.stopPropagation();

    var lineEl = $(this).closest('.qc-line')[0];
    if (!lineEl) return;
    var container = _getTtsContainer();
    if (!container) return;

    var lines    = Array.from(container.querySelectorAll('.qc-line'));
    var lineIdx  = lines.indexOf(lineEl);
    if (lineIdx < 0) return;
    _startTtsFromLine(lineIdx);
  });

  // ── Text selection / click-start capture ──────────────────────────────────
  $(document).on('mouseup', '.qc-text-display', function (e) {
    var sel       = window.getSelection();
    var container = e.currentTarget;
    if (!sel || sel.rangeCount === 0) return;

    if (sel.isCollapsed) {
      // Collapsed click — record TTS start point and show marker
      if (!_tts.supported) return;
      var range = sel.getRangeAt(0);
      _tts.clickOffset  = charOffset(container, range.startContainer, range.startOffset);
      _tts.clickLineIdx = null;
      if (container.classList.contains('qc-line-numbers-on')) {
        var el = range.startContainer.nodeType === Node.TEXT_NODE
          ? range.startContainer.parentNode
          : range.startContainer;
        while (el && el !== container) {
          if (el.classList && el.classList.contains('qc-line')) {
            var lineEls = Array.from(container.querySelectorAll('.qc-line'));
            _tts.clickLineIdx = lineEls.indexOf(el);
            break;
          }
          el = el.parentNode;
        }
      }
      _showClickMark(container, range);
      return;
    }

    // Text selection — clear any click mark and send offsets to Shiny
    _clearClickMark(container);
    var range = sel.getRangeAt(0);
    var start = charOffset(container, range.startContainer, range.startOffset);
    var end   = charOffset(container, range.endContainer,   range.endOffset);
    if (start >= end) return;
    Shiny.setInputValue(
      window._qc_ns + 'selection',
      { start: start + 1, end: end, text: sel.toString() },
      { priority: 'event' }
    );
  });

  // ── TTS toolbar button clicks ──────────────────────────────────────────────
  $(document).on('click', '[data-qc-tts-action]', function (e) {
    e.preventDefault();
    if (!window._qc_ns || (this.id && this.id.indexOf(window._qc_ns) !== 0)) return;
    switch (this.getAttribute('data-qc-tts-action')) {
      case 'playpause':
        if (!_tts.isSpeaking) { _startTts(); }
        else { _toggleTtsPause(); }
        break;
      case 'stop': _stopTts(); break;
    }
  });

  // ── Keyboard shortcuts ─────────────────────────────────────────────────────
  $(document).on('keydown', function (e) {
    var tag = document.activeElement && document.activeElement.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
    if ($('.modal.show').length && $(document.activeElement).closest('.modal').length) return;

    var ns = window._qc_ns;
    if (!ns) return;

    switch (e.key) {
      case 'Enter':
        Shiny.setInputValue(ns + 'hotkey_apply', Date.now(), { priority: 'event' });
        break;
      case 'Escape':
        window.getSelection().removeAllRanges();
        Shiny.setInputValue(ns + 'hotkey_escape', Date.now(), { priority: 'event' });
        break;
      case '/':
        e.preventDefault();
        var $sel = $('#' + CSS.escape(ns + 'sel_code'));
        if ($sel[0] && $sel[0].selectize) { $sel[0].selectize.focus(); }
        else if ($sel[0]) { $sel[0].focus(); }
        break;
      case 'n':
        Shiny.setInputValue(ns + 'hotkey_nav_next', Date.now(), { priority: 'event' });
        break;
      case 'p':
        Shiny.setInputValue(ns + 'hotkey_nav_prev', Date.now(), { priority: 'event' });
        break;
      case 'd':
        Shiny.setInputValue(ns + 'hotkey_nav_disputed', Date.now(), { priority: 'event' });
        break;
      case '?':
        Shiny.setInputValue(ns + 'hotkey_help', Date.now(), { priority: 'event' });
        break;
      case ' ':
        e.preventDefault();
        if (!_tts.isSpeaking) { _startTts(); } else { _toggleTtsPause(); }
        break;
      case 'x':
        _stopTts();
        break;
      case 'b':
        var blindBtn = document.getElementById('btn_blind_mode');
        if (blindBtn) blindBtn.click();
        break;
      case '[':
        e.preventDefault();
        Shiny.setInputValue(ns + 'hotkey_doc_prev', Date.now(), { priority: 'event' });
        break;
      case ']':
        e.preventDefault();
        Shiny.setInputValue(ns + 'hotkey_doc_next', Date.now(), { priority: 'event' });
        break;
      case 'm':
        Shiny.setInputValue(ns + 'hotkey_memo', Date.now(), { priority: 'event' });
        break;
      case 'r':
        Shiny.setInputValue(ns + 'hotkey_remove_last', Date.now(), { priority: 'event' });
        break;
      case 'c':
        Shiny.setInputValue(ns + 'hotkey_cb_toggle', Date.now(), { priority: 'event' });
        break;
      case 'l':
        Shiny.setInputValue(ns + 'hotkey_lines_toggle', Date.now(), { priority: 'event' });
        break;
      default:
        if (/^[1-9]$/.test(e.key)) {
          Shiny.setInputValue(
            ns + 'hotkey_digit',
            { digit: parseInt(e.key, 10), ts: Date.now() },
            { priority: 'event' }
          );
        }
    }
  });

  // ── Global: Alt+1-8 tab switching ─────────────────────────────────────────
  var _altTabMap = {
    '1': 'Documents', '2': 'Coding',  '3': 'Compare',
    '4': 'Codebook',  '5': 'Themes',  '6': 'Query',
    '7': 'Cases',     '8': 'Journal'
  };

  function _navToTab(value) {
    var el = document.querySelector('[data-value="' + CSS.escape(value) + '"]');
    if (el) el.click();
  }

  $(document).on('keydown', function (e) {
    if (!e.altKey || !_altTabMap[e.key]) return;
    var tag = document.activeElement && document.activeElement.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
    e.preventDefault();
    _navToTab(_altTabMap[e.key]);
  });

  // ── Global: Ctrl/Cmd+Enter form submit ────────────────────────────────────
  $(document).on('keydown', 'textarea, input[type="text"]', function (e) {
    if ((!e.ctrlKey && !e.metaKey) || e.key !== 'Enter') return;
    e.preventDefault();
    var id  = this.id;
    var btnId;
    if (id === 'memos-new_memo_content') {
      btnId = 'memos-btn_add_memo';
    } else if (id === 'codebook-code_name') {
      var save = document.getElementById('codebook-btn_save_code');
      btnId = (save && !save.disabled && save.offsetParent !== null)
        ? 'codebook-btn_save_code'
        : 'codebook-btn_add_code';
    } else if (id === 'cases-new_case_name') {
      btnId = 'cases-btn_add_case';
    }
    if (btnId) {
      var btn = document.getElementById(btnId);
      if (btn && !btn.disabled) btn.click();
    }
  });

  // ── charOffset helper ──────────────────────────────────────────────────────
  function charOffset(root, node, offset) {
    var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, null, false);
    var n, count = 0;
    while ((n = walker.nextNode())) {
      if (n === node) return count + offset;
      count += n.length;
    }
    return count;
  }

  // ── Wire up the timestamp jump input ───────────────────────────────────────
  document.addEventListener('keydown', function(e) {
    if (e.target && e.target.id === 'qc_ts_jump' && e.key === 'Enter') {
      e.preventDefault();
      jumpToTime(e.target.value);
    }
  });

  // Clicking any timestamp marker scrolls to it (useful when not in line-numbers mode)
  document.addEventListener('click', function(e) {
    var el = e.target.closest('[data-ts]');
    if (!el) return;
    if (el.classList.contains('qc-ts-marker') || el.classList.contains('qc-line-num')) {
      jumpToTime(el.dataset.ts);
    }
  });

  window.addEventListener('beforeunload', _cancelTts);
  $(document).ready(function () { setTimeout(_syncTtsControls, 0); });
})();
