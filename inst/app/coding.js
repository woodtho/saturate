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
    activeLineIdx: -1  // index into lineMap of the currently highlighted line
  };

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
    return (text || '')
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
    var geom   = _needleGeomForPos(container, absolutePos);
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
    var lang = (document.documentElement.lang || navigator.language || 'en-US').toLowerCase();
    var baseLang = lang.split('-')[0];
    return voices.find(function (v) { return v.lang && v.lang.toLowerCase() === lang; }) ||
           voices.find(function (v) { return v.lang && v.lang.toLowerCase().indexOf(baseLang) === 0; }) ||
           voices[0];
  }

  // ── Controls sync ──────────────────────────────────────────────────────────

  function _setTtsStatus(text, state) {
    if (!window._qc_ns) return;
    var el = document.getElementById(window._qc_ns + 'tts_status');
    if (!el) return;
    el.textContent = text;
    el.setAttribute('data-state', state || 'idle');
  }

  function _syncTtsControls() {
    if (!window._qc_ns) return;
    var playBtn  = document.getElementById(window._qc_ns + 'tts_play');
    var pauseBtn = document.getElementById(window._qc_ns + 'tts_pause');
    var stopBtn  = document.getElementById(window._qc_ns + 'tts_stop');
    var container = _getTtsContainer();
    var hasText = !!_getDocumentSpeechText(container);

    if (playBtn)  playBtn.disabled  = !_tts.supported || !hasText;
    if (pauseBtn) {
      pauseBtn.disabled   = !_tts.supported || !_tts.isSpeaking;
      pauseBtn.textContent = _tts.isPaused ? 'Resume' : '⏸';
      pauseBtn.title       = _tts.isPaused ? 'Resume the current narration' : 'Pause the current narration';
      pauseBtn.setAttribute('aria-label', _tts.isPaused ? 'Resume narration' : 'Pause narration');
    }
    if (stopBtn)  stopBtn.disabled  = !_tts.supported || !_tts.isSpeaking;

    if (!_tts.supported) { _setTtsStatus('Unavailable', 'error'); return; }

    if (_tts.isSpeaking) {
      if (_tts.isPaused) {
        _setTtsStatus('Paused', 'paused');
      } else {
        var label = _tts.currentMode === 'selection' ? 'Reading selection' : 'Reading';
        if (_tts.activeLineIdx >= 0) {
          label += ' · line ' + (_tts.activeLineIdx + 1);
        }
        _setTtsStatus(label, 'active');
      }
      return;
    }
    _setTtsStatus(hasText ? 'Ready' : 'No document', 'idle');
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
    utterance.rate  = 1;
    utterance.pitch = 1;

    // Reading cursor via boundary events (not supported in all browsers — degrades gracefully)
    var chunkOffset = _tts.chunkOffsets[index] || 0;
    utterance.onboundary = function (event) {
      if (runId !== _tts.runId) return;
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
      _setTtsStatus('TTS error', 'error');
    };
    _tts.currentUtterance = utterance;
    window.speechSynthesis.speak(utterance);
  }

  function _launchTts(text, mode, lineMap) {
    _cancelTts();
    var container = _getTtsContainer();

    _tts.isSpeaking          = true;
    _tts.isPaused            = false;
    _tts.currentMode         = mode;
    _tts.currentText         = text;
    _tts.currentDocumentText = _getDocumentSpeechText(container);
    _tts.lineMap             = lineMap || [];
    _tts.activeLineIdx       = -1;

    var split = _splitSpeechTextWithOffsets(text);
    _tts.chunkOffsets = split.offsets;

    var runId  = _tts.runId;
    var chunks = split.chunks;
    _syncTtsControls();
    window.setTimeout(function () { _speakChunk(runId, chunks, 0); }, 0);
  }

  // Play button: read selection (if any) or the full document
  function _startTts() {
    if (!_tts.supported) { _syncTtsControls(); return; }
    var container   = _getTtsContainer();
    var selectedText = container ? _getSelectedSpeechText(container) : '';
    var docText      = _getDocumentSpeechText(container);
    var text = selectedText || docText;
    if (!text) { _syncTtsControls(); return; }

    var lineMap = [];
    if (!selectedText && container && container.classList.contains('qc-line-numbers-on')) {
      lineMap = _buildLineMap(container, 0);
    }
    _launchTts(text, selectedText ? 'selection' : 'document', lineMap);
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
      setTimeout(_handleTtsOutputUpdate, 75);
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

  // ── Text selection offset capture ──────────────────────────────────────────
  $(document).on('mouseup', '.qc-text-display', function (e) {
    var sel = window.getSelection();
    if (!sel || sel.isCollapsed || sel.rangeCount === 0) return;
    var range     = sel.getRangeAt(0);
    var container = e.currentTarget;
    var start     = charOffset(container, range.startContainer, range.startOffset);
    var end       = charOffset(container, range.endContainer,   range.endOffset);
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
      case 'play':  _startTts();       break;
      case 'pause': _toggleTtsPause(); break;
      case 'stop':  _stopTts();        break;
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

  window.addEventListener('beforeunload', _cancelTts);
  $(document).ready(function () { setTimeout(_syncTtsControls, 0); });
})();
