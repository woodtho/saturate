/* saturate — coding interaction
 *
 * Handles:
 *  1. Text-selection offset capture (mouseup on .qc-text-display)
 *  2. Keyboard shortcuts for the coding panel
 *  3. Scroll-to-character-position for segment navigation
 *  4. Scroll-sync for the compare panel
 */
(function () {
  'use strict';

  window._qc_ns          = '';
  window._qc_sync_scroll = false;

  // ── Namespace setup ────────────────────────────────────────────────────────
  Shiny.addCustomMessageHandler('qc_set_ns', function (msg) {
    window._qc_ns = msg.ns_prefix;
  });

  // ── Scroll to character position ───────────────────────────────────────────
  // msg = { pos: N }  where N is a 1-based character offset.
  // First looks for a <mark data-selfirst="…"> at or after pos; falls back to
  // proportional scrolling within the container.
  Shiny.addCustomMessageHandler('qc_scroll_to', function (msg) {
    setTimeout(function () {
      var pos       = msg.pos;
      var container = document.querySelector('.qc-text-display');
      if (!container) return;

      // Find the first mark whose selfirst >= pos
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
      // Proportional fallback
      var total = container.textContent.length;
      if (total > 0) {
        var frac = pos / total;
        container.scrollTop =
          Math.floor(frac * (container.scrollHeight - container.clientHeight));
      }
    }, 120);   // brief delay ensures Shiny has flushed DOM updates first
  });

  // ── Compare-panel scroll sync ──────────────────────────────────────────────
  Shiny.addCustomMessageHandler('qc_compare_sync', function (msg) {
    window._qc_sync_scroll = !!msg.enabled;
    if (msg.enabled) _attachSyncListeners();
  });

  var _syncing = false;
  function _onSyncScroll() {
    if (!window._qc_sync_scroll || _syncing) return;
    _syncing = true;
    var top    = this.scrollTop;
    document.querySelectorAll('.qc-compare-panel .qc-text-display').forEach(
      function (p) { p.scrollTop = top; }
    );
    _syncing = false;
  }

  function _attachSyncListeners() {
    document.querySelectorAll('.qc-compare-panel .qc-text-display').forEach(
      function (p) {
        p.removeEventListener('scroll', _onSyncScroll);
        p.addEventListener('scroll', _onSyncScroll);
      }
    );
  }

  // Re-attach whenever Shiny re-renders outputs (covers dynamic UI updates)
  $(document).on('shiny:value', function (e) {
    if (window._qc_sync_scroll &&
        e.name && e.name.indexOf('text') !== -1) {
      setTimeout(_attachSyncListeners, 150);
    }
  });

  // ── Click on an existing coding mark → open edit panel ────────────────────
  // A click (collapsed selection) on a <mark data-coding-ids="…"> sends the
  // coding IDs to Shiny. Distinguished from a drag-select by checking whether
  // the selection is collapsed after mouseup.
  var _qc_mark_pending = false;

  $(document).on('mousedown', '.qc-text-display mark[data-coding-ids]', function () {
    _qc_mark_pending = true;
  });

  $(document).on('mouseup', '.qc-text-display mark[data-coding-ids]', function () {
    if (!_qc_mark_pending) return;
    _qc_mark_pending = false;
    var sel = window.getSelection();
    if (sel && !sel.isCollapsed) return; // drag-select in progress — ignore
    if (!window._qc_ns) return;
    var ids = (this.dataset.codingIds || '')
      .split(',').map(Number).filter(Boolean);
    if (!ids.length) return;
    Shiny.setInputValue(
      window._qc_ns + 'clicked_coding',
      { coding_ids: ids, ts: Date.now() },
      { priority: 'event' }
    );
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
      { start: start + 1, end: end, text: sel.toString() },  // +1 → 1-based
      { priority: 'event' }
    );
  });

  // ── Keyboard shortcuts ─────────────────────────────────────────────────────
  // Suppressed when focus is in a text input, textarea, select, or modal form.
  $(document).on('keydown', function (e) {
    var tag = document.activeElement && document.activeElement.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
    if ($('.modal.show').length &&
        $(document.activeElement).closest('.modal').length) return;

    var ns = window._qc_ns;
    if (!ns) return;

    switch (e.key) {
      case 'Enter':
        Shiny.setInputValue(ns + 'hotkey_apply',
          Date.now(), { priority: 'event' });
        break;

      case 'Escape':
        window.getSelection().removeAllRanges();
        Shiny.setInputValue(ns + 'hotkey_escape',
          Date.now(), { priority: 'event' });
        break;

      case '/':
        e.preventDefault();
        // Try selectize first, then fall back to native focus
        var $sel = $('#' + CSS.escape(ns + 'sel_code'));
        if ($sel[0] && $sel[0].selectize) {
          $sel[0].selectize.focus();
        } else if ($sel[0]) {
          $sel[0].focus();
        }
        break;

      case 'n':
        Shiny.setInputValue(ns + 'hotkey_nav_next',
          Date.now(), { priority: 'event' });
        break;

      case 'p':
        Shiny.setInputValue(ns + 'hotkey_nav_prev',
          Date.now(), { priority: 'event' });
        break;

      case 'd':
        Shiny.setInputValue(ns + 'hotkey_nav_disputed',
          Date.now(), { priority: 'event' });
        break;

      case '?':
        Shiny.setInputValue(ns + 'hotkey_help',
          Date.now(), { priority: 'event' });
        break;

      default:
        // Digits 1–9: select Nth code and apply it to the current selection
        if (/^[1-9]$/.test(e.key)) {
          Shiny.setInputValue(
            ns + 'hotkey_digit',
            { digit: parseInt(e.key, 10), ts: Date.now() },
            { priority: 'event' }
          );
        }
        break;
    }
  });

  // ── charOffset helper ──────────────────────────────────────────────────────
  // Walks text nodes from `root` to (`node`, `offset`) and returns the
  // cumulative character count relative to the plain-text content of `root`.
  function charOffset(root, node, offset) {
    var walker = document.createTreeWalker(
      root, NodeFilter.SHOW_TEXT, null, false);
    var n, count = 0;
    while ((n = walker.nextNode())) {
      if (n === node) return count + offset;
      count += n.length;
    }
    return count;
  }
})();
