/* qualcoder — text-selection offset capture
 *
 * Listens for mouseup on any .qc-text-display element and sends the
 * 1-based character offsets of the user's selection to Shiny.
 *
 * The namespace prefix is set per-session by the server sending a
 * 'qc_set_ns' custom message before the panel is rendered.
 */
(function () {
  window._qc_ns = '';

  Shiny.addCustomMessageHandler('qc_set_ns', function (msg) {
    window._qc_ns = msg.ns_prefix;
  });

  $(document).on('mouseup', '.qc-text-display', function (e) {
    var sel = window.getSelection();
    if (!sel || sel.isCollapsed || sel.rangeCount === 0) return;

    var range     = sel.getRangeAt(0);
    var container = e.currentTarget;

    var start = charOffset(container, range.startContainer, range.startOffset);
    var end   = charOffset(container, range.endContainer,   range.endOffset);

    if (start >= end) return;

    Shiny.setInputValue(
      window._qc_ns + 'selection',
      { start: start + 1, end: end, text: sel.toString() },  // +1 → 1-based
      { priority: 'event' }
    );
  });

  /* Walk text nodes from `root` to (`node`, `offset`) and return the
   * cumulative character count — i.e. the character offset of that
   * position relative to the plain text content of `root`. */
  function charOffset(root, node, offset) {
    var walker = document.createTreeWalker(
      root, NodeFilter.SHOW_TEXT, null, false
    );
    var n, count = 0;
    while ((n = walker.nextNode())) {
      if (n === node) return count + offset;
      count += n.length;
    }
    return count;
  }
})();
