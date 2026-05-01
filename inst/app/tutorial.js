/* saturate — guided tutorial
 *
 * Self-contained floating panel, fixed bottom-right.
 * Non-blocking: the user can continue using the app while the panel is open.
 *
 * Step fields:
 *   title   {string}        Header text
 *   body    {string}        HTML body
 *   navTo   {string|null}   data-value of the tab to navigate to (null = stay)
 *   hlSel   {string|null}   CSS selector to highlight (null = highlight the nav item)
 *
 * localStorage key: sat_tour_seen_v1
 * Window export:    window.qcStartTutorial(stepIndex)
 * Shiny handlers:   qc_tutorial_start, qc_tutorial_auto
 */
(function () {
  'use strict';

  var STORAGE_KEY = 'sat_tour_seen_v1';

  var STEPS = [

    // ── Welcome ──────────────────────────────────────────────────────────────
    {
      title:  'Welcome to saturate',
      navTo:  null,
      hlSel:  null,
      body:   'saturate is a qualitative data analysis workspace for coding text, ' +
              'building codebooks, developing themes, and tracking every analytical ' +
              'decision. This tour covers every panel. ' +
              'Use <kbd>←</kbd> <kbd>→</kbd> or the buttons to navigate.'
    },

    // ── Prep: Documents ──────────────────────────────────────────────────────
    {
      title:  'Prep › Documents',
      navTo:  'Documents',
      hlSel:  '[id$="-file_upload"]',
      body:   'Every project starts with data. Import <strong>.txt</strong>, ' +
              '<strong>.docx</strong>, <strong>.pdf</strong>, <strong>.md</strong>, ' +
              'or <strong>.csv</strong> files using the file picker, or click ' +
              '<strong>Paste text</strong> to type or paste content directly. ' +
              'Assign a <em>source type</em> (interview, survey, observation…) ' +
              'to each document — used later for triangulation.'
    },
    {
      title:  'Documents — the list',
      navTo:  null,
      hlSel:  '[id$="-btn_code_doc"]',
      body:   'The table shows word count, coding density, import timestamp, and memo ' +
              'for each document. Select a row then click <strong>Code →</strong> to ' +
              'open it in the Coding panel. <strong>Edit</strong> replaces the content ' +
              'and flags existing codings for review — a full version history is kept. ' +
              'Export the list as CSV, Excel, or JSON.'
    },
    {
      title:  'Documents — Record & Transcribe',
      navTo:  null,
      hlSel:  '[id$="-transcribe-btn_open"]',
      body:   'Click <strong>Record &amp; transcribe…</strong> to open the audio ' +
              'workflow. The <em>Record</em> tab captures audio from your microphone — ' +
              'hit <strong>Record</strong>, <strong>Pause/Resume</strong>, and ' +
              '<strong>Stop</strong>. The <em>Upload audio</em> tab accepts ' +
              '.webm, .mp3, .wav, .m4a, and other formats. ' +
              'Once you have audio, click <strong>Transcribe</strong> — ' +
              'a local <a href="https://github.com/bnosac/whisper" target="_blank" rel="noopener">whisper</a> ' +
              'model processes it on your machine (no data leaves). ' +
              'With <strong>Timestamps</strong> ticked (the default), each segment ' +
              'gets a <code>[HH:MM:SS]</code> marker. Edit the transcript, then ' +
              'click <strong>Import transcript</strong> to save it as a document, ' +
              'or download it as <strong>.txt</strong> or <strong>.docx</strong>.'
    },

    // ── Prep: Codebook ───────────────────────────────────────────────────────
    {
      title:  'Prep › Codebook',
      navTo:  'Codebook',
      hlSel:  '[id$="-code_name"]',
      body:   'Create codes with a name, colour, <em>definition</em> (what the code ' +
              'means), and <em>criteria</em> (inclusion and exclusion rules). ' +
              'Optional weights assign direction or intensity when that is meaningful. ' +
              'Codes can also be <strong>merged</strong> (combine two codes and transfer ' +
              'all codings) or <strong>split</strong> (divide one code\'s passages ' +
              'across two new codes).'
    },
    {
      title:  'Codebook — categories',
      navTo:  null,
      hlSel:  '[id$="-btn_add_cat"]',
      body:   'Group codes into <strong>categories</strong> to enable bulk filtering ' +
              'in Coding, Query, and Themes. Categories are lateral — a code can ' +
              'belong to more than one. Use the <strong>Assign</strong> panel to link ' +
              'a code to a category. <strong>Import</strong> loads a codebook from ' +
              'CSV or JSON — useful when sharing a framework across projects.'
    },
    {
      title:  'Codebook — validation and history',
      navTo:  null,
      hlSel:  '[id$="-btn_validate"]',
      body:   '<strong>Validate</strong> checks every code for duplicate names and ' +
              'missing definitions. <strong>History</strong> (per selected code) ' +
              'records every name, definition, colour, and criteria change with coder ' +
              'and timestamp — the full audit trail for your codebook decisions. ' +
              '<strong>Snapshots</strong> save a point-in-time codebook version you ' +
              'can diff against a later snapshot to see what changed.'
    },

    // ── Prep: Cases ──────────────────────────────────────────────────────────
    {
      title:  'Prep › Cases',
      navTo:  'Cases',
      hlSel:  '[id$="-btn_add_case"]',
      body:   'Cases represent participants, sites, or any other unit of analysis. ' +
              'Give each case a name and optional memo, then link source documents to ' +
              'it in the <strong>Documents</strong> sub-tab to group evidence by source.'
    },
    {
      title:  'Cases — attributes',
      navTo:  null,
      hlSel:  '[id$="-attr_variable"]',
      body:   'Add structured <strong>attributes</strong> to each case — age, role, ' +
              'location, and so on — in the Attributes sub-tab. Each attribute is a ' +
              'variable/value pair. Attributes become available as grouping variables in ' +
              'the Query <strong>Cross-tab</strong> view, letting you compare how codes ' +
              'distribute across participant subgroups. Export the full attribute table ' +
              'as CSV, Excel, or JSON.'
    },

    // ── Prep: Journal ────────────────────────────────────────────────────────
    {
      title:  'Prep › Journal',
      navTo:  'Journal',
      hlSel:  '[id$="-new_memo_type"]',
      body:   'Reflexive journals and analytical memos are central to rigorous ' +
              'qualitative research. Choose an entry type — <em>Analytical</em>, ' +
              '<em>Reflexivity</em>, <em>Decision</em>, or <em>Methodological</em> — ' +
              'to keep your thinking organised. Entries support <strong>Markdown</strong>. ' +
              'Filter by type or keyword, and export the full journal as Word, HTML, ' +
              'CSV, or plain text.'
    },

    // ── Coding ───────────────────────────────────────────────────────────────
    {
      title:  'Coding — the workspace',
      navTo:  'Coding',
      hlSel:  '[id$="-open_doc"]',
      body:   'The Coding tab is the heart of the app. Choose a document from the ' +
              'dropdown at the top, read it in the left pane, and use the right panel ' +
              'to apply codes. <strong>Highlighted passages</strong> show existing ' +
              'codings — click any highlight to edit or delete it.'
    },
    {
      title:  'Coding — applying codes',
      navTo:  null,
      hlSel:  '[id$="-btn_apply"]',
      body:   'Select text in the document, choose a code from the selector, add optional ' +
              '<em>confidence</em> and a <em>segment memo</em>, then click ' +
              '<strong>Apply Code</strong> or press <kbd>Enter</kbd>. ' +
              'An Undo toast lets you reverse the last coding instantly. ' +
              'Press <kbd>1</kbd>–<kbd>9</kbd> to apply the Nᵗʰ visible ' +
              'code without the mouse. Use <kbd>/</kbd> to focus the code selector.'
    },
    {
      title:  'Coding — navigation',
      navTo:  null,
      hlSel:  '[id$="-btn_nav_next"]',
      body:   '<strong>Next</strong> and <strong>Prev</strong> jump between uncoded ' +
              'segments so nothing is missed. <strong>Disputed</strong> finds draft ' +
              'or contested codings needing resolution. ' +
              'Keyboard: <kbd>n</kbd> / <kbd>p</kbd> / <kbd>d</kbd>. ' +
              'Use <kbd>[</kbd> and <kbd>]</kbd> to step between documents. ' +
              'Press <kbd>?</kbd> for the full shortcut reference.'
    },
    {
      title:  'Coding — blind mode',
      navTo:  null,
      hlSel:  '.qc-blind-toggle',
      body:   '<strong>Blind mode</strong> hides every other coder’s work so ' +
              'you code independently — essential for inter-rater reliability ' +
              'studies. It also locks the coder filter to your own profile. ' +
              'Toggle it with the button in the navbar or press <kbd>b</kbd>.'
    },
    {
      title:  'Coding — read-aloud',
      navTo:  null,
      hlSel:  '[id$="-tts_playpause"]',
      body:   'The <strong>read-aloud (▶)</strong> toolbar below the document ' +
              'speaks the text using your browser’s TTS engine. ' +
              'Click anywhere in the document to start from that point, or select ' +
              'a passage to read only that. Press <kbd>Space</kbd> to play/pause, ' +
              '<kbd>x</kbd> to stop. Voice and speed are set in Settings.'
    },
    {
      title:  'Coding — display filters and excerpts',
      navTo:  null,
      hlSel:  '[id$="-btn_create_excerpt"]',
      body:   'The <strong>Display filters</strong> section limits which highlights ' +
              'you see — filter by category or coder, adjust opacity, or switch ' +
              'to colour-blind-safe border mode. These never change saved codings. ' +
              '<strong>Create Excerpt</strong> saves a notable passage without a code ' +
              '— useful for vivid quotes or passages to revisit later.'
    },

    // ── Analysis: Compare ────────────────────────────────────────────────────
    {
      title:  'Analysis › Compare — two modes',
      navTo:  'Compare',
      hlSel:  '[id$="-mode"]',
      body:   'Compare has two modes. <strong>Two documents</strong> places any two ' +
              'sources side-by-side to see how coding patterns differ. ' +
              '<strong>Two coders on the same document</strong> shows agreements, ' +
              'unique codings, and conflicts. Tick <em>Sync scroll</em> to keep the ' +
              'panes aligned while reading.'
    },
    {
      title:  'Compare — differences and reliability',
      navTo:  null,
      hlSel:  '[id$="-tbl_diff"]',
      body:   'The <strong>Differences</strong> table below the text panes summarises ' +
              'code-level or segment-level mismatches. In coder-comparison mode, ' +
              'Krippendorff’s α is calculated automatically for the selected ' +
              'codes — a standard measure of inter-rater agreement. ' +
              'Export the difference table as CSV, Excel, or JSON.'
    },

    // ── Analysis: Themes ─────────────────────────────────────────────────────
    {
      title:  'Analysis › Themes',
      navTo:  'Themes',
      hlSel:  '[id$="-btn_new_theme"]',
      body:   'Themes are <em>analytical claims</em>, not just topic labels. ' +
              'Create a theme and write an <strong>analytical statement</strong> ' +
              '(your interpretive argument). Link it to the codes or categories ' +
              'that support it — supporting passages are collected automatically. ' +
              'Export a theme as Word, HTML, or plain text. Themes are also included ' +
              'in the full Analytical Report export.'
    },

    // ── Analysis: Query ──────────────────────────────────────────────────────
    {
      title:  'Analysis › Query — filters',
      navTo:  'Query',
      hlSel:  '[id$="-btn_run"]',
      body:   'Query is the analytical engine. Build a filter with ' +
              '<strong>OR codes</strong> (any match), <strong>AND</strong> constraints ' +
              '(must also have), and <strong>NOT</strong> exclusions. Narrow further ' +
              'by document, case, category, and coder. ' +
              'Click <strong>Run Query</strong> to see matching passages in the ' +
              'Segments tab, then download as CSV.'
    },
    {
      title:  'Query — full-text search',
      navTo:  'Search',
      hlSel:  '[id$="-search_pattern"]',
      body:   'The <strong>Search</strong> tab scans raw document text — ' +
              'not only coded passages. Supports plain text and regular expressions ' +
              'with optional case folding. Useful for finding patterns or terms you ' +
              'haven’t coded yet. Results show document name, match position, ' +
              'and surrounding context. Export as CSV.'
    },
    {
      title:  'Query — co-occurrence',
      navTo:  'Co-occurrence',
      hlSel:  '[id$="-btn_cooc"]',
      body:   '<strong>Co-occurrence</strong> counts how often two codes appear in the ' +
              'same document or overlapping segment, shown as a heatmap and table. ' +
              'Choose the unit (Document or Segment) then click <strong>Compute</strong>. ' +
              'Codes that consistently cluster together likely belong to the same theme. ' +
              'Download the heatmap as PNG or the table as CSV.'
    },
    {
      title:  'Query — saturation',
      navTo:  'Saturation',
      hlSel:  '[id$="-btn_saturation"]',
      body:   '<strong>Saturation</strong> plots cumulative distinct codes per document ' +
              'ordered by import date or first coding date. A flattening curve suggests ' +
              'no new themes are emerging — the standard indicator of theoretical ' +
              'saturation. Click <strong>Compute</strong>, then download as PNG or CSV.'
    },
    {
      title:  'Query — triangulation',
      navTo:  'Triangulation',
      hlSel:  '[id$="-btn_triangulate"]',
      body:   '<strong>Triangulation</strong> compares code presence across document ' +
              'source types — interview, survey, observation, and so on. Set the ' +
              'source type when importing each document. Click <strong>Compute</strong> ' +
              'to see which codes appear consistently across methods and which are ' +
              'method-specific. Export as CSV.'
    },
    {
      title:  'Query — cross-tab',
      navTo:  'Cross-tab',
      hlSel:  '[id$="-btn_xtab"]',
      body:   '<strong>Cross-tab</strong> breaks code frequency down by a case attribute ' +
              'variable — age group, role, location, and so on. Select the attribute, ' +
              'click <strong>Compute</strong>. Requires cases with that attribute set. ' +
              'Useful for comparing how themes distribute across participant subgroups. ' +
              'Export the table as CSV.'
    },
    {
      title:  'Query — network graph',
      navTo:  'Graph',
      hlSel:  '[id$="-btn_draw"]',
      body:   'The <strong>Graph</strong> tab renders three network types: ' +
              '<em>document similarity</em> (shared codes), ' +
              '<em>bipartite</em> (documents and codes together), and ' +
              '<em>code co-occurrence</em>. Node size reflects coding volume. ' +
              'Adjust the minimum-shared threshold to reduce noise. ' +
              'Requires the <code>visNetwork</code> package.'
    },
    {
      title:  'Query — word cloud',
      navTo:  'Word Cloud',
      hlSel:  '[id$="-btn_wordcloud"]',
      body:   '<strong>Word Cloud</strong> shows all codes sized by coding count, ' +
              'or shows the most frequent words in passages under a single selected ' +
              'code — a quick lexical summary of what that code actually captures. ' +
              'Download as PNG. Requires the <code>wordcloud</code> or ' +
              '<code>wordcloud2</code> package.'
    },

    // ── Review: Member Checks ────────────────────────────────────────────────
    {
      title:  'Review › Member Checks',
      navTo:  'Member Checks',
      hlSel:  '[id$="-btn_new_check"]',
      body:   'Create a member check for one document, optionally restricting it to ' +
              'selected codes. Export as <strong>HTML</strong>, <strong>Word</strong>, ' +
              'or plain text for participant review. Record each item as ' +
              '<em>Confirmed</em>, <em>Disputed</em>, or <em>Other</em>. Use ' +
              '<strong>Confirm all</strong> or <strong>Dispute all</strong> for bulk ' +
              'responses. Participant responses become part of the evidence trail.'
    },

    // ── Review: Audit ────────────────────────────────────────────────────────
    {
      title:  'Review › Audit',
      navTo:  'Audit',
      hlSel:  '[id$="-tbl_audit"]',
      body:   'The audit trail records every code creation, edit, deletion, and ' +
              'reassignment with coder name and timestamp. Filter by event type ' +
              '(Coding or Code), operation (create / update / delete / reassign), ' +
              'document, and date range. Export as CSV, Excel, or JSON for ' +
              'methodological appendices or reviewer scrutiny.'
    },

    // ── Review: Export ───────────────────────────────────────────────────────
    {
      title:  'Review › Export',
      navTo:  'Export',
      hlSel:  '[id$="-export_type"]',
      body:   'Three export types. <strong>Analytical Report</strong>: narrative with ' +
              'selected themes, supporting excerpts, and code definitions as Word or HTML. ' +
              '<strong>Codebook</strong>: definitions, criteria, and example passages ' +
              'for methods appendices, with options to include or exclude each section. ' +
              '<strong>Raw Project Data</strong>: any individual table (documents, ' +
              'codes, codings, cases, themes…) as CSV, Excel, or JSON.'
    },

    // ── Settings and Project ─────────────────────────────────────────────────
    {
      title:  'Settings and project tools',
      navTo:  null,
      hlSel:  '#btn_settings',
      body:   '<strong>Settings</strong> manages profiles (coder names), display ' +
              'preferences (colour theme, fonts, text size, line spacing), and ' +
              'read-aloud voice and speed — all persisted in this browser per profile. ' +
              '<strong>Project</strong> shows the file path and lets you ' +
              '<em>Split</em> (create a contributor copy for independent coding) or ' +
              '<em>Merge</em> (bring a contributor’s codings back in).'
    },

    // ── Done ─────────────────────────────────────────────────────────────────
    {
      title:  'You’re ready',
      navTo:  'Help',
      hlSel:  null,
      last:   true,
      body:   'Visit <strong>Help</strong> any time for keyboard shortcuts, ' +
              'detailed workflow guides, and to restart this tour. ' +
              'For a full walkthrough with worked examples, read the ' +
              '<a href="https://thomaswood.github.io/saturate/articles/user-guide.html" ' +
              'target="_blank" rel="noopener">user guide</a>. ' +
              'Good luck with your research.'
    }
  ];

  // ── State ──────────────────────────────────────────────────────────────────
  var _panel         = null;
  var _currentStep   = 0;
  var _highlightedEl = null;
  var _keydownBound  = null;
  var _hlTimer       = null;

  // ── Target resolution ──────────────────────────────────────────────────────
  function _resolveHighlightEl(step) {
    // Explicit CSS selector takes priority
    if (step.hlSel) {
      return document.querySelector(step.hlSel);
    }
    // Fall back to the nav item for the target tab
    if (!step.navTo) return null;
    var el = document.querySelector('[data-value="' + CSS.escape(step.navTo) + '"]');
    if (!el) return null;
    var item = el.closest('.nav-item.dropdown');
    if (item) {
      var toggle = item.querySelector('.dropdown-toggle');
      if (toggle) return toggle;
    }
    return el;
  }

  function _navigateTo(val) {
    if (!val) return;
    var el = document.querySelector('[data-value="' + CSS.escape(val) + '"]');
    if (el) el.click();
  }

  // ── Highlight ──────────────────────────────────────────────────────────────
  function _clearHighlight() {
    if (_hlTimer) { clearTimeout(_hlTimer); _hlTimer = null; }
    if (_highlightedEl) {
      _highlightedEl.classList.remove('qc-tour-highlight');
      _highlightedEl = null;
    }
  }

  // Move the panel to whichever corner doesn't overlap the highlighted element.
  function _repositionPanel(el) {
    if (!_panel) return;
    if (!el) {
      _panel.style.top    = '';
      _panel.style.right  = '';
      _panel.style.bottom = '';
      _panel.style.left   = '';
      return;
    }

    var er = el.getBoundingClientRect();
    var pw = _panel.offsetWidth;
    var ph = _panel.offsetHeight;
    var vw = window.innerWidth;
    var vh = window.innerHeight;
    var m  = 20; // ~1.25rem, matches the CSS default

    // Four candidate corners: [top, right, bottom, left] — null means unset (auto).
    var candidates = [
      { bottom: m, right: m,  top: null, left: null  },  // bottom-right (default)
      { bottom: m, left:  m,  top: null, right: null },  // bottom-left
      { top:    m, right: m,  bottom: null, left: null }, // top-right
      { top:    m, left:  m,  bottom: null, right: null } // top-left
    ];

    function panelRect(c) {
      var l = c.left   != null ? c.left   : vw - pw - c.right;
      var t = c.top    != null ? c.top    : vh - ph - c.bottom;
      return { left: l, top: t, right: l + pw, bottom: t + ph };
    }

    function overlaps(a, b) {
      return a.left < b.right && a.right > b.left &&
             a.top  < b.bottom && a.bottom > b.top;
    }

    var chosen = candidates[0];
    for (var i = 0; i < candidates.length; i++) {
      if (!overlaps(panelRect(candidates[i]), er)) {
        chosen = candidates[i];
        break;
      }
    }

    _panel.style.top    = chosen.top    != null ? chosen.top    + 'px' : '';
    _panel.style.right  = chosen.right  != null ? chosen.right  + 'px' : '';
    _panel.style.bottom = chosen.bottom != null ? chosen.bottom + 'px' : '';
    _panel.style.left   = chosen.left   != null ? chosen.left   + 'px' : '';
  }

  // Scroll el into view only if it is off-screen. Uses instant so that
  // _repositionPanel can immediately read stable getBoundingClientRect coords.
  function _scrollIntoView(el) {
    var r  = el.getBoundingClientRect();
    var vh = window.innerHeight;
    var vw = window.innerWidth;
    if (r.top >= 0 && r.bottom <= vh && r.left >= 0 && r.right <= vw) return;
    el.scrollIntoView({ behavior: 'instant', block: 'center', inline: 'nearest' });
  }

  function _applyHighlight(step) {
    _clearHighlight();
    // Delay when we just switched tabs so the panel transition completes first
    var delay = step.navTo ? 220 : 0;
    _hlTimer = setTimeout(function () {
      _hlTimer = null;
      var el = _resolveHighlightEl(step);
      if (el) {
        el.classList.add('qc-tour-highlight');
        _highlightedEl = el;
        _scrollIntoView(el);   // instant — must happen before reposition
        _repositionPanel(el);  // reads settled coords
      } else {
        _repositionPanel(null);
      }
    }, delay);
  }

  // ── Panel DOM ──────────────────────────────────────────────────────────────
  function _buildPanel() {
    var panel = document.createElement('div');
    panel.id        = 'qc-tour-panel';
    panel.className = 'qc-tour-panel';
    panel.setAttribute('role', 'complementary');
    panel.setAttribute('aria-label', 'Guided tutorial');

    panel.innerHTML = [
      '<div class="qc-tour-header">',
        '<span class="qc-tour-counter"></span>',
        '<button class="qc-tour-close" aria-label="Close tutorial">&times;</button>',
      '</div>',
      '<div class="qc-tour-progress"></div>',
      '<h6 class="qc-tour-title"></h6>',
      '<div class="qc-tour-body"></div>',
      '<div class="qc-tour-footer">',
        '<button class="qc-tour-btn qc-tour-prev">&#8592; Back</button>',
        '<button class="qc-tour-btn qc-tour-btn--primary qc-tour-next">Next &#8594;</button>',
        '<button class="qc-tour-btn qc-tour-btn--success qc-tour-finish"',
          ' style="display:none">Finish &#10003;</button>',
      '</div>',
      '<label class="qc-tour-skip-label">',
        '<input class="qc-tour-skip-check" type="checkbox"> Don’t show again',
      '</label>'
    ].join('');

    document.body.appendChild(panel);

    panel.querySelector('.qc-tour-close').addEventListener('click', _close);
    panel.querySelector('.qc-tour-prev').addEventListener('click', _prev);
    panel.querySelector('.qc-tour-next').addEventListener('click', _next);
    panel.querySelector('.qc-tour-finish').addEventListener('click', _close);

    return panel;
  }

  // ── Render step ────────────────────────────────────────────────────────────
  function _render() {
    var step   = STEPS[_currentStep];
    var n      = STEPS.length;
    var isLast = _currentStep === n - 1;

    _panel.querySelector('.qc-tour-counter').textContent =
      (_currentStep + 1) + ' of ' + n;

    // Progress dots
    var progress = _panel.querySelector('.qc-tour-progress');
    progress.innerHTML = '';
    for (var i = 0; i < n; i++) {
      var dot = document.createElement('span');
      dot.className = 'qc-tour-dot';
      if (i < _currentStep)   dot.classList.add('qc-tour-dot--done');
      if (i === _currentStep) dot.classList.add('qc-tour-dot--active');
      progress.appendChild(dot);
    }

    _panel.querySelector('.qc-tour-title').textContent = step.title;
    _panel.querySelector('.qc-tour-body').innerHTML    = step.body;

    var prevBtn   = _panel.querySelector('.qc-tour-prev');
    var nextBtn   = _panel.querySelector('.qc-tour-next');
    var finishBtn = _panel.querySelector('.qc-tour-finish');

    prevBtn.disabled        = (_currentStep === 0);
    nextBtn.style.display   = isLast ? 'none' : '';
    finishBtn.style.display = isLast ? ''     : 'none';

    _navigateTo(step.navTo);
    _applyHighlight(step);
  }

  // ── Navigation ────────────────────────────────────────────────────────────
  function _next() {
    if (_currentStep < STEPS.length - 1) { _currentStep++; _render(); }
  }

  function _prev() {
    if (_currentStep > 0) { _currentStep--; _render(); }
  }

  // ── Keyboard ──────────────────────────────────────────────────────────────
  function _onKeydown(e) {
    var tag = document.activeElement && document.activeElement.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
    if (e.key === 'Escape')                               { _close(); return; }
    if (e.key === 'ArrowRight' || e.key === 'ArrowDown')  { _next();  return; }
    if (e.key === 'ArrowLeft'  || e.key === 'ArrowUp')    { _prev();  return; }
  }

  // ── Open / close ──────────────────────────────────────────────────────────
  function _start(stepIndex) {
    _currentStep = (typeof stepIndex === 'number' &&
                    stepIndex >= 0 &&
                    stepIndex < STEPS.length)
      ? stepIndex : 0;

    if (!_panel) _panel = _buildPanel();

    _render();

    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        _panel.classList.add('qc-tour-panel--visible');
      });
    });

    if (_keydownBound) document.removeEventListener('keydown', _keydownBound);
    _keydownBound = _onKeydown;
    document.addEventListener('keydown', _keydownBound);
  }

  function _close() {
    if (!_panel) return;

    var skip = _panel.querySelector('.qc-tour-skip-check');
    if (skip && skip.checked) {
      try { window.localStorage.setItem(STORAGE_KEY, '1'); } catch (e) {}
    }

    _clearHighlight();
    _panel.classList.remove('qc-tour-panel--visible');

    if (_keydownBound) {
      document.removeEventListener('keydown', _keydownBound);
      _keydownBound = null;
    }

    var ref = _panel;
    _panel = null;
    setTimeout(function () {
      if (ref && ref.parentNode) ref.parentNode.removeChild(ref);
    }, 280);
  }

  // ── Shiny message handlers ─────────────────────────────────────────────────
  Shiny.addCustomMessageHandler('qc_tutorial_start', function (msg) {
    _start((msg && typeof msg.step === 'number') ? msg.step : 0);
  });

  Shiny.addCustomMessageHandler('qc_tutorial_auto', function () {
    var seen = false;
    try { seen = !!window.localStorage.getItem(STORAGE_KEY); } catch (e) {}
    if (!seen) _start(0);
  });

  // ── Window export ──────────────────────────────────────────────────────────
  window.qcStartTutorial = _start;

}());
