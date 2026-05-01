/* saturate — browser audio recording */
(function () {
  'use strict';

  var _rec = {
    mediaRecorder: null,
    chunks:        [],
    timerInterval: null,
    startTime:     null,   // Date.now() at start of current segment
    elapsedMs:     0,      // accumulated ms from completed (pre-pause) segments
    animFrame:     null,
    analyser:      null,   // kept so waveform can restart after resume
    blob:          null,
    blobExt:       '.webm'
  };

  /* ---- helpers -------------------------------------------------------------- */

  function totalElapsed() {
    return _rec.elapsedMs + (Date.now() - (_rec.startTime || Date.now()));
  }

  function formatTime(ms) {
    var s = Math.floor(ms / 1000);
    return Math.floor(s / 60) + ':' + ('0' + (s % 60)).slice(-2);
  }

  function preferredMimeType() {
    if (typeof MediaRecorder === 'undefined') return '';
    var order = [
      'audio/webm;codecs=opus', 'audio/webm',
      'audio/ogg;codecs=opus',  'audio/mp4'
    ];
    for (var i = 0; i < order.length; i++) {
      if (MediaRecorder.isTypeSupported(order[i])) return order[i];
    }
    return '';
  }

  function extFromMime(mime) {
    if (mime.indexOf('ogg') !== -1) return '.ogg';
    if (mime.indexOf('mp4') !== -1) return '.mp4';
    return '.webm';
  }

  function syncCanvasSize(canvas) {
    var W = canvas.offsetWidth  || (canvas.parentElement ? canvas.parentElement.offsetWidth : 0) || 400;
    var H = canvas.offsetHeight || 48;
    if (canvas.width  !== W) canvas.width  = W;
    if (canvas.height !== H) canvas.height = H;
    return { W: W, H: H };
  }

  function drawWaveform(canvas, analyser) {
    var ctx    = canvas.getContext('2d');
    var bufLen = analyser.frequencyBinCount;
    var data   = new Uint8Array(bufLen);

    function frame() {
      if (!_rec.mediaRecorder || _rec.mediaRecorder.state !== 'recording') return;
      _rec.animFrame = requestAnimationFrame(frame);
      analyser.getByteTimeDomainData(data);
      var dim = syncCanvasSize(canvas);
      var W = dim.W, H = dim.H;
      ctx.clearRect(0, 0, W, H);
      ctx.lineWidth   = 2;
      ctx.strokeStyle = '#dc3545';
      ctx.beginPath();
      var step = W / bufLen, x = 0;
      for (var i = 0; i < bufLen; i++) {
        var y = (data[i] / 255) * H;
        if (i === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
        x += step;
      }
      ctx.lineTo(W, H / 2);
      ctx.stroke();
    }
    _rec.animFrame = requestAnimationFrame(frame);
  }

  function drawIdleLine(canvas, color) {
    if (!canvas) return;
    var ctx = canvas.getContext('2d');
    var dim = syncCanvasSize(canvas);
    ctx.clearRect(0, 0, dim.W, dim.H);
    ctx.lineWidth   = 1;
    ctx.strokeStyle = color || '#adb5bd';
    ctx.beginPath();
    ctx.moveTo(0, dim.H / 2);
    ctx.lineTo(dim.W, dim.H / 2);
    ctx.stroke();
  }

  function stopWaveform(canvas) {
    if (_rec.animFrame) { cancelAnimationFrame(_rec.animFrame); _rec.animFrame = null; }
    drawIdleLine(canvas, '#adb5bd');
  }

  function pauseWaveform(canvas) {
    if (_rec.animFrame) { cancelAnimationFrame(_rec.animFrame); _rec.animFrame = null; }
    drawIdleLine(canvas, '#fd7e14'); /* orange "paused" midline */
  }

  /* ---- init message: reset display when modal opens ------------------------- */

  Shiny.addCustomMessageHandler('qc_rec_init', function (msg) {
    var ns        = msg.ns || '';
    var container = document.querySelector('[data-ns="' + ns + '"]');
    if (!container) return;
    var timerEl   = container.querySelector('.qc-rec-timer');
    var statusEl  = container.querySelector('.qc-rec-status');
    var canvas    = container.querySelector('.qc-rec-waveform');
    var startBtn  = container.querySelector('.qc-rec-start');
    var pauseBtn  = container.querySelector('.qc-rec-pause');
    var stopBtn   = container.querySelector('.qc-rec-stop');
    if (timerEl)  timerEl.textContent  = '0:00';
    if (statusEl) statusEl.textContent = '';
    if (startBtn) { startBtn.disabled = false; startBtn.classList.remove('qc-rec-active'); }
    if (pauseBtn) { pauseBtn.disabled = true; _setPauseLabel(pauseBtn, 'pause'); }
    if (stopBtn)  stopBtn.disabled = true;
    stopWaveform(canvas);
    _rec.elapsedMs = 0;
  });

  /* ---- pause button label helper ------------------------------------------- */

  function _setPauseLabel(btn, state) {
    var pauseLabel  = btn.querySelector('.qc-rec-pause-label');
    var resumeLabel = btn.querySelector('.qc-rec-resume-label');
    if (state === 'pause') {
      if (pauseLabel)  pauseLabel.style.display  = '';
      if (resumeLabel) resumeLabel.style.display = 'none';
    } else {
      if (pauseLabel)  pauseLabel.style.display  = 'none';
      if (resumeLabel) resumeLabel.style.display = '';
    }
  }

  /* ---- Record button -------------------------------------------------------- */

  $(document).on('click', '.qc-rec-start', function () {
    var container = $(this).closest('[data-ns]')[0];
    if (!container) return;
    var ns        = container.getAttribute('data-ns') || '';
    var pauseBtn  = container.querySelector('.qc-rec-pause');
    var stopBtn   = container.querySelector('.qc-rec-stop');
    var timerEl   = container.querySelector('.qc-rec-timer');
    var statusEl  = container.querySelector('.qc-rec-status');
    var canvas    = container.querySelector('.qc-rec-waveform');
    var startBtn  = this;

    if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {
      if (statusEl) statusEl.textContent = 'Microphone API not available in this browser.';
      return;
    }

    navigator.mediaDevices.getUserMedia({ audio: true, video: false })
      .then(function (stream) {
        _rec.chunks    = [];
        _rec.blob      = null;
        _rec.elapsedMs = 0;
        var mimeType   = preferredMimeType();
        _rec.blobExt   = extFromMime(mimeType);
        _rec.mediaRecorder = new MediaRecorder(
          stream,
          mimeType ? { mimeType: mimeType } : {}
        );

        _rec.mediaRecorder.addEventListener('dataavailable', function (e) {
          if (e.data && e.data.size > 0) _rec.chunks.push(e.data);
        });

        _rec.mediaRecorder.addEventListener('stop', function () {
          clearInterval(_rec.timerInterval);
          stream.getTracks().forEach(function (t) { t.stop(); });

          startBtn.disabled = false;
          startBtn.classList.remove('qc-rec-active');
          if (pauseBtn) { pauseBtn.disabled = true; _setPauseLabel(pauseBtn, 'pause'); }
          if (stopBtn)  stopBtn.disabled = true;

          var finalMs = _rec.elapsedMs;
          _rec.blob   = new Blob(_rec.chunks, {
            type: _rec.mediaRecorder.mimeType || 'audio/webm'
          });
          var reader = new FileReader();
          reader.onloadend = function () {
            Shiny.setInputValue(ns + 'audio_dataurl', reader.result, { priority: 'event' });
            if (statusEl) statusEl.textContent = '✓ Ready — ' + formatTime(finalMs);
            if (timerEl)  timerEl.textContent  = '0:00';
          };
          reader.readAsDataURL(_rec.blob);
          stopWaveform(canvas);
        });

        /* Start recording BEFORE setting up waveform (loop checks state). */
        _rec.mediaRecorder.start(100);
        _rec.startTime    = Date.now();
        _rec.timerInterval = setInterval(function () {
          if (timerEl) timerEl.textContent = formatTime(totalElapsed());
        }, 200);

        startBtn.disabled = true;
        startBtn.classList.add('qc-rec-active');
        if (pauseBtn) { pauseBtn.disabled = false; _setPauseLabel(pauseBtn, 'pause'); }
        if (stopBtn)  stopBtn.disabled = false;
        if (statusEl) statusEl.textContent = '';

        /* Waveform */
        var AudioCtx = window.AudioContext || window.webkitAudioContext;
        if (AudioCtx && canvas) {
          try {
            var audioCtx = new AudioCtx();
            var src      = audioCtx.createMediaStreamSource(stream);
            var analyser = audioCtx.createAnalyser();
            analyser.fftSize = 256;
            src.connect(analyser);
            _rec.analyser = analyser;
            syncCanvasSize(canvas);
            drawWaveform(canvas, analyser);
          } catch (_e) { _rec.analyser = null; }
        }
      })
      .catch(function (err) {
        if (statusEl) statusEl.textContent = 'Microphone access denied: ' + err.message;
      });
  });

  /* ---- Pause / Resume button ------------------------------------------------ */

  $(document).on('click', '.qc-rec-pause', function () {
    if (!_rec.mediaRecorder) return;
    var container = $(this).closest('[data-ns]')[0];
    var canvas    = container ? container.querySelector('.qc-rec-waveform')  : null;
    var timerEl   = container ? container.querySelector('.qc-rec-timer')     : null;
    var statusEl  = container ? container.querySelector('.qc-rec-status')    : null;
    var startBtn  = container ? container.querySelector('.qc-rec-start')     : null;
    var btn       = this;

    if (_rec.mediaRecorder.state === 'recording') {
      /* --- pause --- */
      _rec.elapsedMs += Date.now() - _rec.startTime;
      clearInterval(_rec.timerInterval);
      _rec.mediaRecorder.pause();
      pauseWaveform(canvas);
      if (startBtn) startBtn.classList.remove('qc-rec-active');
      _setPauseLabel(btn, 'resume');
      if (statusEl) statusEl.textContent = 'Paused';

    } else if (_rec.mediaRecorder.state === 'paused') {
      /* --- resume --- */
      _rec.startTime = Date.now();
      _rec.timerInterval = setInterval(function () {
        if (timerEl) timerEl.textContent = formatTime(totalElapsed());
      }, 200);
      _rec.mediaRecorder.resume();
      if (_rec.analyser && canvas) drawWaveform(canvas, _rec.analyser);
      if (startBtn) startBtn.classList.add('qc-rec-active');
      _setPauseLabel(btn, 'pause');
      if (statusEl) statusEl.textContent = '';
    }
  });

  /* ---- Stop button ---------------------------------------------------------- */

  $(document).on('click', '.qc-rec-stop', function () {
    if (_rec.mediaRecorder &&
        (_rec.mediaRecorder.state === 'recording' ||
         _rec.mediaRecorder.state === 'paused')) {
      /* Accumulate any remaining segment before stop fires. */
      if (_rec.mediaRecorder.state === 'recording') {
        _rec.elapsedMs += Date.now() - _rec.startTime;
      }
      _rec.mediaRecorder.stop();
    }
    this.disabled = true;
  });

  /* ---- Stop recording when modal is dismissed ------------------------------- */

  $(document).on('hidden.bs.modal', function () {
    if (_rec.mediaRecorder &&
        (_rec.mediaRecorder.state === 'recording' ||
         _rec.mediaRecorder.state === 'paused')) {
      _rec.mediaRecorder.stop();
    }
  });

})();
