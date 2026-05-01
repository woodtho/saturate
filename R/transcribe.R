# Audio transcription helpers - wraps the whisper + av packages.
# Both are in Suggests; availability is checked at call time.

.whisper_available <- function() {
  requireNamespace("whisper", quietly = TRUE) &&
    requireNamespace("av",      quietly = TRUE)
}

# Returns TRUE if the ggml binary for `model` is already on disk.
.whisper_model_cached <- function(model) {
  bin <- file.path(tools::R_user_dir("whisper", "data"),
                   paste0("ggml-", model, ".bin"))
  file.exists(bin)
}

# Decode a data-URL base64 audio string to a temp file.
# Returns the temp file path; caller is responsible for unlink().
.decode_audio_dataurl <- function(data_url) {
  b64       <- sub("^data:[^,]+,", "", data_url)
  raw_bytes <- jsonlite::base64_dec(b64)
  mime      <- regmatches(data_url,
                 regexpr("(?<=data:)[^;]+", data_url, perl = TRUE))
  ext <- switch(mime %||% "",
    "audio/webm" = ".webm", "audio/ogg"  = ".ogg",
    "audio/mp4"  = ".mp4",  "audio/mpeg" = ".mp3",
    "audio/wav"  = ".wav",  ".webm"
  )
  tmp <- tempfile(fileext = ext)
  writeBin(raw_bytes, tmp)
  tmp
}

# Format milliseconds as [HH:MM:SS] for transcript timestamps.
.format_ms <- function(ms) {
  s <- as.integer(ms) %/% 1000L
  sprintf("[%02d:%02d:%02d]", s %/% 3600L, (s %% 3600L) %/% 60L, s %% 60L)
}

# Convert audio to 16 kHz WAV and transcribe with whisper.
# Returns the list from whisper::transcribe() - $text always present.
# .progress is an optional function(message, detail) called at key steps.
.transcribe_audio <- function(path, model = "tiny", language = NULL,
                               .progress = NULL) {
  if (!.whisper_available())
    stop(
      "The 'whisper' and 'av' packages are required for transcription. ",
      "Install with: install.packages(c('whisper', 'av'))"
    )

  if (!is.null(.progress)) {
    if (.whisper_model_cached(model)) {
      .progress("Loading whisper model\u2026", paste0("model: ", model))
    } else {
      .progress("Downloading whisper model\u2026",
                paste0("'", model, "' \u2014 this may take several minutes on first use"))
    }
  }

  if (!is.null(.progress))
    .progress("Converting audio\u2026", "resampling to 16 kHz WAV")

  wav <- tempfile(fileext = ".wav")
  on.exit(unlink(wav), add = TRUE)
  av::av_audio_convert(path, wav, sample_rate = 16000L)

  if (!is.null(.progress))
    .progress("Transcribing\u2026", "this may take a moment for long recordings")

  lang <- if (length(language) == 0L || !nzchar(language %||% "")) NULL else language

  whisper::transcribe(
    wav,
    model      = model,
    language   = lang,
    timestamps = TRUE    # always fetch segments so caller can format them
  )
}
