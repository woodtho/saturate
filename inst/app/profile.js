(function() {
  "use strict";

  var fontStacks = {
    system: "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif",
    serif: "Georgia, 'Times New Roman', serif",
    sans: "Verdana, Geneva, sans-serif",
    mono: "ui-monospace, SFMono-Regular, Consolas, 'Liberation Mono', monospace"
  };

  var defaultSettings = {
    colorTheme: "light",
    uiFont: "system",
    uiScale: 100,
    documentFont: "serif",
    documentScale: 100,
    documentLineHeight: 1.9,
    documentHeight: 68,
    tableDensity: "comfortable",
    reduceMotion: false,
    showLineNumbers: true,
    highlightOpacity: 0.33,
    ttsVoice: "auto",
    ttsRate: 1,
    showTimestamps: true
  };

  var handlerRegistered = false;
  var storageKey = "saturate.profileState.v1";
  var loadedFromLocalStorage = false;

  // In-memory state, persisted in this browser with localStorage.
  var state = {
    profiles:      [],
    activeProfile: "",
    settings:      Object.assign({}, defaultSettings)
  };

  function cleanName(name) {
    return String(name || "").replace(/\s+/g, " ").trim();
  }

  function numberSetting(settings, key, fallback, min, max) {
    var value = Number(settings[key]);
    if (!Number.isFinite(value)) value = fallback;
    return Math.min(max, Math.max(min, value));
  }

  function choiceSetting(settings, key, fallback, choices) {
    var value = String(settings[key] || fallback);
    return choices.indexOf(value) >= 0 ? value : fallback;
  }

  function ttsVoiceSetting(settings) {
    var value = cleanName(String((settings && settings.ttsVoice) || "auto"));
    return value || "auto";
  }

  function listSpeechVoices() {
    if (!window.speechSynthesis || !window.speechSynthesis.getVoices) return [];
    var seen = {};
    return (window.speechSynthesis.getVoices() || [])
      .filter(function(voice) {
        var key = cleanName(voice.voiceURI || voice.name);
        if (!key || seen[key]) return false;
        seen[key] = true;
        return true;
      })
      .map(function(voice) {
        return {
          value: cleanName(voice.voiceURI || voice.name),
          label:
            voice.name +
            (voice.lang ? " (" + voice.lang + ")" : "") +
            (voice.default ? " - browser default" : "")
        };
      })
      .sort(function(a, b) {
        return a.label.localeCompare(b.label);
      });
  }

  function syncTtsVoiceSelect(settings) {
    var select = document.getElementById("settings_tts_voice");
    if (!select) return;

    var note = document.getElementById("settings_tts_voice_note");
    var supported = !!(window.speechSynthesis && window.SpeechSynthesisUtterance);
    var preferred =
      ttsVoiceSetting(settings) ||
      cleanName(select.dataset.preferredVoice) ||
      cleanName(select.value) ||
      ttsVoiceSetting(state.settings);
    var voices = supported ? listSpeechVoices() : [];
    var currentValue = cleanName(select.value);

    select.innerHTML = "";

    var autoOption = document.createElement("option");
    autoOption.value = "auto";
    autoOption.textContent = "System default";
    select.appendChild(autoOption);

    voices.forEach(function(voice) {
      var option = document.createElement("option");
      option.value = voice.value;
      option.textContent = voice.label;
      select.appendChild(option);
    });

    var selected =
      voices.some(function(voice) { return voice.value === currentValue; }) ? currentValue :
      voices.some(function(voice) { return voice.value === preferred; }) ? preferred :
      "auto";

    select.value = selected;
    select.dataset.preferredVoice = selected;
    select.disabled = !supported;

    if (!note) return;
    if (!supported) {
      note.textContent = "Read-aloud voices are not available in this browser.";
    } else if (!voices.length) {
      note.textContent = "Voice list is still loading from this browser.";
    } else {
      note.textContent = "Voice choices come from this browser and device.";
    }
  }

  function applySettings(settings) {
    settings = Object.assign({}, defaultSettings, settings || {});
    var root = document.documentElement;
    var theme = choiceSetting(
      settings,
      "colorTheme",
      "light",
      ["light", "dark", "contrast", "contrast-dark", "ocean", "warm"]
    );
    var density = choiceSetting(
      settings,
      "tableDensity",
      "comfortable",
      ["compact", "comfortable", "roomy"]
    );
    var uiScale = numberSetting(settings, "uiScale", 100, 90, 125);
    var documentScale = numberSetting(settings, "documentScale", 100, 85, 150);
    var documentLineHeight = numberSetting(
      settings,
      "documentLineHeight",
      1.9,
      1.4,
      2.4
    );
    var documentHeight = numberSetting(settings, "documentHeight", 68, 48, 86);
    var ttsRate = numberSetting(settings, "ttsRate", 1, 0.6, 1.8);
    var ttsVoice = ttsVoiceSetting(settings);

    root.setAttribute("data-sat-theme", theme);
    root.setAttribute("data-sat-density", density);
    root.setAttribute("data-sat-tts-voice", ttsVoice);
    root.setAttribute("data-sat-tts-rate", String(ttsRate));
    root.setAttribute(
      "data-sat-motion",
      settings.reduceMotion ? "reduced" : "standard"
    );
    root.style.setProperty("--sat-root-font-size", uiScale + "%");
    root.style.setProperty(
      "--sat-app-font-family",
      fontStacks[settings.uiFont] || fontStacks.system
    );
    root.style.setProperty(
      "--sat-document-font-family",
      fontStacks[settings.documentFont] || fontStacks.serif
    );
    root.style.setProperty(
      "--sat-document-font-size",
      (documentScale / 100).toFixed(2) + "rem"
    );
    root.style.setProperty(
      "--sat-document-line-height",
      String(documentLineHeight)
    );
    root.style.setProperty(
      "--sat-document-height",
      documentHeight + "vh"
    );
    syncTtsVoiceSelect(settings);
  }

  function setCoderInput(name) {
    var display = document.getElementById("current_coder_display");
    if (!display) return;
    display.textContent = cleanName(name) || "default";
    display.setAttribute("title", "Change profile from Settings");
  }

  function normalizeStoredProfile(profile) {
    if (!profile) return null;
    var name = cleanName(profile.name);
    if (!name) return null;
    return {
      name: name,
      createdAt: profile.createdAt || new Date().toISOString(),
      lastUsedAt: profile.lastUsedAt || null,
      settings: Object.assign({}, defaultSettings, profile.settings || {})
    };
  }

  function getLocalStorage() {
    try {
      return window.localStorage || null;
    } catch (error) {
      return null;
    }
  }

  function saveLocalState() {
    var storage = getLocalStorage();
    if (!storage) return;
    try {
      storage.setItem(storageKey, JSON.stringify({
        profiles: state.profiles,
        activeProfile: state.activeProfile,
        settings: state.settings
      }));
    } catch (error) {
      // Private browsing and storage quotas can reject writes.
    }
  }

  function loadLocalState() {
    var storage = getLocalStorage();
    if (!storage) return false;
    try {
      var raw = storage.getItem(storageKey);
      if (!raw) return false;
      var stored = JSON.parse(raw);
      var profiles = Array.isArray(stored.profiles) ?
        stored.profiles.map(normalizeStoredProfile).filter(Boolean) :
        [];
      var active = cleanName(stored.activeProfile || "");
      var activeProfile = active ? profiles.find(function(profile) {
        return profileMatches(profile, active);
      }) : null;

      state.profiles = profiles;
      state.activeProfile = activeProfile ? activeProfile.name : "";
      state.settings = Object.assign(
        {},
        defaultSettings,
        activeProfile ? activeProfile.settings : stored.settings || {}
      );
      loadedFromLocalStorage = profiles.length > 0 || !!state.activeProfile;
      return loadedFromLocalStorage;
    } catch (error) {
      return false;
    }
  }

  function sendState(reason) {
    saveLocalState();
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    var s = {
      profiles:      state.profiles,
      activeProfile: state.activeProfile,
      settings:      state.settings,
      reason:        reason || "sync",
      nonce:         Date.now()
    };
    window.Shiny.setInputValue("profile_state", s, { priority: "event" });
    if (state.activeProfile) {
      window.Shiny.setInputValue(
        "profile_selected",
        { name: state.activeProfile, nonce: Date.now() },
        { priority: "event" }
      );
    }
  }

  function profileMatches(profile, name) {
    return profile && cleanName(profile.name).toLowerCase() === name.toLowerCase();
  }

  function findProfile(name) {
    name = cleanName(name);
    if (!name) return null;
    for (var i = 0; i < state.profiles.length; i += 1) {
      if (profileMatches(state.profiles[i], name)) return state.profiles[i];
    }
    return null;
  }

  function applyProfileSettings(profile) {
    var settings = Object.assign({}, defaultSettings, (profile && profile.settings) || {});
    state.settings = settings;
    applySettings(settings);
  }

  function updateActiveProfileSettings(settings) {
    var profile = findProfile(state.activeProfile);
    if (profile) profile.settings = Object.assign({}, settings || state.settings);
  }

  function hideGate() {
    var gate = document.getElementById("qc-profile-gate");
    if (gate) gate.classList.add("qc-profile-gate-hidden");
    document.body.classList.remove("qc-profile-required");
  }

  function showGate() {
    renderGate();
    var gate = document.getElementById("qc-profile-gate");
    if (gate) gate.classList.remove("qc-profile-gate-hidden");
    document.body.classList.add("qc-profile-required");
  }

  function chooseProfile(name) {
    name = cleanName(name);
    if (!name) return;
    // Add to in-memory list if new
    var profile = findProfile(name);
    if (!profile) {
      profile = {
        name: name,
        createdAt: new Date().toISOString(),
        lastUsedAt: null,
        settings: Object.assign({}, defaultSettings)
      };
      state.profiles.push(profile);
    }
    state.activeProfile = name;
    profile.lastUsedAt = new Date().toISOString();
    applyProfileSettings(profile);
    setCoderInput(name);
    hideGate();
    renderGate();
    sendState("select");
  }

  function renderGate() {
    var list = document.getElementById("qc-profile-list");
    if (!list) return;
    list.innerHTML = "";

    if (state.profiles.length === 0) {
      var empty = document.createElement("p");
      empty.className = "qc-profile-empty";
      empty.textContent = "No saved profiles yet.";
      list.appendChild(empty);
      return;
    }

    state.profiles
      .slice()
      .sort(function(a, b) {
        return String(b.lastUsedAt || "").localeCompare(String(a.lastUsedAt || ""));
      })
      .forEach(function(profile) {
        var button = document.createElement("button");
        button.type = "button";
        button.className = "qc-profile-choice";
        button.dataset.profile = profile.name;
        button.textContent = profile.name;
        list.appendChild(button);
      });
  }

  function handleGateClick(event) {
    var choice = event.target.closest(".qc-profile-choice");
    if (choice) {
      chooseProfile(choice.dataset.profile);
      return;
    }
    if (event.target.id === "qc-profile-create") {
      var input = document.getElementById("qc-profile-new-name");
      chooseProfile(input && input.value);
      if (input) input.value = "";
    }
  }

  function initGate() {
    var gate = document.getElementById("qc-profile-gate");
    if (!gate) return;
    gate.addEventListener("click", handleGateClick);

    var input = document.getElementById("qc-profile-new-name");
    if (input) {
      input.addEventListener("keydown", function(event) {
        if (event.key === "Enter") {
          event.preventDefault();
          chooseProfile(input.value);
          input.value = "";
        }
      });
    }

    renderGate();
  }

  function handleProfileAction(message) {
    var action = message && message.action;
    if (action === "create" || action === "switch") {
      chooseProfile(message.name);
    } else if (action === "delete") {
      var deleteName = cleanName(message.name);
      state.profiles = state.profiles.filter(function(p) {
        return p.name.toLowerCase() !== deleteName.toLowerCase();
      });
      if (state.activeProfile.toLowerCase() === deleteName.toLowerCase()) {
        state.activeProfile = "";
        state.settings = Object.assign({}, defaultSettings);
        applySettings(state.settings);
        showGate();
      } else {
        renderGate();
      }
      sendState("delete");
    } else if (action === "settings") {
      var settings = Object.assign({}, defaultSettings, message.settings || {});
      state.settings = settings;
      updateActiveProfileSettings(settings);
      applySettings(settings);
      sendState("settings");
    } else if (action === "reset_settings") {
      state.settings = Object.assign({}, defaultSettings);
      updateActiveProfileSettings(state.settings);
      applySettings(state.settings);
      sendState("reset_settings");
    } else if (action === "logout") {
      state.activeProfile = "";
      state.settings = Object.assign({}, defaultSettings);
      applySettings(state.settings);
      showGate();
      sendState("logout");
    }
  }

  function normalizeProfileList(message) {
    if (!message) return [];
    if (typeof message.profilesJson === "string" && message.profilesJson.length) {
      try {
        var parsed = JSON.parse(message.profilesJson);
        if (Array.isArray(parsed)) return parsed;
      } catch (error) {
        // Fall back to the Shiny object payload below.
      }
    }
    if (Array.isArray(message.profiles)) return message.profiles;
    if (message.profiles && typeof message.profiles === "object") {
      return Object.keys(message.profiles).map(function(key) {
        return message.profiles[key];
      });
    }
    return [];
  }

  function handleLoadProfiles(message) {
    if (!message) return;
    if (loadedFromLocalStorage && state.profiles.length > 0) {
      renderGate();
      return;
    }
    var profilesPayload = normalizeProfileList(message);

    state.profiles = profilesPayload.map(function(p) {
      return {
        name:       cleanName(p.name),
        createdAt:  p.createdAt  || new Date().toISOString(),
        lastUsedAt: p.lastUsedAt || null,
        settings:   Object.assign({}, defaultSettings, p.settings || {})
      };
    });

    // Auto-select: prefer suggestedActive (most recently used from DB),
    // then fall back to highest lastUsedAt in the list.
    var suggested = cleanName(message.suggestedActive || "");
    var target = null;
    if (suggested) {
      target = profilesPayload.find(function(p) {
        return cleanName(p.name).toLowerCase() === suggested.toLowerCase();
      });
    }
    if (!target) {
      var sorted = profilesPayload.slice().sort(function(a, b) {
        return String(b.lastUsedAt || "").localeCompare(String(a.lastUsedAt || ""));
      });
      if (sorted.length > 0) target = sorted[0];
    }

    // Apply the most-recently-used profile's settings silently (correct theme
    // before the user makes their selection) but do NOT auto-dismiss the gate.
    if (target) {
      applyProfileSettings(target);
    }

    saveLocalState();
    renderGate();
  }

  function initShinyBridge() {
    function registerHandler() {
      if (
        handlerRegistered ||
        !window.Shiny ||
        !window.Shiny.addCustomMessageHandler
      ) {
        return;
      }
      window.Shiny.addCustomMessageHandler("qc_profile_action", handleProfileAction);
      window.Shiny.addCustomMessageHandler("qc_load_profiles",  handleLoadProfiles);
      handlerRegistered = true;
    }

    registerHandler();
    if (!handlerRegistered) window.setTimeout(registerHandler, 50);
    document.addEventListener("shiny:connected", function() {
      registerHandler();
      applySettings(state.settings);
      renderGate();
      showGate();
      sendState("connected");
    });
  }

  function initTtsSettingsBridge() {
    document.addEventListener("change", function(event) {
      if (event.target && event.target.id === "settings_tts_voice") {
        event.target.dataset.preferredVoice = cleanName(event.target.value) || "auto";
      }
    });

    document.addEventListener("shown.bs.modal", function() {
      window.setTimeout(function() {
        syncTtsVoiceSelect(state.settings);
      }, 0);
    });

    if (window.speechSynthesis && window.speechSynthesis.addEventListener) {
      window.speechSynthesis.addEventListener("voiceschanged", function() {
        syncTtsVoiceSelect(state.settings);
      });
    }
  }

  document.addEventListener("DOMContentLoaded", function() {
    loadLocalState();
    applySettings(state.settings);
    initGate();
    showGate();
    initTtsSettingsBridge();
    initShinyBridge();
  });
})();
