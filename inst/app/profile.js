(function() {
  "use strict";

  var PROFILE_KEY = "saturate.profiles.v1";
  var ACTIVE_KEY = "saturate.activeProfile.v1";
  var SETTINGS_KEY = "saturate.settings.v1";

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
    showLineNumbers: false,
    highlightOpacity: 0.33
  };
  var handlerRegistered = false;

  function readJson(key, fallback) {
    try {
      var raw = window.localStorage.getItem(key);
      if (!raw) return fallback;
      var parsed = JSON.parse(raw);
      return parsed == null ? fallback : parsed;
    } catch (e) {
      return fallback;
    }
  }

  function writeJson(key, value) {
    try {
      window.localStorage.setItem(key, JSON.stringify(value));
    } catch (e) {
      window.console && window.console.warn("Could not save saturate settings", e);
    }
  }

  function cleanName(name) {
    return String(name || "").replace(/\s+/g, " ").trim();
  }

  function readProfiles() {
    var raw = readJson(PROFILE_KEY, []);
    if (!Array.isArray(raw)) return [];
    var seen = {};
    return raw.reduce(function(out, p) {
      var name = cleanName(typeof p === "string" ? p : p.name);
      if (!name || seen[name.toLowerCase()]) return out;
      seen[name.toLowerCase()] = true;
      out.push({
        name: name,
        createdAt: p.createdAt || new Date().toISOString(),
        lastUsedAt: p.lastUsedAt || null
      });
      return out;
    }, []);
  }

  function saveProfiles(profiles) {
    writeJson(PROFILE_KEY, profiles);
  }

  function readSettings() {
    var settings = readJson(SETTINGS_KEY, {});
    return Object.assign({}, defaultSettings, settings || {});
  }

  function saveSettings(settings) {
    writeJson(SETTINGS_KEY, Object.assign({}, defaultSettings, settings || {}));
  }

  function resetSettings() {
    var settings = Object.assign({}, defaultSettings);
    saveSettings(settings);
    return settings;
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

  function activeProfile() {
    try {
      return cleanName(window.localStorage.getItem(ACTIVE_KEY));
    } catch (e) {
      return "";
    }
  }

  function setActiveProfile(name) {
    name = cleanName(name);
    try {
      if (name) window.localStorage.setItem(ACTIVE_KEY, name);
      else window.localStorage.removeItem(ACTIVE_KEY);
    } catch (e) {
      window.console && window.console.warn("Could not save saturate profile", e);
    }
  }

  function ensureProfile(name) {
    name = cleanName(name);
    if (!name) return null;

    var profiles = readProfiles();
    var existing = profiles.find(function(p) {
      return p.name.toLowerCase() === name.toLowerCase();
    });

    if (existing) return existing.name;

    profiles.push({
      name: name,
      createdAt: new Date().toISOString(),
      lastUsedAt: null
    });
    saveProfiles(profiles);
    return name;
  }

  function touchProfile(name) {
    var profiles = readProfiles();
    profiles.forEach(function(p) {
      if (p.name.toLowerCase() === name.toLowerCase()) {
        p.lastUsedAt = new Date().toISOString();
      }
    });
    saveProfiles(profiles);
  }

  function deleteProfile(name) {
    name = cleanName(name);
    if (!name) return;
    var profiles = readProfiles().filter(function(p) {
      return p.name.toLowerCase() !== name.toLowerCase();
    });
    saveProfiles(profiles);
    if (activeProfile().toLowerCase() === name.toLowerCase()) {
      setActiveProfile("");
    }
  }

  function applySettings(settings) {
    settings = Object.assign({}, defaultSettings, settings || {});
    var root = document.documentElement;
    var theme = choiceSetting(
      settings,
      "colorTheme",
      "light",
      ["light", "dark", "contrast"]
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

    root.setAttribute("data-sat-theme", theme);
    root.setAttribute("data-sat-density", density);
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
  }

  function setCoderInput(name) {
    var input = document.getElementById("current_coder");
    if (!input) return;
    input.value = name;
    input.setAttribute("readonly", "readonly");
    input.setAttribute("title", "Change profile from Settings");
    input.dispatchEvent(new Event("input", { bubbles: true }));
    input.dispatchEvent(new Event("change", { bubbles: true }));
  }

  function sendState(reason) {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    var state = {
      profiles: readProfiles(),
      activeProfile: activeProfile(),
      settings: readSettings(),
      reason: reason || "sync",
      nonce: Date.now()
    };
    window.Shiny.setInputValue("profile_state", state, { priority: "event" });
    if (state.activeProfile) {
      window.Shiny.setInputValue(
        "profile_selected",
        { name: state.activeProfile, nonce: Date.now() },
        { priority: "event" }
      );
    }
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
    name = ensureProfile(name);
    if (!name) return;
    setActiveProfile(name);
    touchProfile(name);
    setCoderInput(name);
    hideGate();
    renderGate();
    sendState("select");
  }

  function renderGate() {
    var list = document.getElementById("qc-profile-list");
    if (!list) return;
    var profiles = readProfiles();
    list.innerHTML = "";

    if (profiles.length === 0) {
      var empty = document.createElement("p");
      empty.className = "qc-profile-empty";
      empty.textContent = "No saved profiles yet.";
      list.appendChild(empty);
      return;
    }

    profiles
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
    if (action === "create") {
      chooseProfile(message.name);
    } else if (action === "switch") {
      chooseProfile(message.name);
    } else if (action === "delete") {
      deleteProfile(message.name);
      if (activeProfile()) hideGate();
      else showGate();
      sendState("delete");
    } else if (action === "settings") {
      var settings = Object.assign({}, defaultSettings, message.settings || {});
      saveSettings(settings);
      applySettings(settings);
      sendState("settings");
    } else if (action === "reset_settings") {
      var defaulted = resetSettings();
      applySettings(defaulted);
      sendState("reset_settings");
    } else if (action === "logout") {
      setActiveProfile("");
      showGate();
      sendState("logout");
    }
  }

  function handleLoadProfiles(message) {
    if (!message || !Array.isArray(message.profiles)) return;

    // Seed localStorage from DB (DB is the authoritative source)
    var dbProfiles = message.profiles.map(function(p) {
      return {
        name:       cleanName(p.name),
        createdAt:  p.createdAt  || new Date().toISOString(),
        lastUsedAt: p.lastUsedAt || null
      };
    });
    saveProfiles(dbProfiles);

    // Apply settings for whichever profile is active (or most recently used)
    var active = activeProfile();
    var target = message.profiles.find(function(p) {
      return active && cleanName(p.name).toLowerCase() === active.toLowerCase();
    });
    if (!target) {
      var sorted = message.profiles.slice().sort(function(a, b) {
        return String(b.lastUsedAt || "").localeCompare(String(a.lastUsedAt || ""));
      });
      if (sorted.length > 0 && sorted[0].lastUsedAt) target = sorted[0];
    }
    if (target && target.settings && Object.keys(target.settings).length > 0) {
      var merged = Object.assign({}, defaultSettings, target.settings);
      saveSettings(merged);
      applySettings(merged);
    }

    renderGate();
    sendState("db_sync");
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
    document.addEventListener("shiny:connected", function() {
      registerHandler();
      applySettings(readSettings());
      renderGate();
      var active = activeProfile();
      if (active) chooseProfile(active);
      else showGate();
      sendState("connected");
    });
  }

  document.addEventListener("DOMContentLoaded", function() {
    applySettings(readSettings());
    initGate();
    initShinyBridge();
  });
})();
