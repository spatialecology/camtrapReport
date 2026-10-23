function(el, x, config) {
  "use strict";
  var map = this;
  // Padded data bounds do not always cover the whole fitted viewport.
  // Keep their surroundings consistent with the self-contained background.
  el.style.backgroundColor = "#F2F1EC";
  var transparent = "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs=";
  var webPage = /^https?:$/.test(window.location.protocol);
  function publicWebsite() {
    if (!webPage) return false;
    var host = (window.location.hostname || "").toLowerCase()
      .replace(/[.]$/, "").replace(/^\[|\]$/g, "");
    if (!host || host === "localhost" || /[.]localhost$/.test(host) ||
        host === "::1" || host === "::" || /^f[cd][0-9a-f]{2}:/.test(host) ||
        /^fe[89ab][0-9a-f]:/.test(host)) return false;
    var ipv4 = host.match(/^(\d{1,3})[.](\d{1,3})[.](\d{1,3})[.](\d{1,3})$/);
    if (ipv4) {
      var first = Number(ipv4[1]);
      var second = Number(ipv4[2]);
      if (first === 0 || first === 10 || first === 127 ||
          (first === 172 && second >= 16 && second <= 31) ||
          (first === 192 && second === 168) ||
          (first === 169 && second === 254)) return false;
    }
    return true;
  }
  var skipped = [];
  var sources = (config.providers || []).filter(function(source) {
    // file:// pages cannot send OSM's required web Referer. Do not spoof it.
    var reason = source.requiresPublicWebsite && !publicWebsite() ?
      "requires a public HTTP(S) website" :
      (source.requiresWebReferrer && !webPage ? "requires a web Referer" : "");
    if (reason) skipped.push({provider: source.name, reason: reason});
    return !reason;
  });
  var index = -1;
  var layer = null;
  var timer = null;
  var activationTimer = null;
  var activationFrame = null;
  var initialized = false;
  var finished = false;
  var printing = false;
  var resumeAfterPrint = false;
  var disposed = false;
  var printStyles = [];
  var pending = new Set();
  var loaded = new Set();
  var failed = new Set();
  var id = "camtrap-report-online-background";
  var manager = map.layerManager;
  var background = manager && manager.getLayerGroup(config.group, false);
  var state = {
    provider: "offline", history: [], reason: "initializing", skipped: skipped
  };
  var resolveReady;
  state.ready = new Promise(function(resolve) { resolveReady = resolve; });
  window.camtrapReportBasemaps = window.camtrapReportBasemaps || [];
  window.camtrapReportBasemaps.push(state);
  window.camtrapReportBasemapsReady = function() {
    return Promise.all(window.camtrapReportBasemaps.map(function(item) {
      return item.ready;
    }));
  };
  el._camtrapBasemap = state;

  function visible() {
    var rect = el.getBoundingClientRect();
    return rect.width > 0 && rect.height > 0 &&
      el.getClientRects().length > 0 &&
      (!background || map.hasLayer(background));
  }

  function finishPendingRender() {
    // Leaflet deliberately defers its layer construction in zero-size tabs.
    // Complete it synchronously once visible, including during print layout.
    if (map.leafletr && map.leafletr.pendingRenderData && window.HTMLWidgets) {
      var widget = window.HTMLWidgets.find("#" + el.id);
      if (widget && widget.resize) widget.resize(el.offsetWidth, el.offsetHeight);
    }
    manager = map.layerManager;
    var currentGroup = manager && manager.getLayerGroup(config.group, false);
    if (currentGroup !== background) {
      if (background) background.off("add remove", refresh);
      background = currentGroup;
      if (background) background.on("add remove", refresh);
    }
  }

  function clearTimer() {
    if (timer !== null) window.clearTimeout(timer);
    timer = null;
  }

  function stopLayer() {
    clearTimer();
    if (!layer) return;
    layer.off();
    var container = layer.getContainer();
    if (container) {
      Array.prototype.forEach.call(container.querySelectorAll("img"),
        function(image) { image.src = transparent; });
    }
    if (manager) manager.removeLayer("tile", id);
    else map.removeLayer(layer);
    layer = null;
    pending.clear();
    loaded.clear();
    failed.clear();
  }

  function offline(reason) {
    stopLayer();
    finished = true;
    state.provider = "offline";
    state.reason = reason;
    el.setAttribute("data-camtrap-basemap", "offline");
    resolveReady("offline");
  }

  function armTimer() {
    if (timer !== null || !layer || !visible() || finished) return;
    timer = window.setTimeout(function() {
      timer = null;
      if (!visible()) return;
      if (loaded.size === 0 || severeFailure(true)) {
        advance("tile timeout");
      } else {
        // Keep useful imagery while cancelling a few stalled tile requests.
        var stalled = Array.from(pending);
        pending.clear();
        stalled.forEach(function(tile) {
          failed.add(tile);
          if (typeof tile.src === "string") tile.src = transparent;
        });
        resolveReady(state.provider);
      }
    }, config.timeout || 8000);
  }

  function severeFailure(includePending) {
    var unavailable = failed.size + (includePending ? pending.size : 0);
    var total = loaded.size + unavailable;
    return unavailable >= (config.errorLimit || 3) && total > 0 &&
      unavailable / total >= (config.failureRatio || 0.75);
  }

  function settle() {
    if (pending.size > 0) return;
    if (severeFailure(false)) return advance("background tile coverage failed");
    if (loaded.size > 0) {
      clearTimer();
      resolveReady(state.provider);
    } else {
      armTimer();
    }
  }

  function start() {
    if (finished || disposed || !visible()) return;
    if (printing) return offline("print uses the offline background");
    if (index >= sources.length) return offline("online providers exhausted");
    var source = sources[index];
    pending.clear();
    loaded.clear();
    failed.clear();
    state.provider = source.name;
    state.history.push(source.name);
    el.setAttribute("data-camtrap-basemap", source.name);
    layer = L.tileLayer(source.url, {
      attribution: source.attribution,
      subdomains: source.subdomains || "abc",
      maxNativeZoom: source.maxNativeZoom,
      maxZoom: 24,
      zIndex: 1,
      errorTileUrl: transparent,
      updateWhenIdle: true,
      keepBuffer: 0,
      referrerPolicy: "strict-origin-when-cross-origin"
    });
    var activeLayer = layer;
    function current() { return layer === activeLayer && !disposed && !finished; }
    layer.on("loading", function() {
      if (!current()) return;
      if (printing) return offline("print avoids pending tile requests");
      armTimer();
    });
    layer.on("tileloadstart", function(event) {
      if (!current()) return;
      loaded.delete(event.tile);
      failed.delete(event.tile);
      pending.add(event.tile);
      armTimer();
    });
    layer.on("tileload", function(event) {
      if (!current() || (!pending.has(event.tile) && !failed.has(event.tile))) return;
      pending.delete(event.tile);
      // Leaflet can emit tileload for errorTileUrl after the real tile failed.
      if (!failed.has(event.tile)) loaded.add(event.tile);
      settle();
    });
    layer.on("tileunload", function(event) {
      if (!current()) return;
      pending.delete(event.tile);
      loaded.delete(event.tile);
      failed.delete(event.tile);
      settle();
    });
    layer.on("tileerror", function(event) {
      if (!current() || !pending.has(event.tile)) return;
      pending.delete(event.tile);
      loaded.delete(event.tile);
      failed.add(event.tile);
      settle();
    });
    layer.on("load", function() {
      if (current()) settle();
    });
    if (manager) manager.addLayer(layer, "tile", id, config.group);
    else layer.addTo(map);
    armTimer();
  }

  function advance(reason) {
    if (finished || disposed) return;
    stopLayer();
    index += 1;
    state.reason = reason;
    if (printing || index >= sources.length) return offline(reason);
    start();
  }

  function refresh() {
    if (disposed || finished || !initialized) return;
    if (!visible()) {
      clearTimer();
      return;
    }
    finishPendingRender();
    map.invalidateSize({pan: false});
    if (!layer) {
      if (index < 0) advance("initial viewport");
      else start();
    } else if (pending.size > 0 || layer.isLoading()) armTimer();
  }

  function beforePrint() {
    if (!printing) resumeAfterPrint = !finished && sources.length > 0;
    printing = true;
    // Chrome's beforeprint fires before print CSS takes effect. Reveal only
    // hidden tab ancestors long enough to materialize deferred Leaflet data.
    for (var parent = el.parentElement; parent; parent = parent.parentElement) {
      if (!parent.classList || !parent.classList.contains("tab-pane")) continue;
      var computed = window.getComputedStyle(parent);
      if (computed.display !== "none" && computed.opacity !== "0") continue;
      printStyles.push({
        element: parent,
        display: parent.style.getPropertyValue("display"),
        displayPriority: parent.style.getPropertyPriority("display"),
        opacity: parent.style.getPropertyValue("opacity"),
        opacityPriority: parent.style.getPropertyPriority("opacity")
      });
      parent.style.setProperty("display", "block", "important");
      parent.style.setProperty("opacity", "1", "important");
    }
    if (visible()) {
      finishPendingRender();
      map.invalidateSize({pan: false});
    }
    // Keep already loaded imagery. New/unfinished views print offline instead
    // of fetching every hidden tab or waiting indefinitely for remote tiles.
    if (!layer || loaded.size === 0 || pending.size > 0 || layer.isLoading()) {
      offline("print avoids pending tile requests");
    }
  }

  function afterPrint() {
    printing = false;
    printStyles.forEach(function(saved) {
      saved.element.style.setProperty("display", saved.display, saved.displayPriority);
      saved.element.style.setProperty("opacity", saved.opacity, saved.opacityPriority);
    });
    printStyles = [];
    if (resumeAfterPrint) {
      finished = false;
      resumeAfterPrint = false;
    }
    refresh();
  }

  var resizeObserver = typeof ResizeObserver === "function" ?
    new ResizeObserver(refresh) : null;
  if (resizeObserver) resizeObserver.observe(el);
  document.addEventListener("shown.bs.tab", refresh, true);
  if (window.jQuery) window.jQuery(document).on("shown.bs.tab", refresh);
  window.addEventListener("resize", refresh);
  window.addEventListener("beforeprint", beforePrint);
  window.addEventListener("afterprint", afterPrint);
  if (background) background.on("add remove", refresh);
  map.on("unload", function() {
    disposed = true;
    if (activationTimer !== null) window.clearTimeout(activationTimer);
    if (activationFrame !== null && window.cancelAnimationFrame) {
      window.cancelAnimationFrame(activationFrame);
    }
    stopLayer();
    resolveReady("unloaded");
    if (resizeObserver) resizeObserver.disconnect();
    document.removeEventListener("shown.bs.tab", refresh, true);
    document.removeEventListener("DOMContentLoaded", scheduleActivation);
    if (window.jQuery) window.jQuery(document).off("shown.bs.tab", refresh);
    window.removeEventListener("resize", refresh);
    window.removeEventListener("beforeprint", beforePrint);
    window.removeEventListener("afterprint", afterPrint);
    if (background) background.off("add remove", refresh);
  });

  function activate() {
    if (disposed || initialized) return;
    initialized = true;
    refresh();
    // Assess visibility after rmarkdown has built its Bootstrap tab panes.
    if (!visible()) resolveReady("hidden offline background");
  }

  function scheduleActivation() {
    if (disposed || initialized || activationTimer !== null ||
        activationFrame !== null) return;
    // HTMLWidgets can run before buildTabsets() in jQuery's ready queue.
    // A macrotask and two layout frames avoid prefetching future hidden tabs,
    // without waiting for unrelated external images or the window load event.
    activationTimer = window.setTimeout(function() {
      activationTimer = null;
      if (typeof window.requestAnimationFrame !== "function") return activate();
      activationFrame = window.requestAnimationFrame(function() {
        activationFrame = window.requestAnimationFrame(function() {
          activationFrame = null;
          activate();
        });
      });
    }, 0);
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", scheduleActivation, {once: true});
  } else {
    scheduleActivation();
  }
}
