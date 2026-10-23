function(el, x, config) {
  "use strict";
  var map = this;
  // Padded data bounds do not always cover the whole fitted viewport.
  // Keep their surroundings consistent with the self-contained background.
  el.style.backgroundColor = "#F2F1EC";
  var transparent = "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs=";
  var sources = (config.providers || []).filter(function(source) {
    // file:// pages cannot send OSM's required web Referer. Do not spoof it.
    return !source.requiresWebReferrer ||
      /^https?:$/.test(window.location.protocol);
  });
  var index = -1;
  var layer = null;
  var timer = null;
  var finished = false;
  var printing = false;
  var disposed = false;
  var printStyles = [];
  var errors = 0;
  var pending = new Set();
  var id = "camtrap-report-online-background";
  var manager = map.layerManager;
  var background = manager && manager.getLayerGroup(config.group, false);
  var state = {provider: "offline", history: [], reason: "initializing"};
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
      advance("tile timeout");
    }, config.timeout || 8000);
  }

  function start() {
    if (finished || disposed || !visible()) return;
    if (printing) return offline("print uses the offline background");
    if (index >= sources.length) return offline("online providers exhausted");
    var source = sources[index];
    errors = 0;
    pending.clear();
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
    layer.on("loading", function() {
      errors = 0;
      if (printing) return offline("print avoids pending tile requests");
      armTimer();
    });
    layer.on("tileloadstart", function(event) {
      pending.add(event.tile);
      armTimer();
    });
    layer.on("tileload", function(event) { pending.delete(event.tile); });
    layer.on("tileunload", function(event) { pending.delete(event.tile); });
    layer.on("tileerror", function(event) {
      pending.delete(event.tile);
      errors += 1;
      if (errors >= (config.errorLimit || 3)) advance("repeated tile errors");
    });
    layer.on("load", function() {
      clearTimer();
      if (errors > 0) advance("incomplete background tiles");
      else resolveReady(state.provider);
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
    if (disposed || finished) return;
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
    if (!layer || pending.size > 0 || layer.isLoading()) {
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
    stopLayer();
    resolveReady("unloaded");
    if (resizeObserver) resizeObserver.disconnect();
    document.removeEventListener("shown.bs.tab", refresh, true);
    if (window.jQuery) window.jQuery(document).off("shown.bs.tab", refresh);
    window.removeEventListener("resize", refresh);
    window.removeEventListener("beforeprint", beforePrint);
    window.removeEventListener("afterprint", afterPrint);
    if (background) background.off("add remove", refresh);
  });
  refresh();
  // Unvisited tabs remain fully usable with their already-present offline
  // background and must not make printing wait or prefetch unseen viewports.
  if (!visible()) resolveReady("hidden offline background");
}
