"use strict";
const assert = require("node:assert/strict");
const vm = require("node:vm");
const fs = require("node:fs");
const code = fs.readFileSync(process.argv[2], "utf8");

function fixture(protocol = "https:", initiallyVisible = true, deferred = false,
  providers, hostname = "example.org", deferActivation = false,
  documentReadyState = "complete") {
  const callbacks = {};
  const timers = new Map();
  const frames = new Map();
  const made = [];
  let shown = initiallyVisible;
  let timerId = 0;
  let observed;
  function tab(parent) {
    const properties = {};
    return {
      parentElement: parent,
      properties,
      classList: {contains(name) { return name === "tab-pane"; }},
      style: {
        getPropertyValue(name) { return properties[name] || ""; },
        getPropertyPriority() { return ""; },
        setProperty(name, value) { properties[name] = value; }
      }
    };
  }
  const outerTab = tab(null);
  const innerTab = tab(outerTab);
  function visible() {
    return shown || (innerTab.properties.display === "block" &&
      outerTab.properties.display === "block");
  }
  const background = {on() {}, off() {}};
  const overlay = {ecological: true};
  const el = {
    id: "map-fixture", style: {}, parentElement: innerTab,
    get offsetWidth() { return visible() ? 600 : 0; },
    get offsetHeight() { return visible() ? 400 : 0; },
    getBoundingClientRect() {
      return {width: visible() ? 600 : 0, height: visible() ? 400 : 0};
    },
    getClientRects() { return visible() ? [1] : []; },
    setAttribute() {}
  };
  const map = {
    layers: [overlay, background], invalidations: 0,
    hasLayer(item) { return this.layers.includes(item); },
    invalidateSize(options) {
      assert.equal(options.pan, false);
      this.invalidations += 1;
    },
    on(name, callback) { callbacks[name] = callback; },
    layerManager: {
      getLayerGroup() { return background; },
      addLayer(layer, type, id, group) {
        assert.equal(type, "tile");
        assert.equal(group, "Background");
        assert.equal(map.layers.filter(item => item.tile).length, 0);
        map.layers.push(layer);
      },
      removeLayer() { map.layers = map.layers.filter(item => !item.tile); }
    }
  };
  const manager = map.layerManager;
  if (deferred) {
    map.layerManager = undefined;
    map.leafletr = {pendingRenderData: {}};
  }
  const context = {
    Set, Promise,
    L: {tileLayer(url, options) {
      const handlers = {};
      const tiles = [];
      let loading = false;
      const layer = {
        tile: true, url, options,
        on(name, callback) { handlers[name] = callback; },
        off() { for (const key of Object.keys(handlers)) delete handlers[key]; },
        emit(name, tile) {
          if (name === "loading") loading = true;
          if (name === "load") loading = false;
          if (name === "tileloadstart" && !tiles.includes(tile)) tiles.push(tile);
          if (handlers[name]) handlers[name]({tile});
        },
        getContainer() { return {querySelectorAll() { return tiles; }}; },
        isLoading() { return loading; }
      };
      made.push(layer);
      return layer;
    }},
    ResizeObserver: class {
      constructor(callback) { observed = callback; }
      observe() {}
      disconnect() {}
    },
    document: {
      readyState: documentReadyState,
      addEventListener(name, callback) { callbacks[name] = callback; },
      removeEventListener() {}
    },
    window: {
      location: {protocol, hostname},
      getComputedStyle(parent) {
        return {
          display: parent.properties.display || (shown ? "block" : "none"),
          opacity: parent.properties.opacity || "1"
        };
      },
      HTMLWidgets: {find() { return {resize(width, height) {
        assert(width > 0 && height > 0);
        map.layerManager = manager;
        map.leafletr.pendingRenderData = null;
      }}; }},
      setTimeout(callback, delay) {
        assert(delay === 0 || delay === 100);
        const id = ++timerId;
        timers.set(id, {callback, delay});
        return id;
      },
      clearTimeout(id) { timers.delete(id); },
      requestAnimationFrame(callback) {
        const id = ++timerId;
        frames.set(id, callback);
        return id;
      },
      cancelAnimationFrame(id) { frames.delete(id); },
      addEventListener(name, callback) { callbacks[name] = callback; },
      removeEventListener() {}
    }
  };
  const run = vm.runInNewContext("(" + code + ")", context);
  run.call(map, el, {}, {
    group: "Background", timeout: 100, errorLimit: 3,
    providers: providers || [
      {name: "primary", url: "primary", requiresWebReferrer: true},
      {name: "secondary", url: "secondary"},
      {name: "last", url: "last"}
    ]
  });
  function activate() {
    if (callbacks.DOMContentLoaded) callbacks.DOMContentLoaded();
    for (const [id, timer] of timers) {
      if (timer.delay !== 0) continue;
      timers.delete(id);
      timer.callback();
    }
    for (let frame = 0; frame < 2; frame++) {
      const currentFrames = Array.from(frames);
      frames.clear();
      currentFrames.forEach(([, callback]) => callback());
    }
  }
  if (!deferActivation) activate();
  return {
    el, map, made, overlay, timers, callbacks, innerTab, outerTab, activate,
    reveal() { shown = true; observed(); },
    hide() { shown = false; observed(); },
    expire() {
      assert(timers.size > 0, "a bounded provider timer should be armed");
      const [id, timer] = timers.entries().next().value;
      timers.delete(id);
      timer.callback();
    }
  };
}

function begin(layer, count) {
  layer.emit("loading");
  return Array.from({length: count}, (_, index) => {
    const tile = {src: layer.url + "/" + index};
    layer.emit("tileloadstart", tile);
    return tile;
  });
}
function assertOverlay(f) {
  assert(f.map.layers.includes(f.overlay));
  assert(f.map.layers.filter(item => item.tile).length <= 1);
}

// A single missing tile, even emitted repeatedly, is not a failed provider.
let f = fixture();
let first = f.made[0];
let tiles = begin(first, 4);
first.emit("tileerror", tiles[0]);
first.emit("tileerror", tiles[0]);
first.emit("tileerror", tiles[0]);
assert.equal(f.made.length, 1);
tiles.slice(1).forEach(tile => first.emit("tileload", tile));
first.emit("load");
assert.equal(f.el._camtrapBasemap.provider, "primary");
assert.equal(f.timers.size, 0);
assertOverlay(f);

// Moderate partial failures retain useful background imagery.
f = fixture(); first = f.made[0]; tiles = begin(first, 5);
tiles.slice(0, 2).forEach(tile => first.emit("tileerror", tile));
tiles.slice(2).forEach(tile => first.emit("tileload", tile));
first.emit("load");
assert.equal(f.made.length, 1);
assert.equal(f.el._camtrapBasemap.provider, "primary");

// Wait for outstanding siblings before declaring severe (75%) coverage loss.
f = fixture(); first = f.made[0]; tiles = begin(first, 4);
tiles.slice(0, 3).forEach(tile => first.emit("tileerror", tile));
assert.equal(f.made.length, 1);
first.emit("tileload", tiles[3]);
assert.equal(f.made[1].url, "secondary");
assert.equal(f.map.layers.includes(first), false);
assertOverlay(f);
let secondaryTiles = begin(f.made[1], 2);
secondaryTiles.forEach(tile => f.made[1].emit("tileload", tile));
f.made[1].emit("load");
assert.equal(f.timers.size, 0);

// Error placeholders are not real successful tiles.
f = fixture(); first = f.made[0]; tiles = begin(first, 1);
first.emit("tileerror", tiles[0]);
first.emit("tileload", tiles[0]);
first.emit("load");
assert.equal(f.made.length, 1);
f.expire();
assert.equal(f.made[1].url, "secondary");

// A completely failing provider advances, with just one active background.
f = fixture(); first = f.made[0]; tiles = begin(first, 3);
tiles.forEach(tile => first.emit("tileerror", tile));
assert.equal(f.made[1].url, "secondary");
assertOverlay(f);

// All stalled providers exhaust the chain exactly once, without throwing.
f = fixture();
f.expire(); f.expire(); f.expire();
assert.deepEqual(Array.from(f.el._camtrapBasemap.history),
  ["primary", "secondary", "last"]);
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);
assert.equal(f.map.layers.length, 2);
f.reveal();
assert.equal(f.made.length, 3);
f.callbacks.beforeprint();
f.callbacks.afterprint();
assert.equal(f.made.length, 3);
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);

// A few stalled requests are cancelled while viable loaded imagery remains.
f = fixture(); first = f.made[0]; tiles = begin(first, 2);
first.emit("tileload", tiles[0]);
f.expire();
assert.equal(f.el._camtrapBasemap.provider, "primary");
assert.equal(tiles[0].src, "primary/0");
assert(tiles[1].src.startsWith("data:image/gif"));
first.emit("tileerror", tiles[1]);
first.emit("load");
assert.equal(f.made.length, 1);
assert.equal(f.timers.size, 0);
assertOverlay(f);

// A nearly empty viewport with stalled tiles still gets a bounded fallback.
f = fixture(); first = f.made[0]; tiles = begin(first, 4);
first.emit("tileload", tiles[0]);
f.expire();
assert.equal(f.el._camtrapBasemap.provider, "secondary");
assertOverlay(f);

// Already loaded, retained viewport tiles count toward provider viability.
f = fixture(); first = f.made[0]; tiles = begin(first, 4);
tiles.forEach(tile => first.emit("tileload", tile));
first.emit("load");
let edgeTiles = begin(first, 3);
edgeTiles.forEach(tile => first.emit("tileerror", tile));
first.emit("load");
assert.equal(f.made.length, 1);
assert.equal(f.timers.size, 0);
tiles.slice(0, 2).forEach(tile => first.emit("tileunload", tile));
assert.equal(f.made.length, 1);
first.emit("tileunload", tiles[2]);
assert.equal(f.el._camtrapBasemap.provider, "secondary");
first.emit("tileunload", tiles[3]);
assert.equal(f.made.length, 2);
assertOverlay(f);

// Unloading a healthy old viewport before new requests is not itself failure.
f = fixture(); first = f.made[0]; tiles = begin(first, 4);
tiles.forEach(tile => first.emit("tileload", tile));
first.emit("load");
tiles.forEach(tile => first.emit("tileunload", tile));
assert.equal(f.made.length, 1);
assert.equal(f.timers.size, 1);
edgeTiles = begin(first, 3);
edgeTiles.forEach(tile => first.emit("tileload", tile));
first.emit("load");
assert.equal(f.el._camtrapBasemap.provider, "primary");
assert.equal(f.timers.size, 0);
assertOverlay(f);

// Late events for unloaded tiles or removed providers cannot change state.
f = fixture(); first = f.made[0]; tiles = begin(first, 2);
first.emit("tileunload", tiles[0]);
first.emit("tileerror", tiles[0]);
first.emit("tileload", tiles[0]);
first.emit("tileload", tiles[1]);
first.emit("load");
assert.equal(f.made.length, 1);
assert.equal(f.timers.size, 0);
f.hide();
assert.equal(f.timers.size, 0);
f.reveal();
assert.equal(f.made.length, 1);

// Nested hidden widgets delay construction, then resize on tab visibility.
f = fixture("https:", false, true);
assert.equal(f.made.length, 0);
f.reveal();
assert.equal(f.made.length, 1);
assert(f.map.layerManager);
assert.equal(f.map.leafletr.pendingRenderData, null);
assert(f.map.invalidations > 0);

// Printing a hidden nested tab materializes overlays without online requests.
f = fixture("https:", false, true);
f.callbacks.beforeprint();
assert(f.map.layerManager);
assert.equal(f.map.leafletr.pendingRenderData, null);
assert.equal(f.made.length, 0);
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);
f.callbacks.afterprint();
assert.equal(f.innerTab.properties.display, "");
assert.equal(f.outerTab.properties.display, "");
assert.equal(f.made.length, 0);
f.reveal();
assert.equal(f.made.length, 1);
assertOverlay(f);

// Fully loaded online imagery is retained for printing.
f = fixture(); first = f.made[0]; tiles = begin(first, 2);
tiles.forEach(tile => first.emit("tileload", tile)); first.emit("load");
f.callbacks.beforeprint();
assert.equal(f.el._camtrapBasemap.provider, "primary");
assert.equal(f.map.layers.includes(first), true);
assertOverlay(f);

f = fixture("https:", true, false, []);
assert.equal(f.made.length, 0);
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);
f.callbacks.beforeprint();
f.callbacks.afterprint();
assert.equal(f.made.length, 0);

// Existing provider eligibility filtering remains authoritative.
f = fixture("file:");
assert.equal(f.made[0].url, "secondary");
f.callbacks.beforeprint();
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);
assertOverlay(f);
f.callbacks.afterprint();
assert.equal(f.el._camtrapBasemap.provider, "secondary");
assert.equal(f.made.length, 2);
f.callbacks.unload();
assert.equal(f.timers.size, 0);

// Public-service restrictions apply to the actual document context.
const restricted = [
  {name: "publicWebsite", url: "public", requiresPublicWebsite: true,
    requiresWebReferrer: true},
  {name: "anyWeb", url: "web", requiresWebReferrer: true},
  {name: "unrestricted", url: "fallback"}
];
for (const host of ["localhost", "sub.localhost", "LOCALHOST.", "127.0.0.1",
  "127.12.0.1", "10.2.3.4", "172.16.0.1", "172.31.255.254",
  "192.168.1.4", "169.254.1.2", "0.0.0.0", "[::1]", "fd00::1", "fe80::1"]) {
  f = fixture("https:", true, false, restricted, host);
  assert.equal(f.made[0].url, "web", host);
  assert.equal(f.el._camtrapBasemap.skipped[0].provider, "publicWebsite");
  assert.match(f.el._camtrapBasemap.skipped[0].reason, /public HTTP/);
}
for (const host of ["example.org", "fcloud.org", "172.32.0.1", "192.169.1.4",
  "2001:db8::1"]) {
  f = fixture("https:", true, false, restricted, host);
  assert.equal(f.made[0].url, "public", host);
  assert.equal(f.el._camtrapBasemap.skipped.length, 0);
}
f = fixture("file:", true, false, restricted, "");
assert.equal(f.made[0].url, "fallback");
assert.equal(f.el._camtrapBasemap.skipped.length, 2);
assert.match(f.el._camtrapBasemap.skipped[1].reason, /web Referer/);

// Tabset construction may hide maps after HTMLWidgets initially rendered them.
f = fixture("https:", true, false, undefined, "example.org", true);
assert.equal(f.made.length, 0);
f.hide();
f.activate();
assert.equal(f.made.length, 0);
assert.equal(f.timers.size, 0);
f.reveal();
assert.equal(f.made.length, 1);

f = fixture("https:", true, false, undefined, "example.org", true, "loading");
assert.equal(f.made.length, 0);
assert.equal(f.timers.size, 0);
f.activate();
assert.equal(f.made.length, 1);

// Printing before activation must never start an online request.
f = fixture("https:", true, false, undefined, "example.org", true);
f.callbacks.beforeprint();
f.activate();
assert.equal(f.made.length, 0);
assert.equal(f.el._camtrapBasemap.provider, "offline");
f.callbacks.afterprint();
assert.equal(f.el._camtrapBasemap.provider, "primary");
assert.equal(f.made.length, 1);
console.log("basemap runtime checks passed (22 deterministic scenario groups)");
