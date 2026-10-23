"use strict";
const assert = require("node:assert/strict");
const vm = require("node:vm");
const fs = require("node:fs");
const code = fs.readFileSync(process.argv[2], "utf8");

function fixture(protocol = "https:", initiallyVisible = true, deferred = false,
  providers) {
  const callbacks = {};
  const timers = new Map();
  const made = [];
  let shown = initiallyVisible;
  let timerId = 0;
  let observed;
  const background = {on() {}, off() {}};
  const overlay = {ecological: true};
  const container = {querySelectorAll() { return []; }};
  const el = {
    style: {},
    getBoundingClientRect() { return {width: shown ? 600 : 0, height: 400}; },
    getClientRects() { return shown ? [1] : []; },
    setAttribute() {}
  };
  const map = {
    layers: [overlay, background],
    invalidations: 0,
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
        map.layers.push(layer);
      },
      removeLayer() { map.layers = map.layers.filter(x => !x.tile); }
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
      const layer = {
        tile: true, url, options,
        on(name, callback) { handlers[name] = callback; },
        off() { for (const key of Object.keys(handlers)) delete handlers[key]; },
        emit(name) { if (handlers[name]) handlers[name]({tile: {}}); },
        getContainer() { return container; },
        isLoading() { return true; }
      };
      made.push(layer);
      return layer;
    }},
    ResizeObserver: class {
      constructor(callback) { observed = callback; }
      observe() {}
      disconnect() {}
    },
    document: {addEventListener() {}, removeEventListener() {}},
    window: {
      location: {protocol},
      HTMLWidgets: {find() { return {resize() {
        map.layerManager = manager;
        map.leafletr.pendingRenderData = null;
      }}; }},
      setTimeout(callback) { const id = ++timerId; timers.set(id, callback); return id; },
      clearTimeout(id) { timers.delete(id); },
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
  return {
    el, map, made, overlay, timers, callbacks,
    reveal() { shown = true; observed(); },
    expire() { const [id, fn] = timers.entries().next().value; timers.delete(id); fn(); }
  };
}

let f = fixture();
assert.equal(f.made[0].url, "primary");
for (let i = 0; i < 3; i++) f.made[0].emit("tileerror");
assert.equal(f.made[1].url, "secondary");
assert(f.map.layers.includes(f.overlay));
f.made[1].emit("load");
assert.equal(f.timers.size, 0);
assert.equal(f.el._camtrapBasemap.provider, "secondary");

f = fixture();
f.expire(); f.expire(); f.expire();
assert.deepEqual(Array.from(f.el._camtrapBasemap.history), ["primary", "secondary", "last"]);
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);
assert.equal(f.map.layers.length, 2);

f = fixture("https:", false);
assert.equal(f.made.length, 0);
f.reveal();
assert.equal(f.made.length, 1);
assert(f.map.invalidations > 0);

f = fixture("https:", false, true);
f.reveal();
assert(f.map.layerManager);
assert.equal(f.map.leafletr.pendingRenderData, null);
assert.equal(f.map.layers.length, 3);

f = fixture("https:", true, false, []);
assert.equal(f.made.length, 0);
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);

f = fixture("file:");
assert.equal(f.made[0].url, "secondary");
f.callbacks.beforeprint();
assert.equal(f.el._camtrapBasemap.provider, "offline");
assert.equal(f.timers.size, 0);
assert(f.map.layers.includes(f.overlay));
f.callbacks.unload();
console.log("basemap runtime checks passed");
