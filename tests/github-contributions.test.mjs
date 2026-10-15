import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import fs from "node:fs";

const source = fs.readFileSync(new URL("../github-contributions.js", import.meta.url), "utf8");
const sample = dataset("2025-10-12", 369, 1);
class Element {
  constructor(name) { this.name = name; this.children = []; this.attributes = {}; this.textContent = ""; this.hidden = true; }
  setAttribute(key, value) { this.attributes[key] = value; }
  appendChild(child) { this.children.push(child); return child; }
  replaceChildren(...children) { this.children = children; }
}
const flush = () => new Promise(resolve => setImmediate(resolve));
function harness(data = sample, options = {}) {
  const chart = new Element("div");
  const status = new Element("p");
  const requests = [];
  const intervals = [];
  const listeners = {};
  const timeouts = new Map();
  let now = Date.now();
  class Clock extends Date { static now() { return now; } }
  const document = {
    visibilityState: options.hidden ? "hidden" : "visible",
    getElementById: id => options.missing ? null : id.endsWith("chart") ? chart : status,
    createElementNS: (ns, name) => new Element(name),
    addEventListener: (name, callback) => { listeners[name] = callback; },
  };
  const state = { data, fail: false, invalidJSON: false, pending: false };
  const context = {
    document, Date: Clock, Intl, Number, Object, Array, String, Error, AbortController,
    window: {
      setInterval: (fn, ms) => intervals.push({fn, ms}),
      setTimeout: (fn, ms) => { const id = timeouts.size + 1; timeouts.set(id, {fn, ms}); return id; },
      clearTimeout: id => timeouts.delete(id),
    },
    fetch: async (url, options) => {
      requests.push({url, options});
      if (state.pending) return await new Promise((resolve, reject) => options.signal.addEventListener("abort", () => reject(new Error("aborted"))));
      return {ok: !state.fail, json: async () => { if (state.invalidJSON) throw new Error("bad JSON"); return state.data; }};
    },
  };
  vm.runInNewContext(source, context);
  return {chart, status, requests, intervals, listeners, document, state, timeouts,
    advance: ms => { now += ms; }};
}
function dataset(start, length, count = 1) {
  return {total: {lastYear: length * count}, contributions: Array.from({length}, (_, i) => ({
    date: new Date(Date.parse(`${start}T00:00:00Z`) + i * 86400000).toISOString().slice(0, 10), count, level: count ? 1 : 0,
  }))};
}

test("renders a representative fixture and exact returned date range without a hardcoded total", async () => {
  const h = harness(); await flush();
  assert.equal(h.chart.hidden, false);
  assert.equal(h.chart.children[0].name, "svg");
  assert.equal(h.chart.children[0].children.filter(x => x.name === "rect").length, sample.contributions.length + 5);
  assert.ok(h.status.textContent.startsWith(`${sample.total.lastYear.toLocaleString("en")} contributions`));
  assert.match(h.status.textContent, /Oct 12, 2025 to Oct 15, 2026/);
  assert.equal(h.requests[0].url, "https://github-contributions-api.jogruber.de/v4/mlozanoqf?y=last");
  assert.equal(h.requests[0].options.credentials, "omit");
  assert.equal(h.requests[0].options.referrerPolicy, "no-referrer");
  assert.equal(h.timeouts.size, 0);
});
test("refreshes data hourly and accepts new totals", async () => {
  const h = harness(dataset("2026-10-01", 14, 1)); await flush();
  assert.match(h.status.textContent, /^14 contributions/);
  h.state.data = dataset("2026-10-01", 15, 2);
  assert.equal(h.intervals[0].ms, 3600000);
  await h.intervals[0].fn();
  assert.match(h.status.textContent, /^30 contributions/);
  assert.equal(h.chart.children.length, 1);
});
test("skips background fetches and refreshes upon returning after an hour", async () => {
  const h = harness(sample, {hidden: true}); await flush();
  assert.equal(h.requests.length, 0);
  h.document.visibilityState = "visible"; h.listeners.visibilitychange(); await flush();
  assert.equal(h.requests.length, 1);
  h.document.visibilityState = "hidden"; await h.intervals[0].fn();
  assert.equal(h.requests.length, 1);
  h.advance(3600001); h.document.visibilityState = "visible"; h.listeners.visibilitychange(); await flush();
  assert.equal(h.requests.length, 2);
});
test("keeps fallback and handles HTTP failures, malformed JSON, and timeout", async () => {
  const h = harness({}); await flush();
  assert.equal(h.chart.children.length, 0); assert.match(h.status.textContent, /temporarily unavailable/);
  h.state.fail = true; await h.intervals[0].fn(); assert.equal(h.chart.children.length, 0);
  h.state.fail = false; h.state.invalidJSON = true; await h.intervals[0].fn(); assert.equal(h.chart.children.length, 0);
  h.state.invalidJSON = false; h.state.pending = true;
  const pending = h.intervals[0].fn();
  const timeout = Array.from(h.timeouts.values())[0]; assert.equal(timeout.ms, 12000);
  timeout.fn(); await pending; assert.equal(h.timeouts.size, 0);
});
test("preserves last good chart on failed refresh without duplicating warnings", async () => {
  const h = harness(); await flush(); const chart = h.chart.children[0];
  h.state.fail = true; await h.intervals[0].fn(); await h.intervals[0].fn();
  assert.equal(h.chart.children[0], chart);
  assert.equal(h.status.textContent.split("Refresh unavailable").length, 2);
});
test("validates dates, levels, counts, total, duplicates and gaps before drawing", async () => {
  const invalid = [
    {...sample, total: {lastYear: 0}},
    {total: {lastYear: 1}, contributions: [{date: "2026-02-30", count: 1, level: 1}]},
    {total: {lastYear: 1}, contributions: [{date: "2026-10-15", count: 1, level: 99}]},
    {total: {lastYear: 1}, contributions: [{date: "<img src=x onerror=alert(1)>", count: 1, level: 1}]},
    {total: {lastYear: -1}, contributions: [{date: "2026-10-15", count: -1, level: 1}]},
    {total: {lastYear: 2}, contributions: [sample.contributions[0], sample.contributions[0]]},
    {total: {lastYear: 0}, contributions: [sample.contributions[0], sample.contributions[2]]},
  ];
  for (const data of invalid) { const h = harness(data); await flush(); assert.equal(h.chart.children.length, 0); }
});
test("supports leap days, week offsets, changing years and empty activity", async () => {
  for (const data of [dataset("2024-02-28", 3), dataset("2025-12-28", 10), dataset("2026-10-01", 15, 0)]) {
    const h = harness(data); await flush(); assert.equal(h.chart.hidden, false);
    assert.equal(h.chart.children[0].children.filter(x => x.name === "rect").length, data.contributions.length + 5);
  }
});
test("does not initialize when the homepage elements are absent", async () => {
  const h = harness(sample, {missing: true}); await flush();
  assert.equal(h.requests.length, 0); assert.equal(h.intervals.length, 0);
});
