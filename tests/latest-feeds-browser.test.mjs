import test from "node:test";
import assert from "node:assert/strict";


test("updates both homepage feed links from the snapshot", async () => {
  const elements = {
    "latest-youtube-link": { textContent: "fallback", href: "" },
    "latest-youtube-date": { textContent: "" },
    "latest-substack-link": { textContent: "fallback", href: "" },
    "latest-substack-date": { textContent: "" },
  };
  globalThis.document = {
    readyState: "complete",
    getElementById: (id) => elements[id] ?? null,
  };
  globalThis.window = {
    setTimeout,
    clearTimeout,
  };
  globalThis.fetch = async () => ({
    ok: true,
    json: async () => ({
      youtube: {
        title: "New video",
        url: "https://www.youtube.com/watch?v=new",
        date: "Jul 14, 2026",
      },
      substack: {
        title: "New essay",
        url: "https://ahyaentendi.substack.com/p/new",
        date: "Jul 14, 2026",
      },
    }),
  });

  await import(`../latest-feeds.js?test=${Date.now()}`);
  await new Promise((resolve) => setImmediate(resolve));

  assert.equal(elements["latest-youtube-link"].textContent, "New video");
  assert.equal(
    elements["latest-youtube-link"].href,
    "https://www.youtube.com/watch?v=new",
  );
  assert.equal(elements["latest-youtube-date"].textContent, " (Jul 14, 2026)");
  assert.equal(elements["latest-substack-link"].textContent, "New essay");
  assert.equal(
    elements["latest-substack-link"].href,
    "https://ahyaentendi.substack.com/p/new",
  );
});
