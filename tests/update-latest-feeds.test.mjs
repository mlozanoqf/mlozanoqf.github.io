import test from "node:test";
import assert from "node:assert/strict";

import {
  parseSubstack,
  parseYouTube,
  refreshSnapshot,
  SUBSTACK_FEED,
  YOUTUBE_FEED,
} from "../scripts/update-latest-feeds.mjs";


const youtubeXml = `<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <entry>
    <title>Latest video</title>
    <link rel="alternate" href="https://www.youtube.com/watch?v=abc" />
    <published>2026-07-14T07:59:53+00:00</published>
  </entry>
</feed>`;

const substackXml = `<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0"><channel><item>
  <title>Latest essay</title>
  <link>https://ahyaentendi.substack.com/p/latest-essay</link>
  <pubDate>Tue, 14 Jul 2026 07:49:21 GMT</pubDate>
</item></channel></rss>`;

test("parses the first YouTube Atom entry", () => {
  assert.deepEqual(parseYouTube(youtubeXml), {
    title: "Latest video",
    url: "https://www.youtube.com/watch?v=abc",
    published: "2026-07-14T07:59:53Z",
    date: "Jul 14, 2026",
  });
});

test("parses the first Substack RSS item", () => {
  assert.deepEqual(parseSubstack(substackXml), {
    title: "Latest essay",
    url: "https://ahyaentendi.substack.com/p/latest-essay",
    published: "2026-07-14T07:49:21Z",
    date: "Jul 14, 2026",
  });
});

test("one failed source does not block the other", async () => {
  const previousSubstack = {
    title: "Previous essay",
    url: "https://ahyaentendi.substack.com/p/previous",
    published: "2026-07-01T00:00:00Z",
    date: "Jul 01, 2026",
  };
  const fetcher = async (url) => {
    if (url === YOUTUBE_FEED) return youtubeXml;
    if (url === SUBSTACK_FEED) throw new Error("temporary outage");
    throw new Error("unexpected URL");
  };
  const { snapshot, failures } = await refreshSnapshot(
    { substack: previousSubstack },
    fetcher,
  );
  assert.equal(snapshot.youtube.title, "Latest video");
  assert.deepEqual(snapshot.substack, previousSubstack);
  assert.equal(failures.length, 1);
});
