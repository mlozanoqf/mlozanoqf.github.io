#!/usr/bin/env node

import { readFile, writeFile, mkdir } from "node:fs/promises";
import { pathToFileURL } from "node:url";
import path from "node:path";


export const YOUTUBE_FEED =
  "https://www.youtube.com/feeds/videos.xml?channel_id=UChGz5VyXJdZOo1pquFyyOqw";
export const SUBSTACK_FEED = "https://ahyaentendi.substack.com/feed";

const MONTHS = [
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

function decodeEntities(value) {
  const named = {
    amp: "&",
    apos: "'",
    gt: ">",
    lt: "<",
    quot: '"',
  };
  return value
    .replace(/^<!\[CDATA\[([\s\S]*)\]\]>$/, "$1")
    .replace(/&#(x?[0-9a-f]+);|&([a-z]+);/gi, (match, numeric, name) => {
      if (numeric) {
        const radix = numeric[0].toLowerCase() === "x" ? 16 : 10;
        const digits = radix === 16 ? numeric.slice(1) : numeric;
        return String.fromCodePoint(Number.parseInt(digits, radix));
      }
      return named[name.toLowerCase()] ?? match;
    });
}

function cleanText(value = "") {
  return decodeEntities(value).replace(/<[^>]+>/g, " ").replace(/\s+/g, " ").trim();
}

function extractBlock(xml, tag) {
  const match = xml.match(new RegExp(`<${tag}\\b[^>]*>[\\s\\S]*?<\\/${tag}>`, "i"));
  return match?.[0] ?? "";
}

function extractTag(xml, tag) {
  const match = xml.match(new RegExp(`<${tag}\\b[^>]*>([\\s\\S]*?)<\\/${tag}>`, "i"));
  return cleanText(match?.[1] ?? "");
}

function extractAttribute(tag, attribute) {
  const match = tag.match(new RegExp(`${attribute}=["']([^"']+)["']`, "i"));
  return cleanText(match?.[1] ?? "");
}

function makeEntry(title, url, published) {
  const cleanTitle = cleanText(title);
  const cleanUrl = cleanText(url);
  const parsedUrl = new URL(cleanUrl);
  const publishedAt = new Date(published);
  if (!cleanTitle) throw new Error("feed entry has no title");
  if (parsedUrl.protocol !== "https:") throw new Error(`invalid URL: ${cleanUrl}`);
  if (Number.isNaN(publishedAt.valueOf())) throw new Error(`invalid date: ${published}`);
  return {
    title: cleanTitle,
    url: cleanUrl,
    published: publishedAt.toISOString().replace(".000Z", "Z"),
    date: `${MONTHS[publishedAt.getUTCMonth()]} ${String(publishedAt.getUTCDate()).padStart(2, "0")}, ${publishedAt.getUTCFullYear()}`,
  };
}

export function parseYouTube(xml) {
  const entry = extractBlock(xml, "entry");
  if (!entry) throw new Error("YouTube feed has no entries");
  const links = entry.match(/<link\b[^>]*>/gi) ?? [];
  const alternate = links.find((link) => {
    const rel = extractAttribute(link, "rel");
    return !rel || rel === "alternate";
  });
  return makeEntry(
    extractTag(entry, "title"),
    extractAttribute(alternate ?? "", "href"),
    extractTag(entry, "published") || extractTag(entry, "updated"),
  );
}

export function parseSubstack(xml) {
  const item = extractBlock(xml, "item");
  if (!item) throw new Error("Substack feed has no items");
  return makeEntry(
    extractTag(item, "title"),
    extractTag(item, "link"),
    extractTag(item, "pubDate"),
  );
}

async function fetchText(url, attempts = 3) {
  const errors = [];
  for (let attempt = 1; attempt <= attempts; attempt += 1) {
    try {
      const response = await fetch(url, {
        headers: {
          Accept: "application/atom+xml, application/rss+xml, application/xml, text/xml;q=0.9, */*;q=0.1",
          "Cache-Control": "no-cache",
          Pragma: "no-cache",
          "User-Agent": "Mozilla/5.0 (compatible; mlozanoqf-feed-refresh/2.0)",
        },
        signal: AbortSignal.timeout(30000),
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const body = await response.text();
      if (!body.trim()) throw new Error("empty response");
      return body;
    } catch (error) {
      errors.push(`attempt ${attempt}: ${error.message}`);
      if (attempt < attempts) {
        await new Promise((resolve) => setTimeout(resolve, attempt * 2000));
      }
    }
  }
  throw new Error(errors.join("; "));
}

function validEntry(value) {
  return value && typeof value === "object" &&
    ["title", "url", "published", "date"].every(
      (key) => typeof value[key] === "string" && value[key].trim(),
    );
}

async function loadSnapshot(file) {
  try {
    const value = JSON.parse(await readFile(file, "utf8"));
    return value && typeof value === "object" ? value : {};
  } catch {
    return {};
  }
}

export async function refreshSnapshot(existing, fetcher = fetchText) {
  const sources = [
    ["youtube", YOUTUBE_FEED, parseYouTube],
    ["substack", SUBSTACK_FEED, parseSubstack],
  ];
  const entries = {};
  const failures = [];
  let liveCount = 0;

  for (const [name, url, parser] of sources) {
    try {
      entries[name] = parser(await fetcher(url));
      liveCount += 1;
    } catch (error) {
      if (validEntry(existing[name])) {
        entries[name] = existing[name];
        failures.push(`${name}: ${error.message}; kept previous entry`);
      } else {
        failures.push(`${name}: ${error.message}; no previous entry available`);
      }
    }
  }

  const missing = ["youtube", "substack"].filter((name) => !validEntry(entries[name]));
  if (missing.length) throw new Error(`missing valid entries for: ${missing.join(", ")}`);
  if (liveCount === 0) throw new Error("neither live feed could be refreshed");

  const changed = ["youtube", "substack"].some(
    (name) => JSON.stringify(entries[name]) !== JSON.stringify(existing[name]),
  );
  return {
    snapshot: {
      schema_version: 1,
      updated_at: changed || !existing.updated_at ? new Date().toISOString() : existing.updated_at,
      youtube: entries.youtube,
      substack: entries.substack,
    },
    failures,
  };
}

async function writeSnapshot(snapshot, file) {
  const rendered = `${JSON.stringify(snapshot, null, 2)}\n`;
  let current = "";
  try {
    current = await readFile(file, "utf8");
  } catch {
    // The first run creates the file.
  }
  if (current === rendered) return;
  await mkdir(path.dirname(file), { recursive: true });
  await writeFile(file, rendered, "utf8");
}

function parseArguments(argv) {
  const values = {};
  for (let index = 0; index < argv.length; index += 2) {
    const name = argv[index];
    const value = argv[index + 1];
    if (!name?.startsWith("--") || !value) throw new Error("expected --existing and --output");
    values[name.slice(2)] = value;
  }
  if (!values.existing || !values.output) throw new Error("expected --existing and --output");
  return values;
}

async function main() {
  const args = parseArguments(process.argv.slice(2));
  const existing = await loadSnapshot(args.existing);
  const { snapshot, failures } = await refreshSnapshot(existing);
  await writeSnapshot(snapshot, args.output);
  for (const name of ["youtube", "substack"]) {
    const entry = snapshot[name];
    console.log(`${name}: ${entry.title} (${entry.date}) <${entry.url}>`);
  }
  for (const failure of failures) {
    console.log(`::warning title=Partial feed refresh::${failure}`);
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((error) => {
    console.error(`Feed refresh failed: ${error.message}`);
    process.exitCode = 1;
  });
}
