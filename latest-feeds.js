(() => {
  const endpoint =
    "https://raw.githubusercontent.com/mlozanoqf/mlozanoqf.github.io/feed-data/data/latest-feeds.json";

  const targets = {
    youtube: {
      link: "latest-youtube-link",
      date: "latest-youtube-date",
    },
    substack: {
      link: "latest-substack-link",
      date: "latest-substack-date",
    },
  };

  function isValidEntry(entry) {
    if (!entry || typeof entry !== "object") return false;
    if (!entry.title || !entry.url || !entry.date) return false;
    try {
      return new URL(entry.url).protocol === "https:";
    } catch {
      return false;
    }
  }

  function renderEntry(name, entry) {
    if (!isValidEntry(entry)) return;
    const target = targets[name];
    const link = document.getElementById(target.link);
    const date = document.getElementById(target.date);
    if (!link || !date) return;
    link.textContent = entry.title;
    link.href = entry.url;
    date.textContent = ` (${entry.date})`;
  }

  async function refreshLatestFeeds() {
    const controller = new AbortController();
    const timeout = window.setTimeout(() => controller.abort(), 8000);
    const cacheWindow = Math.floor(Date.now() / 300000);

    try {
      const response = await fetch(`${endpoint}?v=${cacheWindow}`, {
        cache: "no-store",
        signal: controller.signal,
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const snapshot = await response.json();
      renderEntry("youtube", snapshot.youtube);
      renderEntry("substack", snapshot.substack);
    } catch (error) {
      console.warn("Latest feeds unavailable; using embedded fallback.", error);
    } finally {
      window.clearTimeout(timeout);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", refreshLatestFeeds, { once: true });
  } else {
    refreshLatestFeeds();
  }
})();
