(() => {
  "use strict";

  const chart = document.getElementById("github-contributions-chart");
  const status = document.getElementById("github-contributions-status");
  if (!chart || !status) return;

  const endpoint = "https://github-contributions-api.jogruber.de/v4/mlozanoqf?y=last";
  const refreshMs = 60 * 60 * 1000;
  const dayMs = 24 * 60 * 60 * 1000;
  const svgNS = "http://www.w3.org/2000/svg";
  const colors = ["#ebedf0", "#9be9a8", "#40c463", "#30a14e", "#216e39"];
  const dateFormat = new Intl.DateTimeFormat("en", {
    month: "short", day: "numeric", year: "numeric", timeZone: "UTC",
  });
  const monthFormat = new Intl.DateTimeFormat("en", { month: "short", timeZone: "UTC" });
  let loading = false;
  let hasChart = false;
  let lastAttempt = 0;

  function validate(data) {
    if (!data || !Array.isArray(data.contributions) ||
        data.contributions.length < 1 || data.contributions.length > 371 ||
        !Number.isSafeInteger(data.total?.lastYear) || data.total.lastYear < 0) {
      throw new Error("Invalid contribution data");
    }
    const days = data.contributions.map((day) => {
      if (!day || !/^\d{4}-\d{2}-\d{2}$/.test(day.date) ||
          !Number.isSafeInteger(day.count) || day.count < 0 ||
          !Number.isInteger(day.level) || day.level < 0 || day.level > 4) {
        throw new Error("Invalid contribution day");
      }
      const date = new Date(`${day.date}T00:00:00Z`);
      if (!Number.isFinite(date.getTime()) || date.toISOString().slice(0, 10) !== day.date) {
        throw new Error("Invalid contribution date");
      }
      return { date, count: day.count, level: day.level };
    }).sort((a, b) => a.date - b.date);
    for (let i = 1; i < days.length; i += 1) {
      if (days[i].date - days[i - 1].date !== dayMs) {
        throw new Error("Non-contiguous contribution dates");
      }
    }
    const total = days.reduce((sum, day) => sum + day.count, 0);
    if (!Number.isSafeInteger(total) || total !== data.total.lastYear) {
      throw new Error("Inconsistent contribution total");
    }
    return { days, total };
  }

  function svgElement(name, attributes, text) {
    const element = document.createElementNS(svgNS, name);
    for (const [key, value] of Object.entries(attributes || {})) {
      element.setAttribute(key, String(value));
    }
    if (text !== undefined) element.textContent = text;
    return element;
  }

  function render({ days, total }) {
    const first = days[0].date;
    const last = days[days.length - 1].date;
    const offset = first.getUTCDay();
    const weeks = Math.ceil((offset + days.length) / 7);
    const width = 56 + weeks * 14;
    const summary = `${total.toLocaleString("en")} contributions · ${dateFormat.format(first)} to ${dateFormat.format(last)}`;
    const svg = svgElement("svg", {
      viewBox: `0 0 ${width} 152`, role: "img",
      "aria-labelledby": "github-calendar-title github-calendar-description",
    });
    svg.appendChild(svgElement("title", { id: "github-calendar-title" }, summary));
    svg.appendChild(svgElement("desc", { id: "github-calendar-description" },
      "Daily GitHub contributions for mlozanoqf. Darker green means more contributions. Each square has a date and count."));

    for (const [row, label] of [[1, "Mon"], [3, "Wed"], [5, "Fri"]]) {
      svg.appendChild(svgElement("text", { x: 0, y: 34 + row * 14, class: "qf-github-label" }, label));
    }
    let previousMonth = -1;
    days.forEach((day, index) => {
      const column = Math.floor((offset + index) / 7);
      const row = day.date.getUTCDay();
      if (day.date.getUTCMonth() !== previousMonth) {
        // The last partial month can share a column with the preceding label.
        if (index === 0 || column > 1) {
          svg.appendChild(svgElement("text", {
            x: 36 + column * 14, y: 13, class: "qf-github-label",
          }, monthFormat.format(day.date)));
        }
        previousMonth = day.date.getUTCMonth();
      }
      const square = svgElement("rect", {
        x: 36 + column * 14, y: 25 + row * 14, width: 10, height: 10,
        rx: 2, fill: colors[day.level], "data-date": day.date.toISOString().slice(0, 10),
      });
      square.appendChild(svgElement("title", {},
        `${day.count} ${day.count === 1 ? "contribution" : "contributions"} on ${dateFormat.format(day.date)}`));
      svg.appendChild(square);
    });
    svg.appendChild(svgElement("text", { x: width - 126, y: 144, class: "qf-github-label" }, "Less"));
    colors.forEach((fill, index) => svg.appendChild(svgElement("rect", {
      x: width - 98 + index * 14, y: 135, width: 10, height: 10, rx: 2, fill,
    })));
    svg.appendChild(svgElement("text", { x: width - 25, y: 144, class: "qf-github-label" }, "More"));
    chart.replaceChildren(svg);
    chart.hidden = false;
    status.textContent = summary;
    hasChart = true;
  }

  async function refresh() {
    if (loading || document.visibilityState === "hidden") return;
    loading = true;
    lastAttempt = Date.now();
    const controller = new AbortController();
    const timeout = window.setTimeout(() => controller.abort(), 12000);
    try {
      const response = await fetch(endpoint, {
        signal: controller.signal, credentials: "omit", referrerPolicy: "no-referrer",
      });
      if (!response.ok) throw new Error("Contribution data is unavailable");
      render(validate(await response.json()));
    } catch {
      if (hasChart) {
        status.textContent = `${status.textContent.replace(/ · Refresh unavailable.*$/, "")} · Refresh unavailable; showing the last loaded data.`;
      } else {
        status.textContent = "The contribution calendar is temporarily unavailable. View the latest activity on GitHub.";
      }
    } finally {
      window.clearTimeout(timeout);
      loading = false;
    }
  }

  refresh();
  window.setInterval(refresh, refreshMs);
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible" && Date.now() - lastAttempt >= refreshMs) refresh();
  });
})();
