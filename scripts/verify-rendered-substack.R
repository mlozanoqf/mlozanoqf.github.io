#!/usr/bin/env Rscript

cache_path <- file.path("data", "latest-feeds.R")
html_path <- "index.html"

if (!file.exists(cache_path)) {
  stop("Missing feed cache: ", cache_path, call. = FALSE)
}
if (!file.exists(html_path)) {
  stop("Missing rendered homepage: ", html_path, call. = FALSE)
}

cache <- tryCatch(dget(cache_path), error = function(e) NULL)
if (is.null(cache) || is.null(cache$substack)) {
  stop("Feed cache does not contain a Substack entry.", call. = FALSE)
}

substack <- cache$substack
title <- if (!is.null(substack$title)) as.character(substack$title)[1] else ""
link <- if (!is.null(substack$link)) as.character(substack$link)[1] else ""
date <- if (!is.null(substack$date)) as.character(substack$date)[1] else ""

if (!nzchar(title) || !nzchar(link)) {
  stop("Cached Substack entry is incomplete.", call. = FALSE)
}

html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (!grepl(link, html, fixed = TRUE)) {
  stop("Rendered homepage does not contain the cached Substack link: ", link, call. = FALSE)
}
if (!grepl(title, html, fixed = TRUE)) {
  stop("Rendered homepage does not contain the cached Substack title: ", title, call. = FALSE)
}

cat(sprintf("Rendered Substack verified: %s (%s) <%s>\n", title, date, link))
