#!/usr/bin/env Rscript

cache_path <- file.path("data", "latest-feeds.R")
api_url <- "https://ahyaentendi.substack.com/api/v1/archive?sort=new"
feed_home <- "https://ahyaentendi.substack.com"

parse_feed_datetime <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(as.POSIXct(NA))
  x2 <- trimws(as.character(x)[1])
  x2 <- sub("\\.\\d+(?=Z$)", "", x2, perl = TRUE)
  x2 <- sub("Z$", "+0000", x2)
  x2 <- sub(" GMT$", " +0000", x2)

  patterns <- list(
    list(fmt = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    list(fmt = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC"),
    list(fmt = "%a, %d %b %Y %H:%M:%S %Z", tz = "UTC"),
    list(fmt = "%b %d, %Y", tz = "UTC")
  )

  for (pat in patterns) {
    dt <- suppressWarnings(as.POSIXct(x2, format = pat$fmt, tz = pat$tz))
    if (!is.na(dt)) return(dt)
  }

  suppressWarnings(as.POSIXct(NA))
}

format_feed_date <- function(x) {
  dt <- parse_feed_datetime(x)
  if (is.na(dt)) return("")
  old_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(try(Sys.setlocale("LC_TIME", old_lc_time), silent = TRUE), add = TRUE)
  try(Sys.setlocale("LC_TIME", "C"), silent = TRUE)
  format(dt, "%b %d, %Y")
}

normalize_entry <- function(entry, fallback_title = "", fallback_url = "") {
  out <- list(
    title = fallback_title,
    link = fallback_url,
    date = "",
    ok = FALSE,
    source = "fallback"
  )

  if (is.null(entry) || !is.list(entry)) return(out)

  for (nm in intersect(names(out), names(entry))) {
    out[[nm]] <- entry[[nm]]
  }

  out$title <- if (!is.null(out$title)) as.character(out$title)[1] else fallback_title
  out$link <- if (!is.null(out$link)) as.character(out$link)[1] else fallback_url
  out$date <- if (!is.null(out$date)) as.character(out$date)[1] else ""
  out$ok <- isTRUE(out$ok)
  out$source <- if (!is.null(out$source) && nzchar(out$source)) {
    as.character(out$source)[1]
  } else if (isTRUE(out$ok)) {
    "cache"
  } else {
    "fallback"
  }

  out
}

same_entry_content <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)
  a <- normalize_entry(a)
  b <- normalize_entry(b)
  identical(
    list(title = a$title, link = a$link, date = a$date, ok = a$ok),
    list(title = b$title, link = b$link, date = b$date, ok = b$ok)
  )
}

read_feed_cache <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(dget(path), error = function(e) NULL)
}

ensure_cache_shape <- function(cache) {
  if (is.null(cache) || !is.list(cache)) return(NULL)
  list(
    updated_at = if (!is.null(cache$updated_at)) as.character(cache$updated_at)[1] else "",
    youtube = if (!is.null(cache$youtube)) normalize_entry(cache$youtube) else NULL,
    substack = if (!is.null(cache$substack)) normalize_entry(cache$substack) else NULL
  )
}

add_cache_buster <- function(url) {
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  paste0(url, sep, "cb=", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))
}

fetch_json_text <- function(url, attempts = 5, delay = 12) {
  errors <- character()

  for (attempt in seq_len(attempts)) {
    request_url <- add_cache_buster(url)
    txt <- tryCatch({
      if (nzchar(Sys.which("curl"))) {
        out <- system2(
          "curl",
          c(
            "-fL",
            "-sS",
            "--compressed",
            "--connect-timeout",
            "20",
            "--max-time",
            "60",
            "-A",
            "mlozanoqf-feed-refresh/1.0",
            "-H",
            "Cache-Control:no-cache,no-store,max-age=0",
            "-H",
            "Pragma:no-cache",
            request_url
          ),
          stdout = TRUE,
          stderr = TRUE
        )
        status <- attr(out, "status")
        if (!is.null(status) && status != 0) {
          stop(paste(out, collapse = "\n"), call. = FALSE)
        }
        paste(out, collapse = "\n")
      } else {
        paste(readLines(request_url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      }
    }, error = function(e) {
      errors <<- c(errors, sprintf("attempt %d: %s", attempt, conditionMessage(e)))
      ""
    })

    if (nzchar(txt) && grepl("^\\s*[\\[{]", txt, perl = TRUE)) {
      return(txt)
    }

    if (nzchar(txt)) {
      errors <- c(errors, sprintf("attempt %d: response was not JSON (%d bytes)", attempt, nchar(txt)))
    }

    if (attempt < attempts) Sys.sleep(delay)
  }

  stop(
    "Could not fetch a valid Substack API response after retries:\n",
    paste(utils::tail(errors, 8), collapse = "\n"),
    call. = FALSE
  )
}

parse_substack_api <- function(url) {
  json_txt <- fetch_json_text(url)

  decode_json_string <- function(x) {
    if (!nzchar(x)) return("")
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      decoded <- tryCatch(jsonlite::fromJSON(paste0("\"", x, "\"")), error = function(e) NULL)
      if (!is.null(decoded)) return(as.character(decoded)[1])
    }

    x <- gsub("\\\\/", "/", x, fixed = TRUE)
    x <- gsub("\\\\\"", "\"", x, fixed = TRUE)
    x <- gsub("\\\\n", "\n", x, fixed = TRUE)
    x <- gsub("\\\\r", "\r", x, fixed = TRUE)
    x <- gsub("\\\\t", "\t", x, fixed = TRUE)
    x
  }

  extract_json_string <- function(key) {
    pattern <- paste0("\"", key, "\"\\s*:\\s*\"((?:\\\\.|[^\"\\\\])*)\"")
    hit <- regmatches(json_txt, regexec(pattern, json_txt, perl = TRUE))[[1]]
    if (length(hit) < 2) return("")
    trimws(decode_json_string(hit[2]))
  }

  title <- extract_json_string("title")
  slug <- extract_json_string("slug")
  link <- extract_json_string("canonical_url")
  date_txt <- extract_json_string("post_date")

  if ((grepl("substack\\.com/home/post/", link) || !nzchar(link)) && nzchar(slug)) {
    link <- paste0(feed_home, "/p/", slug)
  }

  date <- format_feed_date(date_txt)
  if (!nzchar(title) || !nzchar(link) || !nzchar(date)) {
    stop("Substack API latest entry is incomplete; refusing to publish stale cache.", call. = FALSE)
  }

  list(title = title, link = link, date = date, ok = TRUE, source = "live")
}

cache <- ensure_cache_shape(read_feed_cache(cache_path))
existing_substack <- if (!is.null(cache)) cache$substack else NULL
existing_youtube <- if (!is.null(cache)) cache$youtube else NULL

latest_substack <- parse_substack_api(api_url)

cat(
  sprintf(
    "Substack API latest: %s (%s) <%s>\n",
    latest_substack$title,
    latest_substack$date,
    latest_substack$link
  )
)

if (!is.null(existing_substack)) {
  cat(
    sprintf(
      "Cached Substack before refresh: %s (%s) <%s>\n",
      existing_substack$title,
      existing_substack$date,
      existing_substack$link
    )
  )
}

changed <- !same_entry_content(latest_substack, existing_substack)
updated_at <- if (!is.null(cache) && nzchar(cache$updated_at)) cache$updated_at else ""
if (changed || !file.exists(cache_path)) {
  updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

next_cache <- list(
  updated_at = updated_at,
  youtube = existing_youtube,
  substack = latest_substack
)

if (!dir.exists(dirname(cache_path))) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
}

if (changed || !file.exists(cache_path)) {
  dput(next_cache, cache_path)
  cat(sprintf("Updated %s with the latest Substack essay.\n", cache_path))
} else {
  cat(sprintf("%s already has the latest Substack essay.\n", cache_path))
}
