build_site <- function(
    solutions_dir = "solutions",
    out_dir = "docs",
    max_id = 900,
    force = FALSE,
    force_ids = integer(0)  # fx c(1, 17) for kun at tvinge bestemte filer
) {
  stopifnot(dir.exists(solutions_dir))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  fmt_id <- function(i) sprintf("%03d", i)
  
  # Knit step (skipper når HTML er nyere end Rmd – med mindre force/force_ids)
  for (i in seq_len(max_id)) {
    id <- fmt_id(i)
    rmd <- file.path(solutions_dir, paste0(id, ".Rmd"))
    if (!file.exists(rmd)) next
    
    html <- file.path(out_dir, paste0(id, ".html"))
    
    # Skal vi rende?
    doit <- FALSE
    if (force || i %in% force_ids) {
      doit <- TRUE
    } else if (!file.exists(html)) {
      doit <- TRUE
    } else {
      mt_rmd  <- file.info(rmd)$mtime
      mt_html <- file.info(html)$mtime
      if (is.na(mt_html) || mt_rmd > mt_html) doit <- TRUE
    }
    
    if (doit) {
      message("Rendering ", basename(rmd), " -> ", basename(html))
      rmarkdown::render(
        input       = rmd,
        output_format = "html_document",
        output_file = paste0(id, ".html"),
        output_dir  = out_dir,
        quiet       = TRUE
      )
    } else {
      message("Up-to-date: ", basename(html))
    }
  }
  
  # Index (links for eksisterende, “mangler” for resten)
  existing <- sprintf("%03d", as.integer(gsub("\\.html$", "", list.files(out_dir, pattern = "^[0-9]{3}\\.html$"))))
  has_html <- setNames(rep(FALSE, max_id), sprintf("%03d", seq_len(max_id)))
  has_html[existing] <- TRUE
  
  # Lille, ren index.html
  idx_path <- file.path(out_dir, "index.html")
  items <- vapply(seq_len(max_id), function(i) {
    id <- fmt_id(i)
    if (isTRUE(has_html[id])) {
      sprintf('<a class="card" href="%s.html"><span class="id">%s</span><span class="status ok">link</span></a>', id, id)
    } else {
      sprintf('<div class="card missing"><span class="id">%s</span><span class="status no">mangler</span></div>', id)
    }
  }, character(1))
  
  html <- paste0(
    '<!doctype html>
<html lang="da">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Solutions</title>
<style>
  body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 24px; }
  h1 { margin-bottom: 12px; }
  .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(90px, 1fr)); gap: 10px; }
  .card { display: flex; flex-direction: column; align-items: center; justify-content: center;
          padding: 10px; border: 1px solid #ddd; border-radius: 10px; text-decoration: none; }
  .card:hover { box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
  .card .id { font-weight: 600; margin-bottom: 4px; }
  .card .status { font-size: 12px; opacity: 0.8; }
  .missing { color: #888; background: #f8f8f8; }
  .ok { }
  .no { }
  .legend { margin: 8px 0 16px 0; color: #555; font-size: 14px; }
</style>
</head>
<body>
<h1>Solutions (001–', max_id, ')</h1>
<div class="legend">Links findes for genererede filer; resten vises som “mangler”.</div>
<div class="grid">
', paste(items, collapse = "\n"), '
</div>
</body>
</html>'
  )
  
  writeLines(html, idx_path)
  message("Skrev index: ", idx_path)
}

# Eksempler:
# build_site()                       # knit kun dem der mangler/er forældede og skriv index
# build_site(force = TRUE)           # knit alt forfra
# build_site(force_ids = c(1, 42))   # knit kun 001 og 042 forfra
