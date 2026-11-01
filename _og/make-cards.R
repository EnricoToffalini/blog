# Pre-render: create the link-preview card of each post (<name>.og.png next
# to the post) from post-card.html, as the post looks at the top of its page.
# Only missing cards are created: delete a card to redo it (e.g. new title).
# Needs the webshot2 package and Chrome; without them posts use the blog card.

if (!requireNamespace("webshot2", quietly = TRUE) || !requireNamespace("yaml", quietly = TRUE)) {
  message("og cards: install.packages(c('webshot2', 'yaml')) to create post preview cards")
  quit(save = "no", status = 0)
}

mesi = c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio",
         "agosto","settembre","ottobre","novembre","dicembre")
template = paste(readLines("_og/post-card.html", encoding = "UTF-8"), collapse = "\n")

esc = function(x) {
  x = gsub("&", "&amp;", x, fixed = TRUE)
  x = gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
# typographic quotes, as on the page
smart = function(x) {
  x = gsub("\"([^\"]*)\"", "“\\1”", enc2utf8(x), useBytes = TRUE)
  x = gsub("'", "’", x, fixed = TRUE, useBytes = TRUE)
  Encoding(x) = "UTF-8"
  x
}

posts = list.files("posts", pattern = "\\.[qQ]md$", recursive = TRUE, full.names = TRUE)
for (f in posts) {
  out = sub("\\.[qQ]md$", ".og.png", f)
  if (file.exists(out)) next
  lines = readLines(f, encoding = "UTF-8", warn = FALSE)
  delim = which(trimws(lines) == "---")
  if (length(delim) < 2 || delim[1] != 1) next
  meta = yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  if (is.null(meta$title)) next
  authors = unlist(meta$author)
  d = as.Date(as.character(meta$date))
  fill = c(
    title = esc(smart(meta$title)),
    subtitle = if (is.null(meta$subtitle)) "" else paste0('<div class="sub">', esc(smart(meta$subtitle)), "</div>"),
    authors_label = if (length(authors) > 1) "Autori" else "Autore/Autrice",
    authors = paste(esc(authors), collapse = "<br>"),
    date = if (is.na(d)) "" else paste(as.integer(format(d, "%d")), mesi[as.integer(format(d, "%m"))], format(d, "%Y"))
  )
  html = template
  for (k in names(fill)) html = gsub(paste0("{{", k, "}}"), fill[[k]], html, fixed = TRUE)
  tmp = tempfile(fileext = ".html")
  writeLines(html, tmp, useBytes = TRUE)
  ok = tryCatch({
    webshot2::webshot(tmp, out, vwidth = 1200, vheight = 630, cliprect = "viewport", delay = 1.5, quiet = TRUE)
    TRUE
  }, error = function(e) { message("og cards: ", conditionMessage(e)); FALSE })
  if (ok) message("og cards: created ", out)
}
