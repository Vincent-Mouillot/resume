# ============================================================
# cv_build.R
# Generates the CV as HTML + PDF directly from R
# Usage: source("cv_build.R")
# ============================================================

library(yaml)
library(glue)
library(pagedown)
library(base64enc)

# ---- Config ------------------------------------------------
YAML_FILE  <- "resume_data.yaml"
OUTPUT_DIR <- "output"
PHOTO_FILE <- "photo.jpg"   # set to NULL if no photo: PHOTO_FILE <- NULL

dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ---- Load data ---------------------------------------------
cv <- yaml::read_yaml(YAML_FILE)

# ---- Helpers -----------------------------------------------

# Extract the correct language field
# If the field has fr/en sub-keys, return the right one
# Otherwise return the field as-is
t <- function(field, lang) {
  if (is.null(field)) return("")
  if (is.list(field) && !is.null(field[[lang]])) return(field[[lang]])
  if (is.character(field)) return(paste(field, collapse = " "))
  return("")
}

# Format a date range (e.g. "2021 – present")
fmt_period <- function(start, end, present_label) {
  end_str <- if (is.null(end)) present_label else as.character(end)
  paste0(start, " \u2013 ", end_str)
}

# Convert markdown-style bullet lines into an HTML <ul>
to_li <- function(text) {
  lines <- strsplit(trimws(text), "\n")[[1]]
  lines <- lines[nchar(trimws(lines)) > 0]
  
  # Merge continuation lines (no leading dash) with the previous line
  merged <- c()
  for (line in lines) {
    if (grepl("^\\s*[-*] ", line)) {
      merged <- c(merged, line)          # new bullet
    } else {
      # continuation: append to last item
      merged[length(merged)] <- paste(merged[length(merged)], trimws(line))
    }
  }
  
  items <- sapply(merged, function(line) {
    if (grepl("^  [-*] ", line)) {
      content <- gsub("^\\s*[-*] ?", "", line)
      glue("<li class='sub-item'>{content}</li>")
    } else {
      content <- gsub("^[-*] ?", "", trimws(line))
      glue("<li>{content}</li>")
    }
  })
  
  paste0("<ul>", paste(items, collapse = ""), "</ul>")
}

# Encode a local image file to a base64 data URI
# This embeds the image directly in the HTML so chrome_print works without
# needing to resolve external file paths
encode_photo <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  ext      <- tolower(tools::file_ext(path))
  mime     <- switch(ext, jpg = , jpeg = "image/jpeg", png = "image/png", "image/jpeg")
  b64      <- base64enc::base64encode(path)
  glue('data:{mime};base64,{b64}')
}

# ---- Main HTML builder -------------------------------------
build_html <- function(lang) {

  # UI labels for each language
  labels <- list(
    fr = list(
      skills         = "Comp\u00e9tences",
      experience     = "Exp\u00e9riences professionnelles",
      education      = "Formation",
      projects       = "Projets",
      certifications = "Certifications",
      present        = "pr\u00e9sent"
    ),
    en = list(
      skills         = "Skills",
      experience     = "Work Experience",
      education      = "Education",
      projects       = "Projects",
      certifications = "Certifications",
      present        = "present"
    )
  )[[lang]]

  # Shorthand: translate field for current lang
  tl <- function(field) t(field, lang)

  # -- Photo (optional)
  photo_html <- ""
  photo_src  <- encode_photo(PHOTO_FILE)
  if (!is.null(photo_src)) {
    photo_html <- glue('<img src="{photo_src}" class="cv-photo" alt="photo">')
  }

  # -- Header
  header <- glue('
<div class="cv-header">
  <div class="cv-header-left">
    <h1>{cv$meta$name}</h1>
    <div class="cv-job-title">{tl(cv$meta$title)}</div>
    <div class="cv-summary">{tl(cv$meta$summary)}</div>
  </div>
  <div class="cv-header-right">
    {photo_html}
    <ul class="cv-contact">
      <li><a href="mailto:{cv$meta$email}">{cv$meta$email}</a></li>
      <li>{cv$meta$phone}</li>
      <li>{cv$meta$location}</li>
      <li><a href="https://{cv$meta$linkedin}">{cv$meta$linkedin}</a></li>
      <li><a href="https://{cv$meta$github}">{cv$meta$github}</a></li>
    </ul>
  </div>
</div>')

  # -- Skills
  skills_items <- paste(sapply(cv$skills, function(sg) {
    glue('<div class="skill-group"><span class="skill-cat">{tl(sg$category)} :</span> {paste(sg$items, collapse = " \u00b7 ")}</div>')
  }), collapse = "\n")
  skills <- glue('
<div class="cv-section">
  <div class="cv-section-title">{labels$skills}</div>
  <div class="cv-skills-grid">{skills_items}</div>
</div>')

  # -- Work experience
  exp_items <- paste(sapply(cv$experience, function(exp) {
    glue('
<div class="cv-entry">
  <div class="cv-entry-header">
    <div class="cv-entry-left">
      <span class="cv-entry-title">{tl(exp$title)}</span>
      <span class="cv-entry-org">{exp$company} \u2014 {exp$location}</span>
    </div>
    <span class="cv-entry-period">{fmt_period(exp$start, exp$end, labels$present)}</span>
  </div>
  <div class="cv-entry-body">{to_li(tl(exp$description))}</div>
</div>')
  }), collapse = "\n")
  experience <- glue('<div class="cv-section"><div class="cv-section-title">{labels$experience}</div>{exp_items}</div>')

  # -- Education
  edu_items <- paste(sapply(cv$education, function(edu) {
    note <- if (!is.null(edu$note) && nchar(tl(edu$note)) > 0)
      glue('<div class="cv-note">{tl(edu$note)}</div>') else ""
    glue('
<div class="cv-entry">
  <div class="cv-entry-header">
    <div class="cv-entry-left">
      <span class="cv-entry-title">{tl(edu$degree)}</span>
      <span class="cv-entry-org">{edu$institution} \u2014 {edu$location}</span>
    </div>
    <span class="cv-entry-period">{fmt_period(edu$start, edu$end, labels$present)}</span>
  </div>
  {note}
</div>')
  }), collapse = "\n")
  education <- glue('<div class="cv-section"><div class="cv-section-title">{labels$education}</div>{edu_items}</div>')

  # -- Projects
  proj_items <- paste(sapply(cv$projects, function(proj) {
    url_html  <- if (!is.null(proj$url))
      glue('<a class="cv-project-url" href="https://{proj$url}">{proj$url}</a>') else ""
    tags_html <- if (!is.null(proj$tags))
      glue('<div class="cv-tags">{paste(proj$tags, collapse = " \u00b7 ")}</div>') else ""
    glue('
<div class="cv-entry">
  <div class="cv-entry-header">
    <div class="cv-entry-left">
      <span class="cv-entry-title">{proj$title}</span>
      {url_html}
    </div>
    <span class="cv-entry-period">{fmt_period(proj$start, proj$end, labels$present)}</span>
  </div>
  <div class="cv-entry-body"><p>{trimws(tl(proj$description))}</p></div>
  {tags_html}
</div>')
  }), collapse = "\n")
  projects <- glue('<div class="cv-section"><div class="cv-section-title">{labels$projects}</div>{proj_items}</div>')

  # -- Certifications
  cert_items <- paste(sapply(cv$certifications, function(cert) {
    glue('
<div class="cv-cert">
  <span class="cv-cert-name">{cert$name}</span>
  <span class="cv-cert-meta">{cert$issuer} \u00b7 {cert$date}</span>
</div>')
  }), collapse = "\n")
  certifications <- glue('<div class="cv-section"><div class="cv-section-title">{labels$certifications}</div><div class="cv-certs-grid">{cert_items}</div></div>')

  # -- Embed CSS directly in the HTML so chrome_print doesn't need
  #    to resolve any external file paths
  css_content <- paste(readLines("custom.css", encoding = "UTF-8"), collapse = "\n")

  # -- Assemble full HTML document
  glue('<!DOCTYPE html>
<html lang="{lang}">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{cv$meta$name}</title>
  <style>{css_content}</style>
</head>
<body>
{header}
{skills}
{experience}
{education}
{projects}
</body>
</html>')
}

# ---- Generate FR and EN versions ---------------------------
for (lang in c("fr", "en")) {
  html_content <- build_html(lang)
  html_file    <- file.path(OUTPUT_DIR, paste0("cv_", lang, ".html"))
  pdf_file     <- file.path(OUTPUT_DIR, paste0("cv_", lang, ".pdf"))

  writeLines(html_content, html_file, useBytes = TRUE)
  message(glue("OK : {html_file}"))

  pagedown::chrome_print(input = html_file, output = pdf_file)
  message(glue("OK : {pdf_file}"))
}

message("\nDone! Files are in /output")
