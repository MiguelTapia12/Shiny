library(stringr)

tr <- read.csv("www/traducciones.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
existing_keys <- tr$key

extract_keys <- function(filepath) {
  content <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  content <- paste(content, collapse = " ")
  
  # Match data-i18n="<key>" or data-i18n='<key>'
  matches_html <- str_extract_all(content, 'data-i18n`?\\s*=\\s*["\']([^"\']+)["\']')[[1]]
  keys_html <- str_replace(matches_html, '.*["\']([^"\']+)["\'].*', '\\1')
  
  # Match tr("<key>", ...) or tr('<key>', ...)
  matches_tr <- str_extract_all(content, 'tr\\s*\\(\\s*["\']([^"\']+)["\']')[[1]]
  keys_tr <- str_replace(matches_tr, '.*["\']([^"\']+)["\'].*', '\\1')
  
  return(c(keys_html, keys_tr))
}

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
html_files <- list.files("www", pattern = "\\.html$", full.names = TRUE)
all_files <- c(r_files, html_files, "app.R")

all_keys <- c()
for (f in all_files) {
  all_keys <- c(all_keys, extract_keys(f))
}

all_keys <- unique(all_keys)
missing_keys <- setdiff(all_keys, existing_keys)

cat("Found", length(all_keys), "total keys in files.\n")
cat("Found", length(missing_keys), "missing keys not in traducciones.csv.\n")

if (length(missing_keys) > 0) {
  df_missing <- data.frame(
    key = missing_keys,
    es = missing_keys,
    en = missing_keys,
    stringsAsFactors = FALSE
  )
  write.csv(df_missing, "missing_translations.csv", row.names = FALSE, fileEncoding = "UTF-8")
  cat("Saved missing keys to missing_translations.csv\n")
}
