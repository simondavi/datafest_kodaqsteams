# prepare_bot_data.R
# Read enwiki edit data, parse features, export CSV for bot_prediction.py

library(jsonlite)

N_LINES <- 10000
setwd("C:/Users/Lenovo/Downloads/DataFest2026/DataFest2026/data")

#load
con    <- textConnection(readLines(gzfile("edit_types/enwiki.json.gz"), n = N_LINES))
enwiki <- stream_in(con, verbose = FALSE)
close(con)

#features
has_tag <- function(tags, pattern) {
  sapply(tags, function(t) as.integer(any(grepl(pattern, t, fixed = TRUE))))
}

enwiki$tool_mobile  <- has_tag(enwiki$revision_tags, "mobile edit")
enwiki$tool_visual  <- has_tag(enwiki$revision_tags, "visualeditor")
enwiki$tool_wiki    <- has_tag(enwiki$revision_tags, "wikieditor")
enwiki$is_reverted  <- as.integer(
  has_tag(enwiki$revision_tags, "mw-reverted") |
  has_tag(enwiki$revision_tags, "mw-rollback")  |
  has_tag(enwiki$revision_tags, "mw-undo")
)
enwiki$comment_length <- nchar(ifelse(is.na(enwiki$revision_comment), "", enwiki$revision_comment))

is_ip <- function(x) {
  grepl("^[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+$", x) |
  (grepl(":", x, fixed = TRUE) & grepl("[0-9a-fA-F]", x))
}
enwiki$is_anon <- as.integer(is.na(enwiki$user_text) | is_ip(enwiki$user_text))

#parse edit types
parse_edit <- function(json_str) {
  tryCatch({
    p          <- fromJSON(json_str, simplifyVector = FALSE)
    node_edits <- p[["node-edits"]]
    text_edits <- p[["text-edits"]]
    node_types   <- unlist(lapply(node_edits, function(e) e[[1]]))
    node_actions <- unlist(lapply(node_edits, function(e) e[[2]]))
    text_actions <- unlist(lapply(text_edits, function(e) e[[2]]))
    text_counts  <- unlist(lapply(text_edits, function(e) {
      if (length(e) >= 4 && is.numeric(e[[4]])) e[[4]] else 1L
    }))
    data.frame(
      n_node_edits     = length(node_edits),
      n_references     = sum(node_types == "Reference"),
      n_templates      = sum(node_types == "Template"),
      n_wikilinks      = sum(node_types == "Wikilink"),
      n_ext_links      = sum(node_types == "ExternalLink"),
      n_tables         = sum(node_types == "Table"),
      n_headings       = sum(node_types == "Heading"),
      n_words_inserted = sum(text_counts[text_actions == "insert"], na.rm = TRUE),
      n_words_removed  = sum(text_counts[text_actions == "remove"],  na.rm = TRUE),
      n_text_edits     = length(text_edits)
    )
  }, error = function(e) {
    data.frame(n_node_edits=NA, n_references=NA, n_templates=NA, n_wikilinks=NA,
               n_ext_links=NA, n_tables=NA, n_headings=NA,
               n_words_inserted=NA, n_words_removed=NA, n_text_edits=NA)
  })
}

parsed_df <- do.call(rbind, lapply(enwiki$edit_types_json, parse_edit))
enwiki    <- cbind(enwiki, parsed_df)

#feature table build
features <- enwiki[, c(
  "n_node_edits", "n_references", "n_templates", "n_wikilinks",
  "n_ext_links", "n_tables", "n_headings",
  "n_words_inserted", "n_words_removed", "n_text_edits",
  "tool_mobile", "tool_visual", "tool_wiki",
  "is_reverted", "comment_length", "is_anon",
  "is_bot"   # target variable — kept in same file, split in Python
)]

#drop rows with any NA (failed JSON parse)
features <- features[complete.cases(features), ]
features$is_bot <- as.integer(features$is_bot)


write.csv(features, "../bot_features.csv", row.names = FALSE)
