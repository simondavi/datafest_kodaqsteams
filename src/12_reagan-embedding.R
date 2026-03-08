# =============================================================================

library(tidyverse)
library(jsonlite)
library(glmnet)
library(dplyr)
# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

setwd("C:/Users/raedle/Desktop/KODAQS DataFest/DataFest2026/DataFest2026/data")

# Read first 3000 rows of the English Wikipedia edit data
page <- textConnection(readLines(gzfile("page_info.json.gz")))
page_data <- stream_in(page, verbose = FALSE)
close(page)

# =============================================================================
# Cross-Language Embedding Comparison
# Compares text embeddings of the same page_id across Wikipedia editions
# using cosine similarity.
# =============================================================================
# REQUIRES:
#   page_data  — dataframe with columns:
#     - page_id    : article identifier (shared across language editions)
#     - wiki_db    : language edition (e.g. "enwiki", "eswiki", "ruwiki")
#     - embedding  : list column, each element a numeric vector
# =============================================================================

library(tidyverse)

# =============================================================================
# STEP 1 — DATA QUALITY CHECK
# Before comparing, verify the embeddings are clean and joinable.
# =============================================================================

cat("=== Data Quality Check ===\n\n")

# How many rows per language?
cat("Rows per language edition:\n")
page_data %>% count(wiki_db, name = "n_pages") %>% print()

# How many page_ids appear in more than one language? (these are our targets)
page_id_coverage <- page_data %>%
  group_by(page_id) %>%
  summarise(
    n_langs       = n_distinct(wiki_db),
    langs_present = paste(sort(wiki_db), collapse = " | "),
    .groups = "drop"
  )

cat("\npage_id coverage:\n")
page_id_coverage %>% count(n_langs, name = "n_page_ids") %>% print()

# How many page_ids are shared across ALL languages?
all_langs    <- unique(page_data$wiki_db)
n_langs_total <- length(all_langs)

shared_all <- page_id_coverage %>%
  filter(n_langs == n_langs_total) %>%
  pull(page_id)

cat(sprintf("\npage_ids present in all %d editions: %d\n",
            n_langs_total, length(shared_all)))

# Check for missing / NULL embeddings
n_missing <- page_data %>%
  mutate(emb_missing = map_lgl(embedding, ~ is.null(.x) | length(.x) == 0)) %>%
  pull(emb_missing) %>% sum()

cat(sprintf("Rows with missing/empty embeddings: %d\n", n_missing))

# Check embedding dimensions are consistent
emb_dims <- page_data %>%
  mutate(emb_dim = map_int(embedding, length)) %>%
  count(wiki_db, emb_dim)

cat("\nEmbedding dimensions by language:\n")
print(emb_dims)

# =============================================================================
# Data Quality Check: page_id duplicates & duplicate rows
# Dataset: page_data
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. EXACT DUPLICATE ROWS
# A row is a full duplicate if every column matches another row exactly.
# =============================================================================

cat("=== 1. Exact Duplicate Rows ===\n")

n_total    <- nrow(page_data)
n_distinct <- nrow(distinct(page_data))
n_dupes    <- n_total - n_distinct

cat(sprintf("Total rows:            %d\n", n_total))
cat(sprintf("Distinct rows:         %d\n", n_distinct))
cat(sprintf("Exact duplicate rows:  %d (%.2f%%)\n",
            n_dupes, 100 * n_dupes / n_total))

if (n_dupes > 0) {
  cat("\nExample duplicate rows:\n")
  page_data %>%
    group_by(across(everything())) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    slice_head(n = 10) %>%
    print()
}

# =============================================================================
# 2. DUPLICATE page_id WITHIN THE SAME LANGUAGE EDITION
# Each page_id should appear exactly once per wiki_db.
# =============================================================================

cat("\n=== 2. Duplicate page_id Within Language Edition ===\n")

dupes_within_lang <- page_data %>%
  group_by(wiki_db, page_id) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  arrange(desc(n))

cat(sprintf("(wiki_db, page_id) pairs appearing more than once: %d\n",
            nrow(dupes_within_lang)))

if (nrow(dupes_within_lang) > 0) {
  cat("\nBreakdown by language:\n")
  dupes_within_lang %>%
    count(wiki_db, name = "n_duplicated_page_ids") %>%
    print()
  
  cat("\nTop duplicated page_ids:\n")
  print(head(dupes_within_lang, 20))
  
  # Show the actual rows for the most duplicated page_id
  worst <- dupes_within_lang %>% slice_max(n, n = 1)
  cat(sprintf("\nAll rows for worst offender — wiki_db='%s', page_id=%s:\n",
              worst$wiki_db, worst$page_id))
  page_data %>%
    filter(wiki_db == worst$wiki_db, page_id == worst$page_id) %>%
    select(-embedding) %>%   # suppress the vector column for readability
    print()
}

# =============================================================================
# 3. page_id COVERAGE ACROSS LANGUAGE EDITIONS
# How many page_ids appear in 1 language, 2 languages, all languages, etc.
# =============================================================================

cat("\n=== 3. page_id Coverage Across Languages ===\n")

all_langs <- unique(page_data$wiki_db)
cat(sprintf("Language editions found: %s\n\n", paste(sort(all_langs), collapse = ", ")))

coverage <- page_data %>%
  group_by(page_id) %>%
  summarise(
    n_langs       = n_distinct(wiki_db),
    langs_present = paste(sort(wiki_db), collapse = " | "),
    .groups = "drop"
  )

cat("Number of page_ids by how many editions they appear in:\n")
coverage %>%
  count(n_langs, name = "n_page_ids") %>%
  mutate(pct = sprintf("%.1f%%", 100 * n_page_ids / sum(n_page_ids))) %>%
  print()

cat(sprintf("\npage_ids present in ALL %d editions: %d\n",
            length(all_langs),
            sum(coverage$n_langs == length(all_langs))))

cat(sprintf("page_ids present in only 1 edition: %d\n",
            sum(coverage$n_langs == 1)))

# Which language combinations are most common?
cat("\nMost common language combinations per page_id:\n")
coverage %>%
  count(langs_present, name = "n_page_ids") %>%
  arrange(desc(n_page_ids)) %>%
  slice_head(n = 15) %>%
  print()

# =============================================================================
# 4. VISUALISE page_id OVERLAP (upset-style bar chart)
# =============================================================================

coverage %>%
  count(n_langs, name = "n_page_ids") %>%
  mutate(n_langs = factor(n_langs)) %>%
  ggplot(aes(x = n_langs, y = n_page_ids, fill = n_langs)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n_page_ids), vjust = -0.4, size = 4) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "page_id coverage across language editions",
       subtitle = "How many editions does each page_id appear in?",
       x        = "Number of language editions",
       y        = "Number of page_ids") +
  theme_minimal() +
  theme(legend.position = "none")

# Per-language breakdown: how many of its page_ids are shared vs. unique?
page_data %>%
  left_join(coverage %>% select(page_id, n_langs), by = "page_id") %>%
  mutate(shared = ifelse(n_langs > 1, "Shared (2+ editions)", "Edition-unique")) %>%
  count(wiki_db, shared) %>%
  ggplot(aes(x = wiki_db, y = n, fill = shared)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Shared (2+ editions)" = "steelblue",
                               "Edition-unique"       = "tomato")) +
  labs(title    = "Share of page_ids that are cross-edition vs. unique",
       x = NULL, y = "Proportion of page_ids", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# =============================================================================
# 5. SUMMARY
# =============================================================================

cat("\n=== Summary ===\n")
cat(sprintf("  Exact duplicate rows:                %d\n", n_dupes))
cat(sprintf("  Duplicate (wiki_db, page_id) pairs:  %d\n", nrow(dupes_within_lang)))
cat(sprintf("  page_ids in all editions:            %d\n",
            sum(coverage$n_langs == length(all_langs))))
cat(sprintf("  page_ids in only 1 edition:          %d\n",
            sum(coverage$n_langs == 1)))
cat(sprintf("  Language editions:                   %s\n",
            paste(sort(all_langs), collapse = ", ")))



# =============================================================================
# Inspect the 13 page_ids that appear in exactly 5 language editions
# =============================================================================

library(tidyverse)

# Pull the 13 page_ids with n_langs == 5
pages_in_5 <- coverage %>%
  filter(n_langs == 5)

cat(sprintf("=== %d page_ids appearing in exactly 5 editions ===\n\n",
            nrow(pages_in_5)))

# Show which languages each appears in
cat("page_id  |  editions present\n")
cat(strrep("-", 60), "\n")
pages_in_5 %>%
  select(page_id, langs_present) %>%
  print(n = 13)

# Pull all actual rows for these 13 page_ids from page_data
rows_in_5 <- page_data %>%
  filter(page_id %in% pages_in_5$page_id) %>%
  arrange(page_id, wiki_db)

cat(sprintf("\nTotal rows for these 13 page_ids: %d\n", nrow(rows_in_5)))
cat("(should be 13 × 5 = 65 if perfectly clean)\n\n")

# Show all metadata columns except embedding (too long to print)
cat("=== All rows (embedding suppressed) ===\n")
rows_in_5 %>%
  select(-any_of("embedding")) %>%
  print(n = 65)

# =============================================================================
# Inspect the 13 page_ids appearing in exactly 5 language editions (FIXED)
# =============================================================================

library(tidyverse)

# Pull the 13 page_ids with n_langs == 5
pages_in_5 <- coverage %>%
  filter(n_langs == 5)

cat(sprintf("=== %d page_ids appearing in exactly 5 editions ===\n\n",
            nrow(pages_in_5)))

cat("page_id  |  editions present\n")
cat(strrep("-", 60), "\n")
pages_in_5 %>%
  select(page_id, langs_present) %>%
  as.data.frame() %>%
  print()

# Pull all actual rows for these 13 page_ids
rows_in_5 <- page_data %>%
  filter(page_id %in% pages_in_5$page_id) %>%
  arrange(page_id, wiki_db)

cat(sprintf("\nTotal rows for these 13 page_ids: %d\n", nrow(rows_in_5)))
cat("(expected: 13 x 5 = 65)\n\n")

# Drop embedding + any remaining list columns before printing
cat("=== All rows (list columns suppressed) ===\n")
rows_in_5 %>%
  select(where(~ !is.list(.x))) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# Which editions are most commonly paired among these 13?
# =============================================================================

cat("\n=== How often does each edition appear among the 13 page_ids? ===\n")
pages_in_5 %>%
  separate_rows(langs_present, sep = " \\| ") %>%
  count(langs_present, name = "times_appearing") %>%
  arrange(desc(times_appearing)) %>%
  as.data.frame() %>%
  print()

cat("\n=== Edition combinations for these 13 page_ids ===\n")
pages_in_5 %>%
  count(langs_present, name = "n") %>%
  as.data.frame() %>%
  print()



# =============================================================================
# Inspect 13 page_ids (5 editions) — with full metadata from page_data
# =============================================================================

# Join the 13 page_ids back to the original page_data to get all columns
rows_in_5_full <- page_data %>%
  filter(page_id %in% pages_in_5$page_id) %>%
  arrange(page_id, wiki_db) %>%
  select(where(~ !is.list(.x)))   # drop list columns (embedding etc.)

cat(sprintf("Rows: %d  |  Columns: %d\n\n",
            nrow(rows_in_5_full), ncol(rows_in_5_full)))

# Print all columns
rows_in_5_full %>%
  as.data.frame() %>%
  print()

# Also show column names so you can see what metadata is available
cat("\nColumns available:\n")
print(names(rows_in_5_full))





# =============================================================================
# Cross-Language QID Coverage Check — page_data
# Column: qid (Wikidata identifier)
# =============================================================================

library(tidyverse)

all_langs     <- unique(page_data$wiki_db)
n_langs_total <- length(all_langs)

# =============================================================================
# 1. QID COMPLETENESS PER LANGUAGE
# =============================================================================

cat("=== 1. QID completeness per language ===\n")
page_data %>%
  group_by(wiki_db) %>%
  summarise(
    n_rows        = n(),
    n_with_qid    = sum(!is.na(qid) & qid != ""),
    n_missing_qid = sum(is.na(qid) | qid == ""),
    pct_missing   = sprintf("%.1f%%", 100 * n_missing_qid / n_rows),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# 2. QID COVERAGE ACROSS EDITIONS
# =============================================================================

cat("\n=== 2. How many editions share each QID? ===\n")
qid_coverage <- page_data %>%
  filter(!is.na(qid), qid != "") %>%
  group_by(qid) %>%
  summarise(
    n_langs       = n_distinct(wiki_db),
    langs_present = paste(sort(wiki_db), collapse = " | "),
    .groups = "drop"
  )

qid_coverage %>%
  count(n_langs, name = "n_qids") %>%
  mutate(pct = sprintf("%.1f%%", 100 * n_qids / sum(n_qids))) %>%
  as.data.frame() %>%
  print()

cat(sprintf("\nQIDs in ALL %d editions: %d\n", n_langs_total,
            sum(qid_coverage$n_langs == n_langs_total)))
cat(sprintf("QIDs in only 1 edition:  %d\n",
            sum(qid_coverage$n_langs == 1)))

# =============================================================================
# 3. MOST COMMON EDITION COMBINATIONS
# =============================================================================

cat("\n=== 3. Most common edition combinations by QID ===\n")
qid_coverage %>%
  count(langs_present, name = "n_qids") %>%
  arrange(desc(n_qids)) %>%
  slice_head(n = 20) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# 4. PER-LANGUAGE MATCH RATE
# =============================================================================

cat("\n=== 4. Per-language: shared vs. edition-unique QIDs ===\n")
page_data %>%
  filter(!is.na(qid), qid != "") %>%
  left_join(qid_coverage %>% select(qid, n_langs), by = "qid") %>%
  mutate(shared = ifelse(n_langs > 1,
                         "Shared (2+ editions)", "Edition-unique")) %>%
  count(wiki_db, shared) %>%
  pivot_wider(names_from = shared, values_from = n, values_fill = 0) %>%
  mutate(
    total      = rowSums(across(where(is.numeric))),
    pct_shared = sprintf("%.1f%%", 100 * `Shared (2+ editions)` / total)
  ) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# 5. VISUALISATIONS
# =============================================================================

# 5a. How many editions per QID?
qid_coverage %>%
  count(n_langs, name = "n_qids") %>%
  mutate(n_langs = factor(n_langs)) %>%
  ggplot(aes(x = n_langs, y = n_qids, fill = n_langs)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n_qids), vjust = -0.4, size = 4) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "QID coverage across language editions",
       subtitle = "How many editions share each Wikidata QID?",
       x = "Number of language editions",
       y = "Number of QIDs") +
  theme_minimal() +
  theme(legend.position = "none")

# 5b. Per-language shared vs. unique
page_data %>%
  filter(!is.na(qid), qid != "") %>%
  left_join(qid_coverage %>% select(qid, n_langs), by = "qid") %>%
  mutate(shared = ifelse(n_langs > 1,
                         "Shared (2+ editions)", "Edition-unique")) %>%
  count(wiki_db, shared) %>%
  ggplot(aes(x = wiki_db, y = n, fill = shared)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Shared (2+ editions)" = "steelblue",
                               "Edition-unique"       = "tomato")) +
  labs(title = "Share of articles with a cross-edition QID match",
       x = NULL, y = "Proportion of articles", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# =============================================================================
# 6. CREATE ANALYSIS-READY DATASET (QID shared across 2+ editions)
# =============================================================================

qid_shared <- qid_coverage %>%
  filter(n_langs >= 2) %>%
  pull(qid)

page_data_for_comparison <- page_data %>%
  filter(qid %in% qid_shared)

cat("\n=== 6. Dataset ready for embedding comparison ===\n")
cat(sprintf("Rows:            %d\n",   nrow(page_data_for_comparison)))
cat(sprintf("Unique QIDs:     %d\n",   n_distinct(page_data_for_comparison$qid)))
cat(sprintf("Editions:        %s\n",
            paste(sort(unique(page_data_for_comparison$wiki_db)),
                  collapse = ", ")))
cat("\nRows per edition in comparison dataset:\n")
page_data_for_comparison %>%
  count(wiki_db, name = "n_articles") %>%
  as.data.frame() %>%
  print()


names(page_data)


# 1. See all column names
names(page_data)

# 2. See what the embedding looks like for Q9960 in one language
reagan <- page_data %>% 
  filter(qid == "Q9960")



# =============================================================================
# Top 4 Predicted Labels for Ronald Reagan (Q9960) — FIXED
# =============================================================================

library(tidyverse)

# --- Re-extract top 4 -------------------------------------------------------
sample_labels <- page_data %>%
  filter(qid == "Q9960") %>%
  slice(1) %>%
  pull(predicted_labels) %>%
  .[[1]]

label_col <- names(sample_labels)[1]
score_col <- names(sample_labels)[2]

top4 <- page_data %>%
  filter(qid == "Q9960") %>%
  select(wiki_db, predicted_labels) %>%
  mutate(
    labels_df = map(predicted_labels, function(df) {
      df %>%
        arrange(desc(.data[[score_col]])) %>%
        slice_head(n = 2) %>%
        mutate(rank = row_number())
    })
  ) %>%
  select(wiki_db, labels_df) %>%
  unnest(labels_df) %>%
  mutate(
    edition   = str_remove(wiki_db, "wiki"),
    label_str = as.character(.data[[label_col]]),
    score_val = as.numeric(.data[[score_col]]),
    # Clean label for display: remove long prefixes, replace underscores
    label_short = label_str %>%
      str_replace_all("_and_", " & ") %>%
      str_replace_all("_", " ") %>%
      str_trunc(22)
  )

# =============================================================================
# FIXED COLOUR PALETTE — one colour per unique label
# =============================================================================

all_labels     <- unique(top4$label_str)
n_labels       <- length(all_labels)

palette_base <- c(
  "#2166ac",  # blue
  "#4dac26",  # green
  "#d01c8b",  # magenta
  "#f1a340",  # orange
  "#762a83",  # purple
  "#1b7837",  # dark green
  "#e31a1c",  # red
  "#ff7f00",  # bright orange
  "#6a3d9a",  # violet
  "#b15928",  # brown
  "#33a02c",  # medium green
  "#a6cee3"   # light blue
)

if (n_labels > length(palette_base)) {
  palette_base <- c(palette_base,
                    hcl.colors(n_labels - length(palette_base), "Dark 3"))
}

label_colours <- setNames(palette_base[seq_len(n_labels)], all_labels)

# Clean legend labels (same transformation as label_short)
legend_labels <- setNames(
  all_labels %>%
    str_replace_all("_and_", " & ") %>%
    str_replace_all("_", " "),
  all_labels
)

# =============================================================================
# PLOT
# =============================================================================

ggplot(top4,
       aes(x    = edition,
           y    = score_val,
           fill = label_str)) +
  geom_col(position = position_dodge(0.85), width = 0.8, alpha = 0.92) +
  
  scale_fill_manual(values = label_colours, labels = legend_labels) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "Top 2 predicted labels for Ronald Reagan (Q9960)",
    subtitle = "Colour = label category, consistent across all language editions",
    x        = "Language edition",
    y        = "Predicted probability",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(size = 10, face = "bold"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.5, "cm"),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))   # FIX: moved out of theme()





# =============================================================================
# Top 4 Predicted Labels for Ronald Reagan (Q9960) — FIXED
# =============================================================================

library(tidyverse)

# --- Re-extract top 4 -------------------------------------------------------
sample_labels <- page_data %>%
  filter(qid == "Q9960") %>%
  slice(1) %>%
  pull(predicted_labels) %>%
  .[[1]]

label_col <- names(sample_labels)[1]
score_col <- names(sample_labels)[2]

top4 <- page_data %>%
  filter(qid == "Q9960",
         wiki_db %in% c("arwiki", "dewiki", "enwiki", "eswiki")) %>%
  select(wiki_db, predicted_labels) %>%
  mutate(
    labels_df = map(predicted_labels, function(df) {
      df %>%
        arrange(desc(.data[[score_col]])) %>%
        slice_head(n = 4) %>%
        mutate(rank = row_number())
    })
  ) %>%
  select(wiki_db, labels_df) %>%
  unnest(labels_df) %>%
  mutate(
    edition   = str_remove(wiki_db, "wiki"),
    label_str = as.character(.data[[label_col]]),
    score_val = as.numeric(.data[[score_col]]),
    # Clean label for display: remove long prefixes, replace underscores
    label_short = label_str %>%
      str_replace_all("_and_", " & ") %>%
      str_replace_all("_", " ") %>%
      str_trunc(22)
  )

# =============================================================================
# FIXED COLOUR PALETTE — one colour per unique label
# =============================================================================

all_labels     <- unique(top4$label_str)
n_labels       <- length(all_labels)

palette_base <- c(
  "#2166ac",  # blue
  "#4dac26",  # green
  "#d01c8b",  # magenta
  "#f1a340",  # orange
  "#762a83",  # purple
  "#1b7837",  # dark green
  "#e31a1c",  # red
  "#ff7f00",  # bright orange
  "#6a3d9a",  # violet
  "#b15928",  # brown
  "#33a02c",  # medium green
  "#a6cee3"   # light blue
)

if (n_labels > length(palette_base)) {
  palette_base <- c(palette_base,
                    hcl.colors(n_labels - length(palette_base), "Dark 3"))
}

label_colours <- setNames(palette_base[seq_len(n_labels)], all_labels)

# Clean legend labels (same transformation as label_short)
legend_labels <- setNames(
  all_labels %>%
    str_replace_all("_and_", " & ") %>%
    str_replace_all("_", " "),
  all_labels
)

# =============================================================================
# PLOT
# =============================================================================

ggplot(top4,
       aes(x    = edition,
           y    = score_val,
           fill = label_str)) +
  geom_col(position = position_dodge(0.85), width = 0.8, alpha = 0.92) +
  
  scale_fill_manual(values = label_colours, labels = legend_labels) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "Top 3 predicted labels for Ronald Reagan (Q9960)",
    subtitle = "ar · de · en · es — colour consistent across editions",
    x        = "Language edition",
    y        = "Predicted probability",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(size = 10, face = "bold"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.5, "cm"),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))   # FIX: moved out of theme()




# =============================================================================
# Top 4 Predicted Labels for Ronald Reagan (Q9960) — FIXED
# =============================================================================

library(tidyverse)

# =============================================================================
# Top 3 Predicted Labels — Q9916 — ar, de, en, es
# =============================================================================

library(tidyverse)

# Detect column names from nested predicted_labels
sample_labels <- page_data %>%
  filter(qid == "Q9916") %>%
  slice(1) %>%
  pull(predicted_labels) %>%
  .[[1]]

label_col <- names(sample_labels)[1]
score_col <- names(sample_labels)[2]

# Extract top 3 for Q9916
top4 <- page_data %>%
  filter(qid == "Q9916",
         wiki_db %in% c("arwiki", "dewiki", "enwiki", "eswiki")) %>%
  select(wiki_db, predicted_labels) %>%
  mutate(
    labels_df = map(predicted_labels, function(df) {
      df %>%
        arrange(desc(.data[[score_col]])) %>%
        slice_head(n = 3) %>%
        mutate(rank = row_number())
    })
  ) %>%
  select(wiki_db, labels_df) %>%
  unnest(labels_df) %>%
  mutate(
    edition     = str_remove(wiki_db, "wiki"),
    label_str   = as.character(.data[[label_col]]),
    score_val   = as.numeric(.data[[score_col]]),
    label_short = label_str %>%
      str_replace_all("_and_", " & ") %>%
      str_replace_all("_", " ") %>%
      str_trunc(22)
  )

# Check what article Q9916 is
page_title_q9916 <- page_data %>%
  filter(qid == "Q9916", wiki_db == "enwiki") %>%
  pull(page_title) %>%
  first()
cat(sprintf("Q9916 = %s\n", page_title_q9916))

# Fixed colour palette
all_labels  <- unique(top4$label_str)
n_labels    <- length(all_labels)

palette_base <- c(
  "#2166ac", "#4dac26", "#d01c8b", "#f1a340",
  "#762a83", "#1b7837", "#e31a1c", "#ff7f00",
  "#6a3d9a", "#b15928", "#33a02c", "#a6cee3"
)

if (n_labels > length(palette_base)) {
  palette_base <- c(palette_base,
                    hcl.colors(n_labels - length(palette_base), "Dark 3"))
}

label_colours <- setNames(palette_base[seq_len(n_labels)], all_labels)
legend_labels <- setNames(
  all_labels %>%
    str_replace_all("_and_", " & ") %>%
    str_replace_all("_", " "),
  all_labels
)

# Plot
ggplot(top4,
       aes(x = edition, y = score_val, fill = label_str)) +
  geom_col(position = position_dodge(0.85), width = 0.8, alpha = 0.92) +
  scale_fill_manual(values = label_colours, labels = legend_labels) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = sprintf("Top 3 predicted labels for %s (Q9916)", page_title_q9916),
    subtitle = "ar · de · en · es — colour consistent across editions",
    x        = "Language edition",
    y        = "Predicted probability",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(size = 10, face = "bold"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.5, "cm"),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

