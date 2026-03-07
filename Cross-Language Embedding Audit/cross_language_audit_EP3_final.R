library(jsonlite)
library(dplyr)

set.seed(42)  #for reproducibility
N_QID_SAMPLE <- 3000  #number of multi-language QIDs to sample
setwd("C:/Users/Lenovo/Downloads/DataFest2026/DataFest2026/data")

con <- gzcon(file("page_info.json.gz", "rb"))
#---
#from 2nd time
page_info <- readRDS("../page_info.rds")  
#1st time
#page_info <- stream_in(con, verbose = FALSE)
#saveRDS(page_info, "../page_info.rds")
#---
close(con)


#embedding matrix
emb_matrix <- do.call(rbind, page_info$embedding)
#embedding norm: proxy differentiating between stub/niche article and proper article
#low norm = sparse link structure = likely a stub or niche article.
page_info$emb_norm <- sqrt(rowSums(emb_matrix^2))

#top predicted label per article (highest probability label)
get_top_label <- function(labels_df) {
  if (is.null(labels_df) || nrow(labels_df) == 0) return(NA_character_)
  labels_df$label[which.max(labels_df$probability)]
}
page_info$top_label <- sapply(page_info$predicted_labels, get_top_label)


qid_counts   <- table(page_info$qid)
multi_qids   <- names(qid_counts[qid_counts > 1])
sampled_qids <- sample(multi_qids, min(N_QID_SAMPLE, length(multi_qids)))

cosine_sim <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

#pairwise cosine similarity
pairs_list <- vector("list", length(sampled_qids))

for (i in seq_along(sampled_qids)) {
  q   <- sampled_qids[i]
  idx <- which(page_info$qid == q)  # row indices directly into page_info

  if (length(idx) < 2) next

  combos <- combn(idx, 2, simplify = TRUE)  # each column is a pair of page_info row indices
  rows   <- vector("list", ncol(combos))

  for (j in seq_len(ncol(combos))) {
    r1 <- combos[1, j]
    r2 <- combos[2, j]

    rows[[j]] <- data.frame(
      qid           = q,
      wiki_1        = page_info$wiki_db[r1],
      page_title_1  = page_info$page_title[r1],
      pageviews_1   = page_info$pageviews[r1],
      norm_1        = page_info$emb_norm[r1],
      top_label_1   = page_info$top_label[r1],
      wiki_2        = page_info$wiki_db[r2],
      page_title_2  = page_info$page_title[r2],
      pageviews_2   = page_info$pageviews[r2],
      norm_2        = page_info$emb_norm[r2],
      top_label_2   = page_info$top_label[r2],
      cosine_sim    = cosine_sim(emb_matrix[r1, ], emb_matrix[r2, ]),
      stringsAsFactors = FALSE
    )
  }
  pairs_list[[i]] <- do.call(rbind, rows)
}

pairs_df <- do.call(rbind, Filter(Negate(is.null), pairs_list))
pairs_df  <- pairs_df[order(pairs_df$cosine_sim), ]  #most divergent first

#total pairs computed:
nrow(pairs_df)

#different reasons for divergence
# 1. stub vs full article
# 2. different topic framing, articles have differing top labels
# 3. unexplained divergence, something else (if both articles are substantial and labels are the same)

classify_divergence <- function(norm1, norm2, label1, label2, pv1, pv2) {
  norm_ratio <- max(norm1, norm2) / pmax(min(norm1, norm2), 1e-6)
  pv_ratio   <- max(pv1, pv2)    / pmax(min(pv1, pv2), 1)

  if (norm_ratio > 2 || pv_ratio > 10) {
    "Stub vs. full article"
  } else if (!is.na(label1) && !is.na(label2) && label1 != label2) {
    "Different topic framing"
  } else {
    "Unexplained Divergence"
  }
}

pairs_df$divergence_reason <- mapply(
  classify_divergence,
  pairs_df$norm_1, pairs_df$norm_2,
  pairs_df$top_label_1, pairs_df$top_label_2,
  pairs_df$pageviews_1, pairs_df$pageviews_2
)

#results
#Mean cosine similarity
round(mean(pairs_df$cosine_sim), 4)

#save
out_path <- "../cross_language_audit_results.csv"
write.csv(pairs_df, out_path, row.names = FALSE)

#graphs
library(ggplot2)

#distribution of cosine similarity
ggplot(pairs_df, aes(x = cosine_sim)) +
  geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = median(pairs_df$cosine_sim), linetype = "dashed", colour = "red") +
  annotate("text", x = median(pairs_df$cosine_sim) - 0.01, y = Inf,
           label = paste0("Median: ", round(median(pairs_df$cosine_sim), 3)),
           hjust = 1, vjust = 2, colour = "red", size = 3.5) +
  labs(title = "Distribution of cross-language embedding similarity",
       subtitle = "Each pair shares the same Wikidata QID",
       x = "Cosine similarity", y = "Number of pairs") +
  theme_minimal()

#divergence reason breakdown (bar chart)
reason_counts <- as.data.frame(table(pairs_df$divergence_reason))
colnames(reason_counts) <- c("reason", "count")
reason_counts$pct <- round(reason_counts$count / sum(reason_counts$count) * 100, 1)

ggplot(reason_counts, aes(x = reorder(reason, count), y = count, fill = reason)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Why do cross-language pairs diverge?",
       x = NULL, y = "Number of pairs") +
  theme_minimal()

#similarity by divergence reason (boxplot)
ggplot(pairs_df, aes(x = reorder(divergence_reason, cosine_sim, median),
                     y = cosine_sim, fill = divergence_reason)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.5, outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Embedding similarity by divergence reason",
       x = NULL, y = "Cosine similarity") +
  theme_minimal()

#which language pairs diverge most?
pairs_df$lang_pair <- paste(
  pmin(pairs_df$wiki_1, pairs_df$wiki_2),
  pmax(pairs_df$wiki_1, pairs_df$wiki_2),
  sep = " — "
)

lang_pair_stats <- pairs_df |>
  group_by(lang_pair) |>
  summarise(
    n          = n(),
    mean_sim   = mean(cosine_sim),
    median_sim = median(cosine_sim),
    pct_low    = mean(cosine_sim < 0.85) * 100
  ) |>
  arrange(mean_sim)

#language pair similarity (lowest mean first)
print(lang_pair_stats, n = 20)

#heatmap: mean similarity by language pair
lang_pair_matrix <- pairs_df |>
  group_by(wiki_1, wiki_2) |>
  summarise(mean_sim = mean(cosine_sim), .groups = "drop")

#symmetric (add both directions)
lang_pair_sym <- rbind(
  lang_pair_matrix,
  data.frame(wiki_1 = lang_pair_matrix$wiki_2,
             wiki_2 = lang_pair_matrix$wiki_1,
             mean_sim = lang_pair_matrix$mean_sim)
)

ggplot(lang_pair_sym, aes(x = wiki_1, y = wiki_2, fill = mean_sim)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(mean_sim, 2)), size = 2.8) +
  scale_fill_gradient(low = "tomato", high = "steelblue",
                      limits = c(0.85, 1), oob = scales::squish) +
  labs(title = "Mean embedding similarity between language editions",
       x = NULL, y = NULL, fill = "Mean\ncosine sim") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#worst offenders per language pair
#most divergent pair
worst_per_pair <- pairs_df |>
  group_by(lang_pair) |>
  slice_min(cosine_sim, n = 1) |>
  select(lang_pair, page_title_1, page_title_2, cosine_sim, divergence_reason) |>
  arrange(cosine_sim)
print(worst_per_pair, n = 20)


#patterns saved to csv

df <- pairs_df  #or reload with
#df <- read.csv("../cross_language_audit_results.csv", stringsAsFactors = FALSE)

df$norm_ratio  <- pmax(df$norm_1, df$norm_2) / pmin(df$norm_1, df$norm_2)
df$min_pv      <- pmin(df$pageviews_1, df$pageviews_2)
df$lang_pair   <- paste(pmin(df$wiki_1, df$wiki_2),
                        pmax(df$wiki_1, df$wiki_2), sep = " — ")

#repeat offender QIDs (most pairs in bottom 5%)
threshold   <- quantile(df$cosine_sim, 0.05)
low_pairs   <- df[df$cosine_sim < threshold, ]
qid_counts_low <- as.data.frame(sort(table(low_pairs$qid), decreasing = TRUE))
colnames(qid_counts_low) <- c("qid", "n_low_pairs")

qid_summary <- do.call(rbind, lapply(qid_counts_low$qid, function(q) {
  rows <- df[df$qid == q, ]
  data.frame(
    qid            = q,
    n_low_pairs    = sum(df$qid == q & df$cosine_sim < threshold),
    n_total_pairs  = nrow(rows),
    mean_sim       = round(mean(rows$cosine_sim), 4),
    min_sim        = round(min(rows$cosine_sim), 4),
    example_title  = rows$page_title_1[1],
    example_wiki   = rows$wiki_1[1],
    stringsAsFactors = FALSE
  )
}))
qid_summary <- qid_summary[order(-qid_summary$n_low_pairs), ]
write.csv(qid_summary, "../pattern1_repeat_offender_qids.csv", row.names = FALSE)

#mean similarity by language edition
lang_stats <- do.call(rbind, lapply(unique(c(df$wiki_1, df$wiki_2)), function(w) {
  rows <- df[df$wiki_1 == w | df$wiki_2 == w, ]
  data.frame(
    wiki           = w,
    n_pairs        = nrow(rows),
    mean_sim       = round(mean(rows$cosine_sim), 4),
    median_sim     = round(median(rows$cosine_sim), 4),
    pct_below_0.85 = round(mean(rows$cosine_sim < 0.85) * 100, 1),
    stringsAsFactors = FALSE
  )
}))
lang_stats <- lang_stats[order(lang_stats$mean_sim), ]
write.csv(lang_stats, "../pattern2_similarity_by_language.csv", row.names = FALSE)

#language pair heatmap data
lang_pair_stats <- aggregate(cosine_sim ~ lang_pair + wiki_1 + wiki_2,
                             data = df, FUN = mean)
colnames(lang_pair_stats)[4] <- "mean_sim"
lang_pair_stats$mean_sim   <- round(lang_pair_stats$mean_sim, 4)
lang_pair_stats$n_pairs    <- as.integer(table(df$lang_pair)[lang_pair_stats$lang_pair])
lang_pair_stats            <- lang_pair_stats[order(lang_pair_stats$mean_sim), ]
write.csv(lang_pair_stats, "../pattern2b_language_pair_heatmap.csv", row.names = FALSE)

#mean similarity by topic label
label_stats <- aggregate(cosine_sim ~ top_label_1, data = df, FUN = function(x)
  c(mean = mean(x), median = median(x), n = length(x),
    pct_low = mean(x < 0.85) * 100))
label_stats <- do.call(data.frame, label_stats)
colnames(label_stats) <- c("top_label", "mean_sim", "median_sim", "n_pairs", "pct_below_0.85")
label_stats$mean_sim     <- round(label_stats$mean_sim, 4)
label_stats$median_sim   <- round(label_stats$median_sim, 4)
label_stats$pct_below_0.85 <- round(label_stats$pct_below_0.85, 1)
label_stats              <- label_stats[order(label_stats$mean_sim), ]
write.csv(label_stats, "../pattern3_similarity_by_topic.csv", row.names = FALSE)

#norm ratio 
df$norm_ratio_bin <- cut(df$norm_ratio,
                         breaks = c(1, 1.5, 2, 3, 5, Inf),
                         labels = c("1.0–1.5", "1.5–2.0", "2.0–3.0", "3.0–5.0", ">5.0"),
                         include.lowest = TRUE)
norm_stats <- aggregate(cosine_sim ~ norm_ratio_bin, data = df, FUN = function(x)
  c(mean = mean(x), n = length(x)))
norm_stats <- do.call(data.frame, norm_stats)
colnames(norm_stats) <- c("norm_ratio_bin", "mean_sim", "n_pairs")
norm_stats$mean_sim <- round(norm_stats$mean_sim, 4)
write.csv(norm_stats, "../pattern4_norm_ratio_vs_similarity.csv", row.names = FALSE)


