# prepare_similarity_data.R

setwd("C:/Users/Lenovo/Downloads/DataFest2026/DataFest2026")

df <- read.csv("cross_language_audit_results.csv", stringsAsFactors = FALSE)

#numeric features
df$norm_ratio  <- pmax(df$norm_1, df$norm_2) / pmin(df$norm_1, df$norm_2)
df$pv_ratio    <- pmax(df$pageviews_1, df$pageviews_2) /
                  pmax(pmin(df$pageviews_1, df$pageviews_2), 1)
df$norm_sum    <- df$norm_1 + df$norm_2
df$norm_diff   <- abs(df$norm_1 - df$norm_2)
df$pv_sum      <- log1p(df$pageviews_1 + df$pageviews_2)
df$pv_min      <- log1p(pmin(df$pageviews_1, df$pageviews_2))
df$pv_max      <- log1p(pmax(df$pageviews_1, df$pageviews_2))

#categorical features
df$same_label  <- as.integer(
  !is.na(df$top_label_1) & !is.na(df$top_label_2) &
  df$top_label_1 == df$top_label_2
)

# Language pair identity (encoded as integer for RPT-1)
lang_pairs     <- paste(pmin(df$wiki_1, df$wiki_2),
                        pmax(df$wiki_1, df$wiki_2), sep = "_")
df$lang_pair_id <- as.integer(factor(lang_pairs))

#select features and target
features <- df[, c(
  "norm_1", "norm_2", "norm_ratio", "norm_sum", "norm_diff",
  "pv_sum", "pv_min", "pv_max", "pv_ratio",
  "same_label", "lang_pair_id",
  "cosine_sim"   # target — split in Python
)]

features <- features[complete.cases(features), ]

write.csv(features, "similarity_features.csv", row.names = FALSE)
