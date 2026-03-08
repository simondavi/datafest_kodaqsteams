# =============================================================================
# Bot Detection via Edit Type + Comment Features
# Wikipedia English Edition (enwiki)
# =============================================================================
# Goal: Use the structural features of Wikipedia edits (what was changed and
# how) and revision comment text to predict whether an edit was made by a bot.
#
# Two feature sets are compared:
#   Model A — edit-type features only (behavioural fingerprint)
#   Model B — edit-type features + comment text features (combined)
#
# This lets us separate "implicit" bot detection (behaviour alone) from
# "explicit" detection (bots often self-identify in their comments).
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
eng <- textConnection(readLines(gzfile("enwiki.json.gz"), n = 20000))
eng_data <- stream_in(eng, verbose = FALSE)
close(eng)



# -----------------------------------------------------------------------------
# 2. PARSE edit_types_json INTO FEATURES
# -----------------------------------------------------------------------------
# Each edit's JSON contains three keys:
#   - node-edits: structural changes (e.g. inserting a Template or Wikilink)
#   - text-edits: word-level insertions and deletions
#   - context:    which section was affected
#
# We extract:
#   - Count of each (node_type × action) combination per edit
#     e.g. Template_insert, Wikilink_remove, Category_change, ...
#   - Total words inserted / removed (from text-edits)
#   - Total number of node-edits and text-edits

extract_features <- function(json_str) {
  parsed <- tryCatch(fromJSON(json_str, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  
  # Count each (type_action) pair in node-edits
  node_feats <- list()
  for (ne in parsed[["node-edits"]]) {
    key <- paste0(ne[[1]], "_", ne[[2]])          # e.g. "Template_insert"
    node_feats[[key]] <- (node_feats[[key]] %||% 0) + 1
  }
  
  # Sum word counts from text-edits
  n_word_insert <- 0L
  n_word_remove <- 0L
  for (te in parsed[["text-edits"]]) {
    if (te[[2]] == "insert") n_word_insert <- n_word_insert + as.integer(te[[4]])
    if (te[[2]] == "remove") n_word_remove <- n_word_remove + as.integer(te[[4]])
  }
  
  c(node_feats,
    list(n_word_insert = n_word_insert,
         n_word_remove = n_word_remove,
         n_node_edits  = length(parsed[["node-edits"]]),
         n_text_edits  = length(parsed[["text-edits"]])))
}

# Apply to all edits and pivot to a wide feature matrix (one row per edit)
features_df <- eng_data$edit_types_json %>%
  map(extract_features) %>%
  map(as_tibble) %>%
  bind_rows(.id = "row_id") %>%
  mutate(
    across(everything(), ~ replace_na(.x, 0)),
    row_id    = as.integer(row_id),
    is_bot    = eng_data$is_bot[row_id],
    page_id   = eng_data$page_id[row_id],
    user_text = eng_data$user_text[row_id]
  )

# -----------------------------------------------------------------------------
# 3. EXPLORATORY: WHICH FEATURES DIFFER MOST BETWEEN BOTS AND HUMANS?
# -----------------------------------------------------------------------------
# Compute the mean of each feature by bot status, then take the log2 ratio
# (bot mean / human mean). Values > 0 indicate bot-skewed features.

feat_cols <- features_df %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

mean_by_bot <- features_df %>%
  group_by(is_bot) %>%
  summarise(across(all_of(feat_cols), mean), .groups = "drop") %>%
  pivot_longer(-is_bot, names_to = "feature", values_to = "mean") %>%
  pivot_wider(names_from = is_bot, values_from = mean, names_prefix = "is_bot_") %>%
  rename(human = is_bot_FALSE, bot = is_bot_TRUE) %>%
  mutate(ratio = (bot + 0.001) / (human + 0.001)) %>%   # small smoothing to avoid /0
  arrange(desc(ratio))

# Visualise the 10 most bot-skewed and 10 most human-skewed features
mean_by_bot %>%
  slice(c(1:10, (n() - 9):n())) %>%
  mutate(feature = fct_reorder(feature, ratio)) %>%
  ggplot(aes(x = feature, y = log2(ratio), fill = ratio > 1)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Human-skewed", "Bot-skewed")) +
  labs(title = "Edit type features: bot vs. human (log2 ratio)",
       x = NULL, y = "log2(bot mean / human mean)", fill = NULL) +
  theme_minimal()

# -----------------------------------------------------------------------------
# 4. MODELLING: ELASTIC NET LOGISTIC REGRESSION
# -----------------------------------------------------------------------------
# We use regularized logistic regression (elastic net via glmnet) because:
#   (a) Many features are sparse and correlated → plain GLM suffers from
#       complete separation and inflated coefficients
#   (b) Elastic net (alpha = 0.5) combines lasso (feature selection) and
#       ridge (handles correlated predictors) penalties
#
# Class imbalance (~4% bots): handled via inverse-frequency sample weights

# Clean column names (spaces break glmnet formulas)
features_model <- features_df %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  mutate(is_bot = as.integer(is_bot))

feat_cols_clean <- features_model %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

# Inverse-frequency weights: upweight the minority class (bots)
n_total <- nrow(features_model)
n_bot   <- sum(features_model$is_bot)
weights <- ifelse(features_model$is_bot == 1,
                  n_total / (2 * n_bot),
                  n_total / (2 * (n_total - n_bot)))

# Stratified 80/20 train-test split
set.seed(42)
bot_idx   <- which(features_model$is_bot == 1)
human_idx <- which(features_model$is_bot == 0)

train_idx <- c(sample(bot_idx,   size = 0.8 * length(bot_idx)),
               sample(human_idx, size = 0.8 * length(human_idx)))
test_idx  <- setdiff(seq_len(n_total), train_idx)

train   <- features_model[train_idx, ]
test    <- features_model[test_idx, ]
train_w <- weights[train_idx]

# Prepare matrices for glmnet
X_train <- train %>% select(all_of(feat_cols_clean)) %>% as.matrix()
y_train <- train$is_bot
X_test  <- test  %>% select(all_of(feat_cols_clean)) %>% as.matrix()

# Cross-validated elastic net: selects optimal regularization strength (lambda)
# using 5-fold CV, optimizing for AUC (better than accuracy under class imbalance)
set.seed(42)
cv_model <- cv.glmnet(X_train, y_train,
                      family       = "binomial",
                      alpha        = 0.5,
                      weights      = train_w,
                      nfolds       = 5,
                      type.measure = "auc")

plot(cv_model)   # AUC vs. log(lambda); left dashed line = lambda.min

# -----------------------------------------------------------------------------
# 5. EVALUATION
# -----------------------------------------------------------------------------

pred_prob <- predict(cv_model, newx = X_test,
                     s = "lambda.min", type = "response")[, 1]
pred_bot  <- as.integer(pred_prob > 0.5)

cat("=== Confusion Matrix ===\n")
print(table(Predicted = pred_bot, Actual = test$is_bot))

tp <- sum(pred_bot == 1 & test$is_bot == 1)
fp <- sum(pred_bot == 1 & test$is_bot == 0)
fn <- sum(pred_bot == 0 & test$is_bot == 1)

cat(sprintf("\nAUC (CV):  %.3f\n", max(cv_model$cvm)))
cat(sprintf("Precision: %.3f\n", tp / (tp + fp)))
cat(sprintf("Recall:    %.3f\n", tp / (tp + fn)))
cat(sprintf("F1:        %.3f\n", 2 * tp / (2 * tp + fp + fn)))

# -----------------------------------------------------------------------------
# 6. INSPECT COEFFICIENTS
# -----------------------------------------------------------------------------
# Positive coefficients → predictive of bots
# Negative coefficients → predictive of humans
# Features shrunk to zero were not informative (lasso effect)

coef_mat <- coef(cv_model, s = "lambda.min")
coef_df <- tibble(
  term     = rownames(coef_mat),
  estimate = as.numeric(coef_mat)
) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(estimate))

print(coef_df, n = 30)

# -----------------------------------------------------------------------------
# 7. COMMENT FEATURES
# -----------------------------------------------------------------------------
# Revision comments offer a second channel of information. Key patterns:
#   - Bots often self-identify ("Bot:", "BOT", tool names like "WaybackMedic")
#   - Bots always leave a comment; ~23% of human edits have empty comments
#   - Bots link to their own documentation using [[wikilinks]]
#   - Humans use /* Section */ prefixes from clicking "edit section" in the UI
#
# We engineer simple binary/numeric features rather than full text modelling,
# which keeps the approach interpretable and consistent with the edit-type model.

comment_features <- eng_data %>%
  mutate(
    comment_length      = nchar(revision_comment),
    comment_is_empty    = is.na(revision_comment) | revision_comment == "",
    comment_nwords      = str_count(revision_comment, "\\S+"),
    
    # Bots frequently name themselves explicitly in comments
    mentions_bot        = str_detect(tolower(revision_comment), "bot"),
    mentions_automated  = str_detect(tolower(revision_comment), "automated|script|auto"),
    
    # Bots link to their own user pages / policy pages
    has_wikilink        = str_detect(revision_comment, "\\[\\["),
    
    # Bots sometimes open with a bracketed tool name e.g. "[[:en:WP:UCB|..."
    starts_with_bracket = str_detect(revision_comment, "^\\["),
    
    # /* Section */ prefix is generated by the UI "edit section" button — human only
    starts_with_slash   = str_detect(revision_comment, "^/"),
    
    # Bots mention fixes more; humans mention reverts more (but both are noisy)
    mentions_fix        = str_detect(tolower(revision_comment), "fix|correct|clean"),
    mentions_revert     = str_detect(tolower(revision_comment), "revert|undo|rv\\b"),
    has_punctuation     = str_detect(revision_comment, "[!?]")
  ) %>%
  select(page_id, starts_with("comment_"), starts_with("mentions_"),
         starts_with("starts_with_"), starts_with("has_"))

# Quick ratio check: bot mean / human mean for each comment feature
comment_features %>%
  bind_cols(is_bot = eng_data$is_bot) %>%
  group_by(is_bot) %>%
  summarise(across(where(is.numeric), mean),
            across(where(is.logical), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(-is_bot) %>%
  pivot_wider(names_from = is_bot, values_from = value, names_prefix = "bot_") %>%
  mutate(ratio = (bot_TRUE + 0.001) / (bot_FALSE + 0.001)) %>%
  arrange(desc(ratio)) %>%
  print()

# -----------------------------------------------------------------------------
# 8. COMBINED MODEL (edit types + comment features)
# -----------------------------------------------------------------------------

# Merge comment features into the model data frame
# (match on position via row_id, since both derive from eng_data)
comment_features_clean <- comment_features %>%
  mutate(across(where(is.logical), as.integer)) %>%
  select(-page_id)

features_combined <- bind_cols(
  features_model,
  comment_features_clean[match(features_model$row_id, seq_len(nrow(eng_data))), ]
)

feat_cols_combined <- features_combined %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

# Prepare train/test matrices (same split as Model A for fair comparison)
train_c   <- features_combined[train_idx, ]
test_c    <- features_combined[test_idx, ]
X_train_c <- train_c %>% select(all_of(feat_cols_combined)) %>% as.matrix()
X_test_c  <- test_c  %>% select(all_of(feat_cols_combined)) %>% as.matrix()

set.seed(42)
cv_model_c <- cv.glmnet(X_train_c, y_train,
                        family       = "binomial",
                        alpha        = 0.5,
                        weights      = train_w,
                        nfolds       = 5,
                        type.measure = "auc")

# -----------------------------------------------------------------------------
# 9. COMPARE MODELS
# -----------------------------------------------------------------------------
# AUC comparison: the combined model gains ~0.08 AUC, largely driven by
# mentions_bot (bots that self-identify in comments). The confusion matrix
# barely changes however — the ~40% of bots without explicit comment signals
# are still missed at the same rate. This motivates treating the two feature
# sets as complementary: comment features catch the "easy" cases, edit-type
# features provide signal for the rest.

pred_prob_c <- predict(cv_model_c, newx = X_test_c,
                       s = "lambda.min", type = "response")[, 1]
pred_bot_c  <- as.integer(pred_prob_c > 0.5)

tp_c <- sum(pred_bot_c == 1 & test_c$is_bot == 1)
fp_c <- sum(pred_bot_c == 1 & test_c$is_bot == 0)
fn_c <- sum(pred_bot_c == 0 & test_c$is_bot == 1)

cat("=======================================================\n")
cat("                  Model A      Model B\n")
cat("                (edit only)  (+ comments)\n")
cat("-------------------------------------------------------\n")
cat(sprintf("AUC (CV):        %.3f         %.3f\n",
            max(cv_model$cvm), max(cv_model_c$cvm)))
cat(sprintf("Precision:       %.3f         %.3f\n",
            tp / (tp + fp), tp_c / (tp_c + fp_c)))
cat(sprintf("Recall:          %.3f         %.3f\n",
            tp / (tp + fn), tp_c / (tp_c + fn_c)))
cat(sprintf("F1:              %.3f         %.3f\n",
            2*tp / (2*tp + fp + fn), 2*tp_c / (2*tp_c + fp_c + fn_c)))
cat("=======================================================\n")

# Top coefficients for combined model
# Positive → bot signal; negative → human signal
coef_c <- coef(cv_model_c, s = "lambda.min")
tibble(term = rownames(coef_c), estimate = as.numeric(coef_c)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(estimate)) %>%
  print(n = 30)

# -----------------------------------------------------------------------------
# 10. USER-LEVEL SPLIT (addressing edit-level leakage)
# -----------------------------------------------------------------------------
# Problem with the current random split: the same user's edits can appear in
# both train and test. The model then partially "recognises" users it has seen,
# inflating performance. A user-level split ensures every editor is either
# entirely in train or entirely in test — simulating real deployment where
# you encounter new, unseen editors.

# Check for any users with inconsistent bot labels (should be none)
mixed_users <- features_combined %>%
  group_by(user_text) %>%
  summarise(n_labels = n_distinct(is_bot), .groups = "drop") %>%
  filter(n_labels > 1)
cat("Users with mixed bot labels:", nrow(mixed_users), "\n")

# How many unique users, and how many are bots?
user_labels <- features_combined %>%
  group_by(user_text) %>%
  summarise(is_bot = max(is_bot), n_edits = n(), .groups = "drop")

cat("Unique users:", nrow(user_labels),
    "| Bot users:", sum(user_labels$is_bot),
    "| Human users:", sum(!user_labels$is_bot), "\n")

# Stratified 80/20 split at the USER level
set.seed(42)
bot_users   <- user_labels %>% filter(is_bot == 1) %>% pull(user_text)
human_users <- user_labels %>% filter(is_bot == 0) %>% pull(user_text)

train_users <- c(sample(bot_users,   size = floor(0.8 * length(bot_users))),
                 sample(human_users, size = floor(0.8 * length(human_users))))

train_u_idx <- which(features_combined$user_text %in% train_users)
test_u_idx  <- which(!features_combined$user_text %in% train_users)

cat("\nTrain edits:", length(train_u_idx),
    "| Bot edits in train:", sum(features_combined$is_bot[train_u_idx]), "\n")
cat("Test edits: ", length(test_u_idx),
    "| Bot edits in test: ", sum(features_combined$is_bot[test_u_idx]), "\n")

# Recompute weights on the new training set
train_u <- features_combined[train_u_idx, ]
test_u  <- features_combined[test_u_idx, ]

n_u       <- nrow(train_u)
n_bot_u   <- sum(train_u$is_bot)
weights_u <- ifelse(train_u$is_bot == 1,
                    n_u / (2 * n_bot_u),
                    n_u / (2 * (n_u - n_bot_u)))

X_train_u <- train_u %>% select(all_of(feat_cols_combined)) %>% as.matrix()
X_test_u  <- test_u  %>% select(all_of(feat_cols_combined)) %>% as.matrix()
y_train_u <- train_u$is_bot

set.seed(42)
cv_model_u <- cv.glmnet(X_train_u, y_train_u,
                        family       = "binomial",
                        alpha        = 0.5,
                        weights      = weights_u,
                        nfolds       = 5,
                        type.measure = "auc")

pred_u     <- predict(cv_model_u, newx = X_test_u,
                      s = "lambda.min", type = "response")[, 1]
pred_bot_u <- as.integer(pred_u > 0.5)

tp_u <- sum(pred_bot_u == 1 & test_u$is_bot == 1)
fp_u <- sum(pred_bot_u == 1 & test_u$is_bot == 0)
fn_u <- sum(pred_bot_u == 0 & test_u$is_bot == 1)

# -----------------------------------------------------------------------------
# FINAL COMPARISON: random split vs. user-level split
# -----------------------------------------------------------------------------
# The gap between these two tells you how much the random split was inflated
# by user leakage. A large drop = the original results were optimistic.
# A small drop = the model genuinely generalises to unseen editors.

cat("================================================================\n")
cat("              Model A     Model B     Model B\n")
cat("             (edit only) (+comments) (user split)\n")
cat("----------------------------------------------------------------\n")
cat(sprintf("AUC (CV):      %.3f       %.3f       %.3f\n",
            max(cv_model$cvm), max(cv_model_c$cvm), max(cv_model_u$cvm)))
cat(sprintf("Precision:     %.3f       %.3f       %.3f\n",
            tp/(tp+fp), tp_c/(tp_c+fp_c), tp_u/(tp_u+fp_u)))
cat(sprintf("Recall:        %.3f       %.3f       %.3f\n",
            tp/(tp+fn), tp_c/(tp_c+fn_c), tp_u/(tp_u+fn_u)))
cat(sprintf("F1:            %.3f       %.3f       %.3f\n",
            2*tp/(2*tp+fp+fn), 2*tp_c/(2*tp_c+fp_c+fn_c),
            2*tp_u/(2*tp_u+fp_u+fn_u)))
cat("================================================================\n")


features_combined %>%
  group_by(user_text, is_bot) %>%
  summarise(n_edits = n(), .groups = "drop") %>%
  group_by(is_bot) %>%
  summarise(
    n_users      = n(),
    mean_edits   = mean(n_edits),
    median_edits = median(n_edits),
    max_edits    = max(n_edits),
    .groups = "drop"
  ) %>%
  mutate(is_bot = ifelse(is_bot == 1, "Bot", "Human"))


# =============================================================================
# Extended Bot Detection — User-Level Features
# English Wikipedia (enwiki)
# Added features:
#   1. inter_edit_seconds  (mean + variance of gap between consecutive edits)
#   2. edit_entropy        (diversity of edit-type operations per user)
#   3. user_page_diversity (number of distinct pages a user touches)
#   4. comment_repetition_rate (share of a user's comments that are duplicates)
#   5. word_insert_remove_ratio (balance of insertions vs. deletions per edit)
#   6. is_pure_structural  (edit changes nodes but zero text — bot maintenance)
# =============================================================================
# ASSUMES: eng_data and features_combined are already in memory from the main
# enwiki script. Run that first, then source this file.
# =============================================================================

library(tidyverse)
library(glmnet)

# -----------------------------------------------------------------------------
# FEATURE 1 & 2: TEMPORAL — inter-edit gap (requires sorted timestamps)
# Computed per user; joined back to edit level.
# -----------------------------------------------------------------------------

# Parse timestamps and sort within each user
temporal <- eng_data %>%
  select(user_text, revision_timestamp) %>%
  mutate(
    ts = as.POSIXct(revision_timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ) %>%
  arrange(user_text, ts) %>%
  group_by(user_text) %>%
  mutate(
    gap_seconds = as.numeric(difftime(ts, lag(ts), units = "secs"))
  ) %>%
  summarise(
    inter_edit_mean = mean(gap_seconds, na.rm = TRUE),   # NA for users with 1 edit
    inter_edit_var  = var(gap_seconds,  na.rm = TRUE),   # NA for users with <= 2 edits
    .groups = "drop"
  ) %>%
  # Users with only 1 edit get NA — fill with a large value (they made no repeated edits)
  mutate(
    inter_edit_mean = replace_na(inter_edit_mean, 86400),  # default: 1 day gap
    inter_edit_var  = replace_na(inter_edit_var,  0)
  )

cat("Temporal features computed for", nrow(temporal), "users\n")

# -----------------------------------------------------------------------------
# FEATURE 3: EDIT ENTROPY — diversity of (node_type × action) per user
# Low entropy = user repeats the same operation (bot-like)
# High entropy = user does many different things (human-like)
# -----------------------------------------------------------------------------

# We need the raw node-edit type counts per edit, then aggregate by user.
# Re-use features_combined which already has all node-edit columns.

node_cols <- features_combined %>%
  select(-row_id, -is_bot, -page_id, -user_text,
         -starts_with("comment_"), -starts_with("mentions_"),
         -starts_with("starts_with_"), -starts_with("has_"),
         -n_word_insert, -n_word_remove, -n_node_edits, -n_text_edits) %>%
  names()

edit_entropy_df <- features_combined %>%
  select(user_text, all_of(node_cols)) %>%
  group_by(user_text) %>%
  summarise(across(all_of(node_cols), sum), .groups = "drop") %>%
  rowwise() %>%
  mutate(
    edit_entropy = {
      counts <- c_across(all_of(node_cols))
      counts <- counts[counts > 0]
      if (length(counts) == 0) {
        0
      } else {
        probs <- counts / sum(counts)
        -sum(probs * log2(probs))   # Shannon entropy in bits
      }
    }
  ) %>%
  ungroup() %>%
  select(user_text, edit_entropy)

cat("Edit entropy computed for", nrow(edit_entropy_df), "users\n")

# -----------------------------------------------------------------------------
# FEATURE 4: USER PAGE DIVERSITY — distinct pages per user
# Bots sweep many pages; humans concentrate on fewer.
# -----------------------------------------------------------------------------

page_diversity <- features_combined %>%
  group_by(user_text) %>%
  summarise(
    user_page_diversity = n_distinct(page_id),
    .groups = "drop"
  )

cat("Page diversity computed for", nrow(page_diversity), "users\n")

# -----------------------------------------------------------------------------
# FEATURE 5: COMMENT REPETITION RATE
# Fraction of a user's comments that are exact duplicates.
# Bots reuse the same templated comment; humans vary.
# -----------------------------------------------------------------------------

comment_rep <- eng_data %>%
  select(user_text, revision_comment) %>%
  group_by(user_text) %>%
  summarise(
    n_comments       = n(),
    n_unique_comments = n_distinct(revision_comment, na.rm = TRUE),
    comment_repetition_rate = 1 - (n_unique_comments / n_comments),
    .groups = "drop"
  ) %>%
  select(user_text, comment_repetition_rate)

cat("Comment repetition rate computed for", nrow(comment_rep), "users\n")

# -----------------------------------------------------------------------------
# JOIN ALL USER-LEVEL FEATURES BACK TO EDIT LEVEL
# -----------------------------------------------------------------------------

user_features <- temporal %>%
  left_join(edit_entropy_df, by = "user_text") %>%
  left_join(page_diversity,  by = "user_text") %>%
  left_join(comment_rep,     by = "user_text")

features_extended <- features_combined %>%
  left_join(user_features, by = "user_text")

cat("User-level features joined. New columns added:\n")
cat(" ", setdiff(names(features_extended), names(features_combined)), "\n")

# -----------------------------------------------------------------------------
# FEATURE 6 (edit-level): WORD INSERT/REMOVE RATIO + IS_PURE_STRUCTURAL
# These are computed directly on existing columns — no new data needed.
# -----------------------------------------------------------------------------

features_extended <- features_extended %>%
  mutate(
    # Ratio: >1 means more insertion than deletion (content creation, human-like)
    #        ~1 means balanced (maintenance, bot-like)
    #        0  means pure deletion
    word_insert_remove_ratio = (n_word_insert + 1) / (n_word_remove + 1),
    
    # Pure structural: node edits happened but zero text changed
    # Almost exclusively bot maintenance behaviour
    is_pure_structural = as.integer(n_text_edits == 0 & n_node_edits > 0)
  )

# Quick signal check: bot vs. human mean for all new features
new_feats <- c("inter_edit_mean", "inter_edit_var", "edit_entropy",
               "user_page_diversity", "comment_repetition_rate",
               "word_insert_remove_ratio", "is_pure_structural")

cat("\n=== New feature signal (bot mean vs. human mean) ===\n")
features_extended %>%
  group_by(is_bot) %>%
  summarise(across(all_of(new_feats), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-is_bot) %>%
  pivot_wider(names_from = is_bot, values_from = value,
              names_prefix = "is_bot_") %>%
  rename(human = is_bot_0, bot = is_bot_1) %>%
  mutate(
    ratio    = (bot + 0.001) / (human + 0.001),
    log2ratio = log2(ratio)
  ) %>%
  arrange(desc(abs(log2ratio))) %>%
  print()

# Visualise signal
features_extended %>%
  group_by(is_bot) %>%
  summarise(across(all_of(new_feats), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-is_bot) %>%
  pivot_wider(names_from = is_bot, values_from = value,
              names_prefix = "is_bot_") %>%
  rename(human = is_bot_0, bot = is_bot_1) %>%
  mutate(
    log2ratio = log2((bot + 0.001) / (human + 0.001)),
    feature   = fct_reorder(name, log2ratio)
  ) %>%
  ggplot(aes(x = feature, y = log2ratio, fill = log2ratio > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Human-skewed", "Bot-skewed")) +
  labs(title    = "New user-level features: bot vs. human signal (enwiki)",
       subtitle = "log2(bot mean / human mean)",
       x = NULL, y = "log2 ratio", fill = NULL) +
  theme_minimal()

# -----------------------------------------------------------------------------
# MODEL C: ALL FEATURES (edit types + comments + new user features)
# Same train/test split as before for fair comparison
# -----------------------------------------------------------------------------

feat_cols_ext <- features_extended %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

train_e <- features_extended[train_idx, ]
test_e  <- features_extended[test_idx, ]

X_train_e <- train_e %>%
  select(all_of(feat_cols_ext)) %>%
  mutate(across(everything(), ~ replace_na(as.numeric(.), 0))) %>%
  as.matrix()

X_test_e <- test_e %>%
  select(all_of(feat_cols_ext)) %>%
  mutate(across(everything(), ~ replace_na(as.numeric(.), 0))) %>%
  as.matrix()

set.seed(42)
cv_model_e <- cv.glmnet(X_train_e, y_train,
                        family       = "binomial",
                        alpha        = 0.5,
                        weights      = train_w,
                        nfolds       = 5,
                        type.measure = "auc")

pred_e     <- predict(cv_model_e, newx = X_test_e,
                      s = "lambda.min", type = "response")[, 1]
pred_bot_e <- as.integer(pred_e > 0.5)

tp_e <- sum(pred_bot_e == 1 & test_e$is_bot == 1)
fp_e <- sum(pred_bot_e == 1 & test_e$is_bot == 0)
fn_e <- sum(pred_bot_e == 0 & test_e$is_bot == 1)

# -----------------------------------------------------------------------------
# FINAL COMPARISON: all four models
# -----------------------------------------------------------------------------

cat("\n=================================================================\n")
cat("         Model A     Model B     Model B     Model C\n")
cat("        (edit only) (+comments) (user split) (+user feats)\n")
cat("-----------------------------------------------------------------\n")
cat(sprintf("AUC:     %.3f       %.3f       %.3f       %.3f\n",
            max(cv_model$cvm), max(cv_model_c$cvm),
            max(cv_model_u$cvm), max(cv_model_e$cvm)))
cat(sprintf("Prec:    %.3f       %.3f       %.3f       %.3f\n",
            tp/(tp+fp), tp_c/(tp_c+fp_c),
            tp_u/(tp_u+fp_u), tp_e/(tp_e+fp_e)))
cat(sprintf("Recall:  %.3f       %.3f       %.3f       %.3f\n",
            tp/(tp+fn), tp_c/(tp_c+fn_c),
            tp_u/(tp_u+fn_u), tp_e/(tp_e+fn_e)))
cat(sprintf("F1:      %.3f       %.3f       %.3f       %.3f\n",
            2*tp/(2*tp+fp+fn), 2*tp_c/(2*tp_c+fp_c+fn_c),
            2*tp_u/(2*tp_u+fp_u+fn_u), 2*tp_e/(2*tp_e+fp_e+fn_e)))
cat("=================================================================\n")

# Top coefficients in Model C
cat("\nTop coefficients — Model C (edit + comment + user features):\n")
coef_e <- coef(cv_model_e, s = "lambda.min")
tibble(term = rownames(coef_e), estimate = as.numeric(coef_e)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(estimate)) %>%
  print(n = 25)

