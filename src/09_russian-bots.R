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

setwd("")

# Read first 3000 rows of the English Wikipedia edit data
ru <- textConnection(readLines(gzfile("ruwiki.json.gz"), n = 20000))
ru_data <- stream_in(ru, verbose = FALSE)
close(ru)

# -----------------------------------------------------------------------------
# 2. PARSE edit_types_json INTO FEATURES
# -----------------------------------------------------------------------------

extract_features <- function(json_str) {
  parsed <- tryCatch(fromJSON(json_str, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  
  node_feats <- list()
  for (ne in parsed[["node-edits"]]) {
    key <- paste0(ne[[1]], "_", ne[[2]])
    node_feats[[key]] <- (node_feats[[key]] %||% 0) + 1
  }
  
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

# FIX: was eng_data — now correctly uses ru_data
features_df <- ru_data$edit_types_json %>%
  map(extract_features) %>%
  map(as_tibble) %>%
  bind_rows(.id = "row_id") %>%
  mutate(
    across(everything(), ~ replace_na(.x, 0)),
    row_id    = as.integer(row_id),
    is_bot    = ru_data$is_bot[row_id],      # FIX
    page_id   = ru_data$page_id[row_id],     # FIX
    user_text = ru_data$user_text[row_id]    # FIX
  )

# -----------------------------------------------------------------------------
# 3. EXPLORATORY: WHICH FEATURES DIFFER MOST BETWEEN BOTS AND HUMANS?
# -----------------------------------------------------------------------------

feat_cols <- features_df %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

mean_by_bot <- features_df %>%
  group_by(is_bot) %>%
  summarise(across(all_of(feat_cols), mean), .groups = "drop") %>%
  pivot_longer(-is_bot, names_to = "feature", values_to = "mean") %>%
  pivot_wider(names_from = is_bot, values_from = mean, names_prefix = "is_bot_") %>%
  rename(human = is_bot_FALSE, bot = is_bot_TRUE) %>%
  mutate(ratio = (bot + 0.001) / (human + 0.001)) %>%
  arrange(desc(ratio))

mean_by_bot %>%
  slice(c(1:10, (n() - 9):n())) %>%
  mutate(feature = fct_reorder(feature, ratio)) %>%
  ggplot(aes(x = feature, y = log2(ratio), fill = ratio > 1)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Human-skewed", "Bot-skewed")) +
  labs(title = "Edit type features: bot vs. human (log2 ratio) — ruwiki",
       x = NULL, y = "log2(bot mean / human mean)", fill = NULL) +
  theme_minimal()

# -----------------------------------------------------------------------------
# 4. MODELLING: ELASTIC NET LOGISTIC REGRESSION
# -----------------------------------------------------------------------------

features_model <- features_df %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  mutate(is_bot = as.integer(is_bot))

feat_cols_clean <- features_model %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

n_total <- nrow(features_model)
n_bot   <- sum(features_model$is_bot)
weights <- ifelse(features_model$is_bot == 1,
                  n_total / (2 * n_bot),
                  n_total / (2 * (n_total - n_bot)))

set.seed(42)
bot_idx   <- which(features_model$is_bot == 1)
human_idx <- which(features_model$is_bot == 0)

train_idx <- c(sample(bot_idx,   size = floor(0.8 * length(bot_idx))),
               sample(human_idx, size = floor(0.8 * length(human_idx))))
test_idx  <- setdiff(seq_len(n_total), train_idx)

train   <- features_model[train_idx, ]
test    <- features_model[test_idx, ]
train_w <- weights[train_idx]

X_train <- train %>% select(all_of(feat_cols_clean)) %>% as.matrix()
y_train <- train$is_bot
X_test  <- test  %>% select(all_of(feat_cols_clean)) %>% as.matrix()

set.seed(42)
cv_model <- cv.glmnet(X_train, y_train,
                      family       = "binomial",
                      alpha        = 0.5,
                      weights      = train_w,
                      nfolds       = 5,
                      type.measure = "auc")

plot(cv_model)

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
# NOTE: Russian bots may self-identify as "бот" (Cyrillic) in addition to
# the Latin "bot". The pattern below catches both.

# FIX: was eng_data — now correctly uses ru_data
comment_features <- ru_data %>%
  mutate(
    comment_length      = nchar(revision_comment),
    comment_is_empty    = is.na(revision_comment) | revision_comment == "",
    comment_nwords      = str_count(revision_comment, "\\S+"),
    mentions_bot        = str_detect(tolower(revision_comment), "bot|\u0431\u043e\u0442"),  # "bot" + "бот"
    mentions_automated  = str_detect(tolower(revision_comment), "automated|script|auto|\u0430\u0432\u0442\u043e"),
    has_wikilink        = str_detect(revision_comment, "\\[\\["),
    starts_with_bracket = str_detect(revision_comment, "^\\["),
    starts_with_slash   = str_detect(revision_comment, "^/"),
    mentions_fix        = str_detect(tolower(revision_comment),
                                     "fix|correct|clean|\u0438\u0441\u043f\u0440\u0430\u0432"),  # + "исправ"
    mentions_revert     = str_detect(tolower(revision_comment),
                                     "revert|undo|rv\\b|\u043e\u0442\u043c\u0435\u043d"),        # + "отмен"
    has_punctuation     = str_detect(revision_comment, "[!?]")
  ) %>%
  select(page_id, starts_with("comment_"), starts_with("mentions_"),
         starts_with("starts_with_"), starts_with("has_"))

# FIX: was eng_data$is_bot — now correctly uses ru_data$is_bot
comment_features %>%
  bind_cols(is_bot = ru_data$is_bot) %>%
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

comment_features_clean <- comment_features %>%
  mutate(across(where(is.logical), as.integer)) %>%
  select(-page_id)

# FIX: match against ru_data row count (not eng_data)
features_combined <- bind_cols(
  features_model,
  comment_features_clean[match(features_model$row_id, seq_len(nrow(ru_data))), ]  # FIX
)

feat_cols_combined <- features_combined %>%
  select(-row_id, -is_bot, -page_id, -user_text) %>%
  names()

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

coef_c <- coef(cv_model_c, s = "lambda.min")
tibble(term = rownames(coef_c), estimate = as.numeric(coef_c)) %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(estimate)) %>%
  print(n = 30)

# -----------------------------------------------------------------------------
# 10. USER-LEVEL SPLIT
# -----------------------------------------------------------------------------

mixed_users <- features_combined %>%
  group_by(user_text) %>%
  summarise(n_labels = n_distinct(is_bot), .groups = "drop") %>%
  filter(n_labels > 1)
cat("Users with mixed bot labels:", nrow(mixed_users), "\n")

user_labels <- features_combined %>%
  group_by(user_text) %>%
  summarise(is_bot = max(is_bot), n_edits = n(), .groups = "drop")

cat("Unique users:", nrow(user_labels),
    "| Bot users:", sum(user_labels$is_bot),
    "| Human users:", sum(!user_labels$is_bot), "\n")

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
# FINAL COMPARISON
# -----------------------------------------------------------------------------

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
    .groups      = "drop"
  ) %>%
  mutate(is_bot = ifelse(is_bot == 1, "Bot", "Human"))

