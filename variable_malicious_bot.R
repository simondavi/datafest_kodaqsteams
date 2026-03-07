library(tidyverse)
library(jsonlite)
library(purrr)
library(lubridate)

# -----------------------------
# Load sample Swedish Wikipedia data
# -----------------------------
svwiki_sample <- textConnection(readLines(gzfile("Data/data/edit_types/svwiki.json.gz"), n = 10000))

df <- stream_in(svwiki_sample, verbose = FALSE)

# -----------------------------
# Data inspection: missing values and counts
# -----------------------------
missing_pct <- colMeans(is.na(df)) * 100          # % of missing values per column
n_page <- n_distinct(df$page_id)                  # number of unique pages
n_rev  <- n_distinct(df$revision_id)              # number of unique revisions

# -----------------------------
# Data cleaning and data quality features 
# -----------------------------
df_clean <- df %>%
  distinct(page_id, revision_id, .keep_all = TRUE) %>%  # remove duplicate revisions per page
  mutate(
    # Parse revision timestamps to UTC
    ts_parsed = ymd_hms(revision_timestamp, tz = "UTC"),
    # Fill missing is_bot values with FALSE
    is_bot_flag = ifelse(is.na(is_bot), FALSE, is_bot),
    # Flag if revision was reverted based on common revert tags
    revert_flag = map_lgl(revision_tags, ~ !is.null(.x) &&
                            any(c("mw-reverted", "mw-rollback", "mw-undo", "mw-manual-revert") %in% .x))
  )

# -----------------------------
# Do bots behave as expected?
# -----------------------------

# --- Bot Quality Diagnostics ---

bot_quality <- df_clean %>%
  # Group by bot flag
  group_by(is_bot_flag) %>%
  summarise(
    n_edits = n(),                                # total edits
    n_pages = n_distinct(page_id),                # page coverage
    revert_rate = mean(revert_flag),              # proportion of reverted edits
    ts_fail_rate = mean(is.na(ts_parsed)),        # timestamp parsing failure
    .groups = "drop"
  )

# --- Hourly edit pattern plot ---

# Aggregate hourly edits
hourly_edits <- df_clean %>%
  filter(!is.na(ts_parsed)) %>%
  mutate(hour = hour(ts_parsed)) %>%
  group_by(is_bot_flag, hour) %>%
  summarise(edits = n(), .groups = "drop")

# Separate bots vs humans
human_edits <- hourly_edits %>% filter(!is_bot_flag)
bot_edits <- hourly_edits %>% filter(is_bot_flag)

# Base R plot
plot(human_edits$hour, human_edits$edits, type="l", col="blue",
     xlab="Hour (UTC)", ylab="Number of Edits", ylim=c(0, max(hourly_edits$edits)),
     main="Hourly Edit Patterns by Bot Flag")
lines(bot_edits$hour, bot_edits$edits, col="red")
legend("topright", legend=c("Human", "Bot"), col=c("blue","red"), lty=1)

bot_quality %>%
  mutate(edits_per_editor = n_edits / n_pages)

# -----------------------------
# Compute a "malicious_bot_like" flag per revision
# Criteria: high revert rate (>10%), many edits per page (>5)
# -----------------------------
# -----------------------------
# Compute "malicious_bot_like" using quantiles
# -----------------------------
df_clean <- df_clean %>%
  group_by(user_text) %>%
  mutate(
    edits_per_page = n() / n_distinct(page_id),
    revert_rate_user = mean(revert_flag)
  ) %>%
  ungroup() %>%
  mutate(
    # 80th percentile thresholds for revert rate and edits per page
    revert_thresh = quantile(revert_rate_user, 0.8, na.rm = TRUE),
    edits_per_page_thresh = quantile(edits_per_page, 0.8, na.rm = TRUE),
    malicious_bot_like = (revert_rate_user > revert_thresh) |
      (edits_per_page > edits_per_page_thresh)
  )

# -----------------------------
# Summary table by our computed "malicious_bot_like"
# -----------------------------

malicious_summary <- df_clean %>%
  group_by(malicious_bot_like) %>%
  summarise(
    n_edits = n(),
    n_pages = n_distinct(page_id),
    revert_rate = mean(revert_flag, na.rm = TRUE),
    ts_fail_rate = mean(is.na(ts_parsed)),
    edits_per_editor = n_edits / n_pages,
    .groups = "drop"
  ) %>%
  arrange(desc(n_edits))

# -----------------------------
# View the final table
# -----------------------------
malicious_summary

# -----------------------------
# Plot again
# -----------------------------

hourly_edits <- df_clean %>%
  filter(!is.na(ts_parsed)) %>%
  mutate(hour = hour(ts_parsed)) %>%
  group_by(user_group = case_when(
    malicious_bot_like ~ "Malicious-like",
    is_bot_flag ~ "Bot",
    TRUE ~ "Human"
  ), hour) %>%
  summarise(edits = n(), .groups = "drop")

# -----------------------------
# Separate groups for plotting
# -----------------------------
human_edits <- hourly_edits %>% filter(user_group == "Human")
bot_edits <- hourly_edits %>% filter(user_group == "Bot")
malicious_edits <- hourly_edits %>% filter(user_group == "Malicious-like")

# -----------------------------
# Base R line plot
# -----------------------------
plot(human_edits$hour, human_edits$edits, type="l", col="blue",
     xlab="Hour (UTC)", ylab="Number of Edits",
     ylim=c(0, max(hourly_edits$edits)),
     main="Hourly Edit Patterns by User Type")
lines(bot_edits$hour, bot_edits$edits, col="red")
lines(malicious_edits$hour, malicious_edits$edits, col="purple")


