library(tidyverse)
library(jsonlite)
library(purrr)
library(lubridate)
library(knitr)

# -------------------------------------------------
# Data Loading
# -------------------------------------------------

# Load sample Swedish Wikipedia data
svwiki_sample <- textConnection(
  readLines(gzfile("Data/data/edit_types/svwiki.json.gz"), n = 10000)
)

df <- stream_in(svwiki_sample, verbose = FALSE)

# Basic dataset statistics
missing_pct <- colMeans(is.na(df)) * 100
n_page <- n_distinct(df$page_id)
n_rev  <- n_distinct(df$revision_id)

cat("Unique pages:", n_page, "\n")
cat("Unique revisions:", n_rev, "\n\n")


# -------------------------------------------------
# Data Cleaning
# -------------------------------------------------

cat("### Data Cleaning\n")

cat("
The dataset is cleaned by performing several preprocessing steps:

- removing duplicate revisions
- parsing timestamps into UTC format
- identifying whether edits correspond to revert actions based on Wikipedia revision tags

\n")

df_clean <- df %>%
  distinct(page_id, revision_id, .keep_all = TRUE) %>%
  mutate(
    ts_parsed = ymd_hms(revision_timestamp, tz = "UTC"),
    is_bot_flag = ifelse(is.na(is_bot), FALSE, is_bot),
    revert_flag = map_lgl(
      revision_tags,
      ~ !is.null(.x) &&
        any(c("mw-reverted","mw-rollback","mw-undo","mw-manual-revert") %in% .x)
    )
  )


# -------------------------------------------------
# Bot Quality Diagnostics
# -------------------------------------------------

cat("### Bot Quality Diagnostics\n")

cat("
To understand whether the official Wikipedia bot label behaves as expected,
we compare editing behaviour between accounts labeled as bots and normal users.

The table below summarizes key metrics:

- total number of edits
- number of distinct pages edited
- revert rate
- editing concentration (edits per page)
\n")

bot_quality <- df_clean %>%
  group_by(is_bot_flag) %>%
  summarise(
    n_edits = n(),
    n_pages = n_distinct(page_id),
    revert_rate = mean(revert_flag),
    .groups = "drop"
  ) %>%
  mutate(edits_per_editor = n_edits / n_pages)

kable(
  bot_quality,
  digits = 3,
  caption = "Editing behaviour comparison between bots and humans"
)


# -------------------------------------------------
# Detecting Malicious-like Editors
# -------------------------------------------------

cat("### Detecting Malicious-like Editors\n")

cat("
While official bot accounts usually perform maintenance tasks,
some human editors may display suspicious editing behaviour.

To identify such patterns, we construct a **malicious_bot_like** indicator
based on two user-level metrics:

- the revert rate of an editor
- the concentration of edits per page

Editors exceeding the **80th percentile** in either metric are flagged
as potentially suspicious.
\n")

df_clean <- df_clean %>%
  group_by(user_text) %>%
  mutate(
    edits_per_page = n() / n_distinct(page_id),
    revert_rate_user = mean(revert_flag)
  ) %>%
  ungroup() %>%
  mutate(
    revert_thresh = quantile(revert_rate_user, 0.8, na.rm = TRUE),
    edits_per_page_thresh = quantile(edits_per_page, 0.8, na.rm = TRUE),
    malicious_bot_like =
      (revert_rate_user > revert_thresh) |
      (edits_per_page > edits_per_page_thresh)
  )


# -------------------------------------------------
# Summary of Suspicious Editing Behaviour
# -------------------------------------------------

cat("### Summary of Suspicious Editing Behaviour\n")

cat("
The following table compares editors flagged as **malicious-like**
with normal contributors.

Higher revert rates and concentrated editing behaviour may indicate
potentially problematic editing activity.
\n")

malicious_summary <- df_clean %>%
  group_by(malicious_bot_like) %>%
  summarise(
    n_edits = n(),
    n_pages = n_distinct(page_id),
    revert_rate = mean(revert_flag, na.rm = TRUE),
    edits_per_editor = n_edits / n_pages,
    .groups = "drop"
  ) %>%
  arrange(desc(n_edits))

kable(
  malicious_summary,
  digits = 3,
  caption = "Summary statistics for editors flagged as malicious-like"
)


# -------------------------------------------------
# Hourly Editing Patterns
# -------------------------------------------------

cat("### Hourly Editing Patterns\n")

cat("
Finally, we examine how editing activity varies across the day.

The figure below compares hourly editing patterns for:

- human editors
- official bot accounts
- editors flagged as malicious-like

Differences in temporal behaviour can provide additional insights
into how automated and suspicious editing patterns differ from
normal human activity.
\n")

hourly_edits <- df_clean %>%
  filter(!is.na(ts_parsed)) %>%
  mutate(hour = hour(ts_parsed)) %>%
  group_by(
    user_group = case_when(
      malicious_bot_like ~ "Malicious-like",
      is_bot_flag ~ "Bot",
      TRUE ~ "Human"
    ),
    hour
  ) %>%
  summarise(edits = n(), .groups = "drop")

human_edits <- hourly_edits %>% filter(user_group == "Human")
bot_edits <- hourly_edits %>% filter(user_group == "Bot")
malicious_edits <- hourly_edits %>% filter(user_group == "Malicious-like")


# -------------------------------------------------
# Plot
# -------------------------------------------------

plot(
  human_edits$hour,
  human_edits$edits,
  type = "l",
  col = "blue",
  xlab = "Hour (UTC)",
  ylab = "Number of Edits",
  ylim = c(0, max(hourly_edits$edits)),
  main = "Hourly Edit Patterns by User Type"
)

lines(bot_edits$hour, bot_edits$edits, col = "red")
lines(malicious_edits$hour, malicious_edits$edits, col = "purple")

legend(
  "topleft",
  legend = c("Human", "Bot", "Malicious-like"),
  col = c("blue", "red", "purple"),
  lty = 1,
  cex = 0.7
)
