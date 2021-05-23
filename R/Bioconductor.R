## ----setup, include = FALSE-----------------------------------------------------------------------
library("lubridate")
library("BiocManager")
library("socialGH")
library("tidyverse")
library("ggrepel")
library("patchwork")

## ----download--------------------------------------------------------------
# repository <- "Bioconductor/Contributions"
# issues <- socialGH::get_issues(repository)
# saveRDS(issues, "output/issues_bioconductor.RDS")
# gt <- lapply(issues$id, get_timelines, repository = repository)
# comments <- do.call(rbind, gt)
# saveRDS(comments, "output/comments_bioconductor.RDS")
issues <- readRDS("output/issues_bioconductor.RDS")
comments <- readRDS("output/comments_bioconductor.RDS")

issues2 <- issues %>% 
  select(-n_comments) %>% 
  filter(!id %in% c(1:5, 51, 587, 764, 1540, 1541)) %>% # Remove testing issues
  mutate(actor = poster, event = list("created"))

comments <- filter(comments, !id %in% c(1:5, 51, 587, 764, 1540, 1541)) # Remove testing issues

posts <- full_join(issues2, comments,  
                   by = c("assignees", "assignee", "label", "state", "locked", 
                          "milestone", "title", "created", "updated", 
                          "association", "text", "id", "closer", "actor", 
                          "event")) %>% 
  arrange(id, created) %>% 
  group_by(id) %>% 
  mutate(event_n = 1:n(),
         event = unlist(event, FALSE, FALSE),
         state = ifelse(event_n == 1, list("opened"), state)) %>% 
  ungroup()

pr <- posts %>% 
  ungroup() %>% 
  filter(event %in% c("merged", "committed")) %>% 
  pull(id)

posts <- posts %>% 
  filter(!id %in% pr) %>% 
  group_by(id) %>% 
  mutate(assignees = if_else(event_n == 1, list(NA), assignees),
         assignee = if_else(event_n == 1, list(NA), assignee),
         label = if_else(event_n == 1, list(NA), label),
         ) %>% 
  ungroup()

github <- str_extract_all(posts$text, "https?://github.com/[:graph:]+/[:graph:]+")
github_urls <- lapply(github, function(x){
  y <- x[!grepl("/commit/|/issues|/blob/|/pull/|/actions|/releases/|/tree/|/projects/|/notifications/|/runs/|/raw/|#", x)]
  y <- tolower(sub("[).,/\">\`]+$", "", y, ignore.case = TRUE))
  unique(sub("^/|\".*$|\\]\\(.*", "", y))
})
repos <- lapply(github_urls, 
                strcapture, 
                pattern = "https?://github.com/((.+)/(.+))",
                proto = data.frame(
                  repo = character(), org = character(), pkg = character()))
posts$repos <- repos

approved <- posts %>% 
  select(id, label, event) %>% 
  unnest(label) %>%
  filter(str_detect(label, "accept")) %>% 
  group_by(id) %>% 
  summarize(approved = sum(event == "labeled") >=  sum(event == "unlabeled"))
posts <- posts %>% 
  full_join(approved, by = "id") %>% 
  mutate(approved = ifelse(is.na(approved), FALSE, TRUE)) %>% 
  group_by(id) %>% 
  mutate(Approved = case_when(
           any(approved) ~ "Yes",
           !any(approved) & sum(event == "closed") == 0 ~ "Ongoing",
           sum(event == "closed") > sum(event == "reopened") ~ "No",
           TRUE ~ "Ongoing")) %>% 
  ungroup()


get_element <- function(x, name) {
  if (!is.null(names(x))) {
    getElement(x, name)
  } else {
    NA_character_
  }
} 

trelative <- function(x) {
  created <- x$created
  event <- x$event
  start <- created[event == "created"]
  k <- event == "closed"
  if (any(k)) {
    closing <- created[which.max(k)]
  } else {
    closing <- max(created)
  }
  
  o <- difftime(created[!is.na(created)], start, units = "days")
  as.numeric(o)
}

reviewers <- function(assigners, unassigners) {
  ta <- table(assigners)
  tu <- table(unassigners)
  y <- 0
  n <- sum(ta) - sum(tu)
  reviewers <- vector("character", n)
  for(reviwer in names(ta)) {
    x <- ta[reviwer]-tu[reviwer]
    if (x >= 1 | is.na(x)){
      y <- y + 1
      reviewers[y] <- reviwer
    }
  }
  reviewers
}

posts <- mutate(posts, 
                reviewer = vapply(assignee, get_element, name = "user", character(1)),
                actor = vapply(actor, get_element, name = "user", character(1L)))
full <- posts %>% 
  nest_by(id) %>% 
  summarize(time_relative = trelative(data), created = data$created, 
            .groups = "drop") %>% 
  inner_join(posts, by = c("id", "created")) %>% 
  distinct() %>% 
  ungroup() %>% 
  select(-locked, -milestone)

saveRDS(full, "output/submissions_bioconductor.RDS")

## ----tidy_issues--------------------------------------------------------------
releases <- data.frame(release = paste0("3.", 3:13),
                       date = as.POSIXct(
                         c("2016/04/04", "2016/10/18", "2017/04/25", 
                           "2017/10/31", "2018/05/01", "2018/10/31", 
                           "2019/05/03", "2019/10/30", "2020/04/28",
                           "2020/10/01", "2021/05/20"), format = "%Y/%m/%d"),
                       stringsAsFactors = FALSE)
scale_data <- scale_x_datetime(
  expand = expansion(add = 10), 
  limits = as.POSIXct(c("2016-06-01", "2021-06-20"), "%Y-%m-%d"))
theme_set(theme_minimal())
col_bioc <- scale_color_gradient(low = "#87b13f", high = "#1a81c2")
fill_bioc <- scale_fill_gradient(low = "#87b13f", high = "#1a81c2")

cut <- 5
full %>% 
  ggplot() +
  geom_point(aes(created, id, 
                 col = fct_lump_n(event, cut),
                 shape = fct_lump_n(event, cut))) +
  geom_vline(xintercept = releases$date, col = "#1a81c2") + # Releases dates
  geom_text(data = releases, aes(x = date, y = c(rep(1200, 5), rep(300, 6)),
                                 label = release)) + # Release dates
  scale_data +
  labs(x = "Events", y = "Issue", col = "Type", shape = "Type",
       title = "Events on issues") +
  scale_color_viridis_d() +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
ggsave("output/bioconductor_events.png")

## ----users_submitting-----------------------------------------------------------------------------
full %>% 
  filter(event == "created") %>% 
  distinct(id, actor, Approved) %>%
  count(actor) %>% 
  count(n, sort = TRUE, name = "nn") %>% 
  ggplot() +
  geom_col(aes(n, nn), col = "#87b13f", fill = "#1a81c2") +
  labs(y = "Contributors", x = "Packages", 
       title = "Number of submissions by user") +
  scale_y_continuous(expand = expansion(add = c(0, 10))) +
  scale_x_continuous(expand = expansion(add = 0.05),
                     breaks = 1:35)
ggsave("output/bioconductor_submission_users.png")
usr_diff_pkg <- full %>% 
  filter(event == "created",
         vapply(repos, NROW, FUN.VALUE = numeric(1L)) == 1) %>% 
  unnest(repos) %>% 
  mutate(package = pkg) %>%
  group_by(actor) %>% 
  distinct(package) %>% 
  count(sort = TRUE) %>% 
  ungroup()

usr_diff_pkg %>% 
  count(n, sort = TRUE, name = "nn") %>% 
  ggplot() +
  geom_col(aes(n, nn), col = "#87b13f", fill = "#1a81c2") +
  labs(y = "Contributors", x = "Packages", 
       title = "Number of different package submissions by user") +
  scale_y_continuous(expand = expansion(add = c(0, 10))) +
  scale_x_continuous(expand = expansion(add = 0.05),
                     breaks = 1:35)
ggsave("output/bioconductor_submission_packages_users.png")

## ----users_ratio----------------------------------------------------------------------------------
usr_ratio <- full %>% 
  group_by(id) %>%
  filter(Approved %in% c("Yes", "No")) %>% 
  filter(event == "created") %>% 
  ungroup() %>% 
  group_by(actor) %>% 
  summarise(ratio = sum(approved)/n()) %>% 
  ungroup() %>% 
  arrange(ratio)

usr_ratio %>% 
  ggplot() +
  geom_bar(aes(ratio, fill = ratio)) +
  col_bioc +
  labs(title = "Authors success submitting packages",
       x = "Success ratio", y = "Users")
ggsave("output/bioconductor_success_ratio_users.png")


## ----users----------------------------------------------------------------------------------------
usr_success <- usr_diff_pkg %>% 
  inner_join(usr_ratio, by = "actor")

ggplot(usr_success) +
  geom_count(aes(n, ratio, col = ratio)) +
  col_bioc +
  labs(x = "Packages", y = "Approval success ratio", size = "Users",
       title = "Submitting more packages increases approval rate",
       col = "Success ratio") +
  scale_x_continuous(expand = expansion(add = 0.5), breaks = c(1:12, 33)) +
  theme(panel.grid.minor.x = element_blank())
ggsave("output/bioconductor_submission_success_ratio.png")

## ----users_progression----------------------------------------------------------------------------
# Date progression for users that submit more than once (the previous plot is the final snapshot)
usr_diff_pkg2 <- rename(usr_diff_pkg, diff_pkg = n)

usr_submission_pkg <- full %>% 
  filter(event == "created") %>% 
  filter(Approved %in% c("Yes", "No")) %>% 
  unnest(repos) %>% 
  filter(pkg != "yourpackagename") %>% 
  group_by(actor) %>% 
  arrange(id) %>% 
  mutate(n = n(),
         user_submission = 1:n()) %>% 
  filter(n != 1) %>% 
  ungroup() %>% 
  select(actor, user_submission, created, pkg, Approved)

usr_submission_pkg %>% 
  group_by(user_submission) %>% 
  summarize(ratio = sum(Approved == "Yes")/n(),
            n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_path(aes(user_submission, ratio, size = n, col = ratio)) +
  geom_point(aes(user_submission, ratio, size = n-5, col = ratio), show.legend = FALSE) +
  col_bioc +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(limits = c(0, 1))  +
  labs(title = "User submissions", y = "Approval ratio", 
       x = "Number of submission", size = "Users") +
  guides(col = FALSE) + 
  theme(panel.grid.minor.x = element_blank())
ggsave("output/bioconductor_approval_submissions.png")

## ----submissions----------------------------------------------------------------------------------
full %>% 
  filter(event == "created",
         Approved %in% c("Yes", "No")) %>% 
  group_by(year = year(created), Approved) %>% 
  summarise(n = n(), .groups = "keep") %>%
  ungroup() %>% 
  ggplot() +
  geom_col(aes(year, n, fill = Approved))  +
  labs(title = "Yearly submissions and approval", 
       y = "Issues", x = element_blank(), 
       fill = "Approval ratio")
ggsave("output/bioconductor_global_submission_rate.png")

## ----reviewers, include=FALSE---------------------------------------------------------------------
full %>% 
  filter(Approved != "Ongoing",
         !is.na(reviewer)) %>% 
  group_by(reviewer) %>% 
  count(Approved) %>% 
  ungroup() %>% 
  mutate(ratio = n/sum(n)) %>% 
  arrange(-ratio, n) %>% 
  filter(n > 10) %>% 
  pivot_wider(id_cols = c("reviewer"),
              values_from = c(n, ratio),
              names_from = "Approved",
              values_fill = 0)

full %>%
  group_by(id) %>% 
  mutate(n_reviewers = sum(!is.na(reviewer))) %>% 
  ungroup() %>% 
  group_by(n_reviewers) %>% 
  count(Approved) %>% 
  ungroup() %>% 
  filter(n_reviewers != 0) %>% 
  mutate(ratio = n/sum(n)) %>% 
  group_by(Approved) %>% 
  summarise(fr = sum(ratio))


unique_na <- function(x) {
  y <- unique(x)
  y[!is.na(y)]
}
## ----reviewers_frequency--------------------------------------------------------------------------
normal_reviews <- full %>% 
  group_by(id) %>% 
  summarize(n_reviewers = sum(!is.na(reviewer)),
         n_labels = sum(event == "labeled") - sum(event == "unlabeled"),
         n_comments = sum(event == "commented") - sum(event == "comment_deleted"),
         closed = any(event == "closed"),
         time_opened = time_relative[which.max(event == "closed")],
         reviewer = list(unique_na(reviewer))) %>% 
  ungroup() %>% 
  filter(n_reviewers == 1, n_labels >= 1, n_comments > 1,
         !is.na(closed)) %>% 
  select(-n_reviewers) %>% 
  mutate(reviewer = unlist(reviewer))
normal_reviews <- normal_reviews %>% 
  left_join(full[full$event == "created", ], by = "id") %>% 
  select(-reviewer.y) %>% 
  rename(reviewer = reviewer.x)

top_reviewers <- normal_reviews %>% 
  count(reviewer, sort = TRUE) %>% 
  filter(reviewer > 50) %>% 
  pull(reviewer)

normal_reviews %>% 
  mutate(year = year(created)) %>% 
  group_by(year) %>% 
  count(reviewer) %>% 
  mutate(share = n/sum(n),
         reviewer = fct_reorder2(reviewer, year, n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(year, share, col = reviewer)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(x = element_blank(), y = element_blank(),
       title = "Share of issues reviewed",
       linetype = "Reviewer", col = "Reviewer")
ggsave("output/bioconductor_reviewers_share.png")


## ----reviewer_comments----------------------------------------------------------------------------
approval_reviewers <- normal_reviews %>% 
  group_by(reviewer) %>% 
  count(Approved) %>% 
  mutate(ratio = n/sum(n), total = sum(n),
         Reviewer = fct_reorder(reviewer, ratio)) %>% 
  ungroup() %>% 
  filter(Approved == "Yes", total  > 50)

approval_reviewers %>% 
  ggplot() +
  geom_point(aes(total, ratio, col = reviewer), size = 3) + 
  geom_hline(yintercept = median(approval_reviewers$ratio), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Issues handled", y = "Approval ratio",
       title = "Reviewers approval Ratio", col = "Reviewer")
ggsave("output/bioconductor_approval_ratio.png")

## ----reviewers_time-------------------------------------------------------------------------------
breaks <- function(limits) {
  seq(from = 0, to = floor(limits[2]), by = 100)
}

normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(reviewer, time_opened, 
                                   col = Approved, shape = Approved), 
                               size = 0.75) +
  labs(y = "Days open", title = "Time open by reviewers",
       x = element_blank())
ggsave("output/bioconductor_open_time_reviewers.png")

# Focusing a bit

normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(reviewer, time_opened, 
                                   col = Approved, shape = Approved)) +
  geom_hline(yintercept = median(normal_reviews$time_opened), 
             linetype = "dashed") +
  coord_cartesian(ylim = c(0, 150)) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  labs(y = "Days open", title = "Time open by reviewers", 
       subtitle = "A zoom", x = element_blank())
ggsave("output/bioconductor_open_time_reviewers_zoom.png")


## ----reviewer_time_diff---------------------------------------------------------------------------
reviewer_time <- normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  group_by(reviewer) %>% 
  summarise(m = median(time_opened), me = mean(time_opened), n = n()) %>% 
  ungroup() %>% 
  arrange(m)

global_medians_time <- normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  group_by(Approved) %>% 
  summarise(m = median(time_opened), me = mean(time_opened), n = n()) %>% 
  filter(Approved != "Ongoing")

reviewers_time_approved <- normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  group_by(reviewer, Approved) %>% 
  summarise(m = median(time_opened), 
            me = mean(time_opened), 
            s = sd(time_opened),
            n = n(),
            sem = sqrt(pi/2)*s/sqrt(n)) %>% # https://stats.stackexchange.com/q/59838/105234
  ungroup() %>% 
  mutate(reviewer = fct_relevel(reviewer, reviewer_time$reviewer)) %>% 
  filter(Approved != "Ongoing")

reviewers_time_approved %>% 
  ggplot() +
  geom_hline(data = global_medians_time, 
             aes(yintercept = m, col = Approved), linetype = "dotted") +
  geom_point(data = reviewer_time, aes(fct_relevel(reviewer, reviewer), 
                                       m, size = n)) +
  geom_point(aes(reviewer, m, col = Approved, group = Approved, shape = Approved, size = n)) +
  geom_errorbar(aes(x = reviewer, group = Approved, ymin = m-sem, ymax = m+sem, col = Approved), width = 0.2) +
  labs(x = element_blank(), y = "Days (median)", 
       title = "Reviewers speed to close", 
       subtitle = "In black all reviews together. Errorbars are the standard error of the median",
       size = "Reviews") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(add = c(1, 0)), 
                     breaks = seq(from = 0, to = 130, by = 20)) +
  scale_shape_manual(values = c(15, 17)) +
  scale_x_discrete(expand = expansion(add = 0.1))
ggsave("output/bioconductor_reviewers_speed.png")


## ----reviewers_comments---------------------------------------------------------------------------
normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(reviewer, n_comments, 
                                   col = Approved, shape = Approved), 
                               size = 0.75) +
  
  labs(y = "Comments", title = "Comments on the issue", x = element_blank())
ggsave("output/bioconductor_comments_reviwers.png")

## ----reviewer_comment_diff------------------------------------------------------------------------
reviewer_comments <- normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  group_by(reviewer) %>% 
  summarise(m = median(n_comments), me = mean(n_comments), n = n()) %>% 
  ungroup() %>% 
  arrange(m)

global_medians_comments <- normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  group_by(Approved) %>% 
  summarise(m = median(n_comments), me = mean(n_comments), n = n()) %>% 
  filter(Approved != "Ongoing")

reviewers_comment_approved <- normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  group_by(reviewer, Approved) %>% 
  summarise(m = median(n_comments), 
            me = mean(n_comments), 
            s = sd(n_comments),
            n = n(),
            sem = sqrt(pi/2)*s/sqrt(n)) %>% 
  ungroup() %>% 
  mutate(reviewer = fct_relevel(reviewer, reviewer_comments$reviewer)) %>% 
  filter(Approved != "Ongoing")

reviewers_comment_approved %>% 
  ggplot() +
  geom_hline(data = global_medians_comments, 
             aes(yintercept = m, col = Approved), linetype = "dotted") +
  geom_point(data = reviewer_comments, aes(fct_relevel(reviewer, reviewer), 
                                           m, size = n)) +
  geom_point(aes(reviewer, m, col = Approved, fill = Approved, shape = Approved, size = n)) +
  geom_errorbar(aes(x = reviewer, ymin = m-sem, ymax = m+sem, col = Approved), width = 0.2) +
  labs(x = element_blank(), y = "Comments (median)", 
       title = "Comments on the issues", 
       subtitle = "In black all reviews together. Errorbars indicate standard error of the median. ",
       size = "Reviews") +
  scale_y_continuous(limits = c(0, 70), expand = expansion(add = c(1, 0)), 
                     breaks = seq(from = 0, to = 70, by = 20)) +
  scale_shape_manual(values = c(15, 17)) + 
  scale_x_discrete(expand = expansion(add = 0.1))
ggsave("output/bioconductor_comments_approval.png")

## ----acceptance_comments, include = FALSE---------------------------------------------------------
normal_reviews %>%  
  group_by(Approved) %>% 
  summarise(median = median(n_comments), mean = mean(n_comments),
            s = sd(n_comments),
            n = n(),
            sem = sqrt(pi/2)*s/sqrt(n)) %>% 
  ungroup() %>% 
  select(Approved, median, sem) %>% 
  knitr::kable(col.names = c("Approved", "Median comments", "Standard error of the median"),
               caption = "Reviews comments", 
               align = "c")


## ----reviewers_comments_time----------------------------------------------------------------------
normal_reviews %>% 
  filter(reviewer  %in% top_reviewers) %>% 
  ggplot() +
  geom_point(aes(time_opened, n_comments, col = Approved, shape = Approved), 
             size = 0.75) +
  scale_x_continuous(breaks = breaks, guide = guide_axis(check.overlap = TRUE)) +
  scale_y_continuous(breaks = breaks) +
  facet_wrap(~reviewer, scales = "free") +
  labs(x = "Days opened", y = "Comments", 
       title = "Comments and open days per reviewer")
ggsave("output/bioconductor_reviewers_comments_time_open.png")


## ----submission_rate------------------------------------------------------------------------------
full %>% 
  filter(event == "created") %>% 
  mutate(md  = as.numeric(format(created, "%j")),
         year = year(created)) %>% 
  group_by(md) %>% 
  summarise(n = median(n())) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_vline(xintercept = as.numeric(format(releases$date, "%j")), alpha = 0.5,
             linetype = "dashed") +
  geom_point(aes(md, n), col = "#1a81c2") + 
  geom_smooth(aes(md, n), span = 0.25, col = "#87b13f") +
  labs(title = "Median daily submissions", 
       x = "Day of year", y = "Issues opened") +
  scale_x_continuous(expand = expansion(add = 2)) +
  scale_y_continuous(expand = expansion(add = 1))
ggsave("output/bioconductor_daily_submission.png")

## ----margin_submission----------------------------------------------------------------------------
release_attempt <- function(x, release = releases) {
  diff_time <- difftime(x, release$date, units = "days")
  pre_release <- diff_time < 0
  pick <- which(diff_time[pre_release] == max(diff_time[pre_release]))
  data.frame("release" = release$release[pre_release][pick], 
             "margin" = abs(diff_time[pre_release][pick]), stringsAsFactors = FALSE)
}

ra <- lapply(normal_reviews$created, release_attempt)
rs <- do.call("rbind", ra)
df2 <- normal_reviews %>% 
  mutate(release = rs$release, margin = rs$margin,
         devel = release == "3.14" & approved & margin < 30)
df2 %>% 
  ggplot() +
  geom_histogram(aes(margin, fill = margin, col = margin), bins = 40) +
  geom_vline(xintercept = 30) +
  col_bioc +
  scale_y_continuous(expand = expansion(add = c(0, 5))) +
  scale_x_continuous(expand = expansion())  + 
  labs(title = "Days till next release", y = "Issues", x = "Days") +
  guides(fill = FALSE, col = FALSE )
ggsave("output/bioconductor_days_submission.png")

## ----margin_submission_accepted, include=FALSE----------------------------------------------------
df2 %>% 
  filter(margin < 30) %>% 
  count(Approved) %>% 
  knitr::kable(col.names = c("Approved", "Issues"), 
               caption = "Packages submitted 30 days before a release", 
               align = "c")
df2 %>% 
  filter(margin > 30) %>% 
  count(Approved) %>% 
  knitr::kable(col.names = c("Approved", "Issues"),
               caption = "Packages submitted more than 30 days before a release", 
               align = "c")


## ----worst_scenario-------------------------------------------------------------------------------
latest_submission <- max(df2$margin, na.rm = TRUE)

df2 %>% 
  filter(!(time_opened == 0 & !approved)) %>% 
  ggplot() +
  geom_vline(xintercept = 30) +
  geom_hline(yintercept = latest_submission*c(1:2), col = "darkgrey") +
  geom_point(aes(margin, time_opened, col = Approved, shape = Approved)) +
  geom_point(data = ~filter(.x, devel), aes(margin, time_opened), col = "grey") +
  ggplot2::annotate(geom = "rect", xmin = 0, ymin = latest_submission,
                    xmax = max(df2$margin, na.rm = TRUE), ymax = max(df2$time_opened) + 10, 
                    fill = "orange", alpha = 0.25) +
  ggplot2::annotate(geom = "rect", xmin = 0, ymin = 0,
                    xmax = 30, ymax = max(df2$time_opened) + 10, 
                    fill = "red", alpha = 0.25) +
  ggplot2::annotate(geom = "text", x = 50, y = 180, 
                    label = "Missed release") +
  ggplot2::annotate(geom = "text", x = 13, y = 250, 
                    label = "Submitted right before a release", 
                    angle = 90, hjust = 0, vjust = 1) +
  scale_x_continuous(expand = expansion(add = 1)) +
  scale_y_continuous(expand = expansion(add = 9)) +
  scale_color_discrete(na.value = "grey") +
  labs(x = "Days till release", y = "Days open",
       title = "Packages not closed the same day as submitted",
       subtitle = "In red the worse time to submit. Each horitzonal bar indicates a missed release")
ggsave("output/bioconductor_days_till_release_days_open.png")

## ----open_releases--------------------------------------------------------------------------------
df2 %>% 
  ggplot() +
  geom_point(aes(created, time_opened, col = Approved, shape = Approved)) +
  geom_vline(data = releases, aes(xintercept = date), alpha = 0.5, 
             col = "darkgreen") + # Release dates
  geom_text(data = releases, aes(x = date, y = rep(300, 11),
                                 label = release), col = "#1a81c2") + # Release dates
  scale_y_continuous(expand = expansion(add = 6)) +
  scale_data +
  labs(x = element_blank(), y = "Days open", 
       title = "Time open") 
ggsave("output/bioconductor_days_open.png")

## ----pkg_source-----------------------------------------------------------------------------------
df2 %>% 
  mutate(repos = lapply(repos, function(x){if(length(x) != 1){x[1, ]}})) %>% 
  unnest(repos) %>% 
  group_by(same_submitter = ifelse(org != actor, "No", "Yes"))  %>% 
  filter(!is.na(same_submitter)) %>% 
  count(Approved) %>% 
  mutate(ratio = n/sum(n), total = sum(n), pos = paste0(n, collapse = "/")) %>% 
  ggplot() +
  geom_col(aes(fct_relevel(same_submitter, c("Yes", "No")), n, fill = Approved)) +
  geom_text(aes(fct_relevel(same_submitter, c("Yes", "No")), total*1.05, label = pos)) +
  labs(x = "Repository belongs to submitter", y = "Submissions",
       title = "Success if repository belongs to the submitter",
       subtitle = "The fraction indicates: Not approved/approved")
ggsave("output/bioconductor_org_pkg_submissions.png")

## ----time_rejected--------------------------------------------------------------------------------
df2 %>% 
  filter(Approved == "No") %>% 
  ggplot() + 
  geom_histogram(aes(time_opened), bins = 50) +
  # fill_bioc +
  labs(x = "Days open", y = "Issues", 
       title = "Most not approved packages are closed the same day") 
ggsave("output/bioconductor_open_time_rejected.png")

## ----repolink-------------------------------------------------------------------------------------
rejected <- filter(df2, Approved == "No")

## ----repolinks------------------------------------------------------------------------------------
rejected %>%
  mutate(repos = lapply(repos, function(x){if(length(x) != 1){x[1, ]}})) %>% 
  unnest(repos) %>% 
  count(pkg, sort = TRUE) %>% 
  head() %>% 
  knitr::kable(col.names = c("Name", "Packages"), 
               caption = "Multiple submission for the same package",
               align = "c")


## ----comments-------------------------------------------------------------------------------------
rejected %>% 
  count(n_comments, sort = TRUE) %>% 
  head(10) %>% 
  knitr::kable(col.names = c("Comments", "Submissions"), 
               caption = "Comments on not approved submissions", 
               align = "c")


## ----labels---------------------------------------------------------------------------------------
rejected %>% 
  count(n_labels) %>% 
  knitr::kable(col.names = c("Number of labels", "Number of issues"),
               caption = "Labels of rejected packages",
               align = "c")

# --- Part 2 ###--------------------------------------


## ----by_issue-------------------------------------------------------------------------------------
by_issue <- full %>% 
  group_by(id) %>% 
  summarize(time_window = difftime(max(created), min(created), units = "days"),
            events = n(), 
            diff_users = n_distinct(actor),
            diff_events = n_distinct(event),
            Approved = unique(Approved),
            approved = unique(approved),
            assignments = sum(event %in% "assigned"),
            reassigned = any(event %in% "unassigned"),
            assigners = list(reviewer[event %in% "assigned"]),
            unassigners = list(reviewer[event %in% "unassigned"]),
            reviewers = list(reviewers(unlist(assigners, FALSE, FALSE), 
                                    unlist(unassigners, FALSE, FALSE))),
            reviewer_comments = sum(event == "commented" & 
                                      actor %in% unlist(reviewers), na.rm = TRUE),
            closers = list(actor[event == "closed"]),
            openers = list(actor[event == "reopened"]),
            closed = sum(event == "closed") >= sum(event == "reopened") & any(event == "closed"),
            closer = list(setdiff(unlist(closers, FALSE, FALSE), 
                                    unlist(openers, FALSE, FALSE))),
            labels_added = list(label[event == "labeled"]),
            labels_removed = list(label[event == "unlabeled"]),
            labels_final = list(setdiff(unlist(labels_added, FALSE, FALSE), 
                                    unlist(labels_removed, FALSE, FALSE))),
            check_labels = all(unlist(labels_final, FALSE, FALSE) %in%
                                 label[event == "created"]),
            submitter = actor[event == "created"]
            
) %>% 
  mutate(n_reviewers = lengths(reviewers),
         n_closers = lengths(closer))

by_issue1 <- full %>% 
  group_by(id) %>% 
  count(event) %>% 
  filter(event != "created") %>% 
  pivot_wider(values_from = n, names_from = event, values_fill = 0) %>%
  nest_by(.key = "event")
by_issue2 <- full %>% 
  group_by(id) %>% 
  count(actor) %>% 
  pivot_wider(values_from = n, names_from = actor, values_fill = 0) %>% 
  nest_by(.key = "actor")

by_issue <- by_issue %>% 
  inner_join(by_issue1, by = "id") %>% 
  inner_join(by_issue2, by = "id")


## ----by_user--------------------------------------------------------------------------------------
by_user0 <- full %>%
  group_by(actor) %>% 
  summarize(
    actions = n(),
    issues_participated = n_distinct(id),
    issues = list(unique(id)),
    events_participated = n_distinct(event),
  ) %>% 
  mutate(is_reviewer = actor %in% unlist(by_issue$reviewers, FALSE, FALSE))

by_user1 <- full %>% 
  group_by(actor) %>% 
  count(event) %>% 
  pivot_wider(values_from = n, names_from = event, values_fill = 0) %>% 
  nest_by(.key = "event")
by_user2 <- full %>% 
  group_by(actor) %>% 
  count(id) %>% 
  pivot_wider(values_from = n, names_from = id, values_fill = 0) %>% 
  nest_by(.key = "ids")

by_user <- by_user0 %>% 
  full_join(by_user1, by = "actor") %>% 
  full_join(by_user2, by = "actor")

by_user %>% 
  filter(is_reviewer) %>%
  unnest(event) %>% 
  filter(commented != 0) %>% 
  ggplot() + 
  geom_point(aes(actions, issues_participated)) +
  geom_text_repel(aes(actions, issues_participated, label = actor)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(size = "Users", col = "Different events", x = "Comments", y = "Issues",
       title = "Reviewers involvement") +
  theme_minimal()
ggsave("output/bioconductor_reviewers_comments.png")

## ----by_user2-------------------------------------------------------------------------------------
by_user %>% 
  filter(!is_reviewer & actor != "bioc-issue-bot" & !is.na(actor)) %>%
  unnest(event) %>% 
  filter(commented != 0) %>% 
  ggplot() + 
  geom_count(aes(actions, issues_participated)) +
  scale_color_viridis_d() + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_radius(breaks = c(1, 10, 20, 30, 40)) +
  geom_text_repel(aes(actions, issues_participated, label = actor), 
                data = . %>% filter(issues_participated > 10 | actions > 100)) +
  labs(size = "Users", col = "Different events", x = "Comments", y = "Issues",
       title = "Users involvement") +
  theme_minimal()
ggsave("output/bioconductor_users_comments.png")

## ----by_event-------------------------------------------------------------------------------------
by_event1 <- full %>% 
  group_by(event) %>% 
  count(actor) %>% 
  pivot_wider(names_from = actor, values_from = n, values_fill = 0) %>% 
  nest_by(.key = "actor")
by_event2 <- full %>% 
  group_by(event) %>% 
  count(id) %>% 
  pivot_wider(names_from = id, values_from = n, values_fill = 0) %>% 
  nest_by(.key = "id") 

by_event <- full %>% 
  group_by(event) %>% 
  summarize(
    n = n(),
    ids = list(unique(id)),
    actors = list(unique(actor)),
    diff_id = n_distinct(id),
    diff_actor = n_distinct(actor))
by_event <- by_event %>% 
  inner_join(by_event1, by = "event") %>% 
  inner_join(by_event2, by = "event")


## ----by_event_actor_id----------------------------------------------------------------------------
by_actor_event_id <- full %>% 
  group_by(event, actor, id) %>% 
  count()


## ----events_view----------------------------------------------------------------------------------
by_event %>% 
  ggplot() + 
  geom_abline(slope = 1, col = "gray") +
  geom_text_repel(aes(diff_id, diff_actor, label = event, size = n, col = n)) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Events", x = "Number of different issues", 
       y = "Number of different users", label = "Events", size = "Events",
       col = "Events") +
  scale_color_binned(trans = "log10", guide = "bins") +
  scale_size(trans = "log10") +
  theme_minimal()
ggsave("output/bioconductor_events.png")

## ----second_plot----------------------------------------------------------------------------------
full %>% 
  group_by(id) %>% 
  filter(event != "created") %>% 
  count(event) %>% 
  ungroup() %>% 
  arrange(id, event, n) %>% 
  ggplot() +
  geom_tile(aes(id, fct_reorder(event, n, .fun = sum), col = n)) +
  scale_color_continuous(expand = expansion(), trans = "log10", 
                         high = "#132B43", low = "#56B1F7") +
  labs(y = element_blank(), x = "Issue", title = "Events per issue", 
       col = "Times")
ggsave("output/bioconductor_events_issues.png")

## ----events_time----------------------------------------------------------------------------------
unit <- "days"
full %>% 
  group_by(id) %>% 
  filter(event_n == max(event_n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_histogram(aes(event_n), bins = 40) +
  labs(x = "Events", y = "Issues", title = "Events per issue") + 
  theme_minimal()
ggsave("output/bioconductor_events_issue.png")

## ----events_time2---------------------------------------------------------------------------------
diff_time <- full %>% 
  group_by(id) %>% 
  summarise(open_time = difftime(max(created), min(created), units = unit),
            n = max(event_n), 
            id = unique(id),
            slope = n/as.numeric(open_time)) %>% 
  ungroup()
diff_time %>%   
  ggplot() +
  geom_point(aes(open_time, n, col = slope)) +
  scale_color_continuous(expand = expansion(), trans = "log10",
                         high = "#132B43", low = "#56B1F7") +
  labs(y = "Events", x = glue::glue("Time ({unit})"), 
       col = glue::glue("Events per {unit}"), 
       title = "Number of events and time") +
  scale_x_continuous(breaks = 1:7*365, labels = function(x) {paste(x/365, "year")}) + 
  theme_minimal() +
  theme(axis.text.x = element_text())
ggsave("output/bioconductor_events_time.png")

## ----events_time3---------------------------------------------------------------------------------
diff_time %>% 
  filter(open_time <= median(diff_time$open_time)) %>% 
  ggplot() +
  geom_point(aes(open_time, n, col = slope)) +
  scale_color_continuous(expand = expansion(), trans = "log10", 
                         high = "#132B43", low = "#56B1F7") +
  labs(y = "Events", 
       x = glue::glue("Time ({unit})"), 
       col = glue::glue("Events per {unit}"), 
       title = "Number of events and time",
       subtitle = "A zoom to the fastest half") +
  scale_x_continuous(breaks = 1:7*7, labels = function(x) {paste(x/7, "weeks")}) + 
  theme_minimal() +
  theme(axis.text.x = element_text()) 
ggsave("output/bioconductor_events_time_zoom.png")

## ----assignments----------------------------------------------------------------------------------
by_issue %>% 
  ggplot() +
  geom_count(aes(n_reviewers, assignments, col = reassigned, 
                 shape = reassigned)) +
  labs(x = "Final reviewers", y = "Assigned", title = "Reviewers", 
       col = "Reassigned?", shape = "Reassigned?", size = "Submissions") +
  scale_color_brewer(labels = c("No", "Yes"), type = "qual") +
  scale_shape(labels = c("No", "Yes")) +
  scale_size(trans = "log10")
ggsave("output/bioconductor_reviewers_change.png")

## ----open_close2----------------------------------------------------------------------------------
by_issue %>% 
  filter(Approved %in% c("No", "Yes")) %>% 
  rowwise(id) %>% 
  mutate(n_openers = n_distinct(openers),
         n_closers = n_distinct(closers)) %>% 
  ggplot() +
  geom_count(aes(n_closers, n_openers)) +
  scale_radius(trans = "log10") +
  facet_wrap(~Approved, scales = "free") +
  labs(x = "Closers", y = "Openers", title = "Issues closers and reopeners", 
       subtitle = "Approved?", size = "Submissions")
ggsave("output/bioconductor_change_effect.png")

## ----submission_acceptance------------------------------------------------------------------------
revi <- filter(by_issue, lengths(reviewers) != 0)

reviwer_didnt_close <- revi %>% 
  filter(!is.na(closer),
         closer %in% unlist(reviewers, FALSE, FALSE)) %>% 
  pull(id)

author_closed <- revi %>% 
  filter(!is.na(closer), submitter %in% closer) %>% 
  pull(id)

revi_sum <- revi %>% 
  filter(!id %in% author_closed,
         n_reviewers == 1) %>% 
  mutate(reviewer = unlist(reviewers, FALSE, FALSE),
         reviewer_commented = ifelse(reviewer_comments != 0, "commented", "not commented")) %>% 
  group_by(reviewer, reviewer_commented, Approved) %>% 
  count() %>% 
  group_by(reviewer) %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(reviewer, reviewer_commented, Approved)

revi %>% 
  ggplot() +
  geom_jitter(aes(Approved, reviewer_comments, 
                  col = Approved, shape = Approved), height = 0) +
  facet_wrap(~n_reviewers, drop = TRUE) +
  labs(x = element_blank(), y = "Reviewers comments", title = "Comments from reviewers", size = "Issues",
       subtitle = "Reviewers assigned:") + 
  theme_minimal()
ggsave("output/bioconductor_reviewers_comments.png")

## ----reviews--------------------------------------------------------------------------------------
revi %>% 
  mutate(comments = unlist(map(event, pull, commented), FALSE, FALSE)) %>% 
  ggplot() +
  geom_count(aes(comments, reviewer_comments, 
                  col = Approved, shape = Approved)) +
  labs(y = "Reviewer's comments", x = "Comments", size = "Issues",
       title = "Comments from reviewers") + 
  facet_wrap(~Approved, scales = "free") +
  theme_minimal()
ggsave("output/bioconductor_comments_reviewers_others.png")

## ----submission_acceptance2-----------------------------------------------------------------------
ord_rev <- revi_sum %>% 
  summarise(total = sum(n)) %>% 
  arrange(-total) %>% 
  pull(reviewer) %>% .[1:8]

r <- revi_sum %>% 
  mutate(Approved = ifelse(Approved == "Yes", "Approved", "Rejected"),
         reviewer = as.factor(reviewer)) %>% 
  filter(reviewer %in% ord_rev) %>% 
  mutate(reviewer = fct_drop(reviewer)) %>% 
  mutate(reviewer = fct_relevel(reviewer, !!!ord_rev)) %>% 
  ungroup() %>% 
  nest_by(Approved, reviewer_commented) %>% 
  mutate(plot_relative = list(
    ggplot(data) +
      geom_col(aes(perc, reviewer)) +
      labs(title = paste(Approved, reviewer_commented, sep = " and "), 
           y = element_blank(), x = element_blank()) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_discrete(drop = FALSE) +
      scale_fill_brewer(type =  "qual")),
    plot_abs = list(
    ggplot(data) +
      geom_col(aes(reviewer, n)) + 
      coord_flip() +
      labs(title = paste(Approved, reviewer_commented, sep = " and "), 
           x = element_blank(),  y = element_blank()) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_brewer(type =  "qual"))
  )

r$plot_relative[[2]] <- r$plot_relative[[2]] + theme(axis.text.y = element_blank())
r$plot_relative[[4]] <- r$plot_relative[[4]] + theme(axis.text.y = element_blank())
ptch <- wrap_plots(r$plot_relative)
ptch  
ggsave("output/bioconductor_reviewers_ratios.png")

r$plot_abs[[2]] <- r$plot_abs[[2]]  + theme(axis.text.y = element_blank())
r$plot_abs[[4]] <- r$plot_abs[[4]]  + theme(axis.text.y = element_blank())
ptch2 <- wrap_plots(r$plot_abs)
ptch2 
ggsave("output/bioconductor_reviewers_numbers.png")

## ----events_days----------------------------------------------------------------------------------
full %>% 
  ggplot() +
  geom_line(aes(time_relative, event_n, col = id, group = id)) +
  facet_wrap(~Approved) +
  labs(title = "Events along time", subtitle = "Approved?",
       x = "Days", y = element_blank(), col = "Issue") + 
  theme_minimal()
ggsave("output/bioconductor_events_time.png")

## ----events_user_distribution---------------------------------------------------------------------
p1 <- ggplot(by_issue) +
  geom_bar(aes(as.factor(diff_events))) +
  labs(y = "Issues", x = element_blank(), 
       title = "Different events in the issue")
p2 <- ggplot(by_issue) +
  geom_bar(aes(as.factor(diff_users))) +
  labs(y = element_blank(), x = element_blank(), 
       title = "Different users involved in the issue")
p1 + p2
ggsave("output/bioconductor_events_and_users.png")

## ----actor_event_types----------------------------------------------------------------------------
by_issue %>% 
  mutate(diff_actors = factor(diff_users, levels = 1:13)) %>% 
  ggplot() +
  geom_count(aes(diff_actors, as.factor(diff_events))) +
  labs(x = "Users", y = "Events", title = "Users involved and different events",
       size = "Issues") + 
  scale_radius() +
  scale_x_discrete(drop = FALSE) 
ggsave("output/bioconductor_events_users.png")

## ----actors_events--------------------------------------------------------------------------------
by_issue %>% 
  mutate(diff_actors = factor(diff_users, levels = 1:13)) %>% 
  group_by(diff_actors) %>% 
  count(events, Approved) %>% 
  ggplot() +
  geom_jitter(aes(as.factor(diff_actors), events, size = n,
                  col = Approved, shape = Approved), 
             height = 0) +
  labs(x = "Users", y = "Events", size = "Issues",
       title = "Users involved on the issues and events") +
  scale_x_discrete(drop = FALSE) + 
  scale_size(breaks = c(seq(0, 300, by = 50)))
ggsave("output/bioconductor_events_users_cloud.png")

## ----who------------------------------------------------------------------------------------------
top_events <- 35
by_user %>%
  arrange(-actions) %>% 
  top_n(top_events, actions) %>% 
  select(actor, event) %>% 
  unnest(event) %>% 
  pivot_longer(names_to = "event", cols = created:unlabeled, values_to = "n") %>% 
  filter(n != 0) %>% 
  ggplot() + 
  geom_tile(aes(fct_reorder(event, -n, .fun = sum), 
                fct_reorder(actor, n, .fun = sum), 
                fill = n)) +
  scale_fill_viridis_c(trans = "log10", expand = expansion()) +
  labs(title = "Events by users", y = element_blank(), x = element_blank(),
       fill = "Events") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("output/bioconductor_users_events.png")

## ----comments_plot--------------------------------------------------------------------------------
comments <- full %>%
  group_by(id) %>% 
  mutate(n = sum(event %in% "assigned"),
         assigners = list(reviewer[event == "assigned"]),
         unassigners = list(reviewer[event == "unassigned"]),
         reasigned  = any(event %in% "unassigned")) %>% 
  group_by(id) %>% 
  mutate(reviewers = list(reviewers(unlist(assigners, FALSE, FALSE), 
                                    unlist(unassigners, FALSE, FALSE))),
         reviewers_n = lengths(assigners)-lengths(unassigners),
         creator = unique(actor[event == "created"])) %>% 
  ungroup() %>% 
  filter(actor != "bioc-issue-bot", event == "commented", reviewers_n >= 1) %>% 
  group_by(id) %>% 
  summarise(speaking = n_distinct(actor), 
            comments = n(), 
            reviewer = sum(actor %in% unlist(reviewers[1])),
            author = sum(actor == creator),
            mtmorgan = sum(actor == "mtmorgan"),
            other = comments - reviewer - author)
comments %>% 
  ggplot() +
  geom_count(aes(author, reviewer)) +
  labs(title = "Comments", x = "Authors", y = "Reviewer(s)", size = "Issues") +
  theme_minimal()
ggsave("output/bioconductor_comments_authors_reviewers.png")

## ----comment_plot2--------------------------------------------------------------------------------
comments %>% 
  filter(mtmorgan != reviewer) %>% 
  mutate(perc = mtmorgan/other) %>% 
  mutate(perc = if_else(is.na(perc), 0, perc)) %>% 
  ggplot() +
  geom_count(aes(other, mtmorgan, col = perc)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Other comments compared to mtmorgan", x = "Other", 
       size = "Issues", col = "Ratio") +
  scale_y_continuous(breaks = seq(0, 13, by = 2)) +
  scale_radius() +
  theme_minimal()
ggsave("output/bioconductor_comments_mtmorgan_others.png")

## ----bioc_bot_plot--------------------------------------------------------------------------------
comments <- full %>% 
  ungroup() %>% 
  filter(event == "commented")
bioc_bot <- comments %>% 
  filter(actor == "bioc-issue-bot") %>% 
  mutate(reason = case_when(
    startsWith(text, "Hi @") ~ "Received",
    startsWith(text, "Received a valid push") ~ "Valid push",
    str_detect(text, "^(\n)?Dear Package contributor,") ~ "Build result",
    startsWith(text, "A reviewer has been assigned to your package") ~ "Reviewer assigned",
    str_detect(text, "There is no repository called") ~ "Missing repository",
    str_detect(text, "Thanks for submitting your additional package") ~ "Additional package",
    str_detect(text, "has already posted ") ~ "repost",
    str_detect(text, "for an extended period of time") ~ "Closing",
    str_detect(text, "DESCRIPTION file") ~ "Unmatch",
    str_detect(text, "Your package has been approved for building") ~ "Building",
    str_detect(text, "We only start builds when the `Version`") ~ "Update version",
    str_detect(text, "fix your version number") ~ "Fix version",
    str_detect(text, "a GitHub repository URL") ~ "Missing repository",
    str_detect(text, "more than one GitHub URL") ~ "Multiple repositories",
    str_detect(text, "Add SSH keys") ~ "SSH key",
    startsWith(text, "Your package has been accepted.") ~ "Accepted",
    TRUE ~ "Other"
  ))
bioc_bot %>% 
  group_by(id) %>% 
  count(reason, sort = TRUE) %>% 
  ungroup() %>% 
  ggplot() +
  geom_tile(aes(id, fct_reorder(reason, n, .fun = sum), col = n)) +
  scale_color_viridis_c(trans = "log10", expand = expansion()) +
  labs(x = "Issue", title = "bioc-issue-bot activity", 
       y = element_blank(), col = "Comments")
ggsave("output/bioconductor_bioc_bot_activity.png")

## ----user_events----------------------------------------------------------------------------------
bm3 <- bioc_bot %>% 
  filter(Approved != "Ongoing") %>% 
  group_by(id, reason) %>% 
  summarize(n = n(), Approved = unique(Approved)) %>% 
  mutate(total = sum(n)) %>% 
  group_by(id) %>% 
  mutate(perc = n/total)

bm3 %>% 
  filter(reason != "Accepted") %>% 
  group_by(reason) %>% 
  ggplot() +
  geom_jitter(aes(Approved, n, col = Approved, shape = Approved), height = 0) + 
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~reason, ncol = 3) +
  labs(x = element_blank(), y = "Comments", 
       title = "Diferences between accepted and non accepted issues by bioc-issue-bot comments")
ggsave("output/bioconductor_differences_bot_comments_approved.png")

## ----significant, eval=FALSE, include=FALSE-------------------------------------------------------
## bm3 %>%
##   group_by(reason) %>%
##   filter(n_distinct(Approved) >= 2 & sum(Approved == "Yes") >= 2) %>%
##   nest_by() %>%
##   mutate(t = list(t.test(n ~ Approved, data = data))) %>%
##   mutate(broom::glance(t)) %>%
##   ungroup() %>%
##   mutate(adj.p.value = p.adjust(p.value)) %>%
##   filter(adj.p.value < 0.05) %>%
##   pull(reason)


## ----common_feedback------------------------------------------------------------------------------
build_related <- c("Build result", "Building", "Valid push", "Received", 
                   "Update version")

bm_com <- bioc_bot %>% 
  group_by(id) %>% 
  summarise(builds = sum(reason %in% build_related), 
            total = n(),
            diff = total - builds, 
            Approved = unique(Approved)) 
bm_com %>%
  filter(Approved != "Ongoing") %>% 
  ggplot() +
  geom_count(aes(total, builds, col = Approved, shape = Approved)) +
  facet_wrap(~ Approved) +
  labs(x = "All comments", title = "bioc-issue-bot comments",
       y = "Build related", size = "Issues")
ggsave("output/bioconductor_bioc_bot_build_comments.png")

## ----bot_comments_approve-------------------------------------------------------------------------
# t.test(diff ~ Approved, data = ungroup(bm_com))
bm_com %>% 
  count(diff, Approved) %>% 
  ggplot() +
  geom_tile(aes(Approved, as.factor(diff), fill = n)) +
  # geom_jitter(aes(Approved, diff, col = Approved), height = 0) +
  # geom_violin(aes(Approved, diff, col = Approved), alpha = 0) +
  labs(x = element_blank(), 
       title = "bioc-issue-bot comments not related to builds", 
       y = "Comments not related to builds", fill = "Issues")
ggsave("output/bioconductor_bioc_bot_comments_no_build.png")

## ----comments_issues------------------------------------------------------------------------------
com_is <- full %>% 
  filter(Approved != "Ongoing") %>% 
  group_by(id) %>% 
  mutate(submitter = unique(actor[event == "created"])) %>% 
  mutate(assigners = list(reviewer[event %in% "assigned"]),
         unassigners = list(reviewer[event %in% "unassigned"]),
         reviewers = list(setdiff(unlist(assigners, FALSE, FALSE), 
                                  unlist(unassigners, FALSE, FALSE))),
         commenter = case_when(
           actor %in% unlist(reviewers) ~ "Reviewer",
           actor %in% submitter ~ "Author",
           TRUE ~ "Other")
  ) %>% 
  filter(actor != "bioc-issue-bot" & event == "commented") %>% 
  ungroup()
com_is_w <- com_is %>% 
  group_by(id) %>% 
  count(commenter) %>% 
  pivot_wider(names_from = commenter, values_from = n, values_fill = 0) %>% 
  ungroup()
  
ra <- com_is_w %>% 
  group_by(Author, Reviewer) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(Author, Reviewer, col = n)) +
  scale_color_continuous(limits = c(1, 50)) +
  guides(col = FALSE)

ro <- com_is_w %>% 
  group_by(Other, Reviewer) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  geom_abline(slope = -1) +
  geom_point(aes(Reviewer, Other, col = n)) +
  scale_y_continuous(position = "right") +
  scale_x_continuous(trans = "reverse") +
  labs(y = element_blank()) +
  scale_color_continuous(limits = c(1, 50)) +
  guides(col = FALSE)
oa <- com_is_w %>% 
  group_by(Author, Other) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(Author, Other, col = n)) +
  scale_color_continuous(limits = c(1, 50)) +
  labs(col = "Issues")
  # guides(col = FALSE)
ra /(ro + oa ) +
  plot_annotation(title = "Comments on submissions by users",
                  tag_levels = "i") &
  plot_layout(guides = "collect") 
ggsave("output/bioconductor_comments_user_reviewer_author.png")

## ----comments_approved----------------------------------------------------------------------------
com_is_w <- com_is %>% 
  filter(Approved != "Ongoing") %>% 
  group_by(id, Approved) %>% 
  count(commenter) %>% 
  pivot_wider(names_from = commenter, values_from = n, values_fill = 0) %>% 
  ungroup()
  
a <- com_is_w %>% 
  group_by(Author, Reviewer, Approved) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(Author, Reviewer, col = n)) +
  scale_color_continuous(limits = c(1, 50)) +
  labs(col = "Issues") +
  facet_wrap(~Approved)
b <- com_is_w %>% 
  group_by(Other, Reviewer, Approved) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(Reviewer, Other, col = n)) +
  scale_color_continuous(limits = c(1, 50)) +
  facet_wrap(~Approved) +
  labs(col = "Issues")
d <- com_is_w %>% 
  group_by(Author, Other, Approved) %>% 
  count(sort = TRUE) %>% 
  ggplot() +
  geom_abline(slope = 1) +
  geom_point(aes(Author, Other, col = n)) +
  scale_color_continuous(limits = c(1, 50)) +
  facet_wrap(~Approved) +
  labs(col = "Issues")
a + b + d + plot_layout(ncol = 1,
                        guides = "collect")  +
  plot_annotation(title = "Comments on submissions by users",
                  subtitle = "Separated by if approved or not")
ggsave("output/bioconductor_comments_submissions_reviewer_other_authors.png")

## ----delays---------------------------------------------------------------------------------------
closed_c <- full %>% 
  group_by(id) %>% 
  mutate(comment_n = cumsum(event == "commented" & actor != "bioc-issue-bot")) %>% 
  filter(event == "closed") %>% 
  filter(event_n == max(event_n)) %>% 
  select(id, time_relative, comment_n, event_n, Approved) %>% 
  ungroup() %>% 
  mutate(Closed = TRUE)

full %>% 
  filter(event %in% c("commented", "created") & actor != "bioc-issue-bot") %>% 
  group_by(id) %>% 
  mutate(comment_n = cumsum(event == "commented" & actor != "bioc-issue-bot")) %>% 
    ungroup() %>% 
  ggplot() +
  geom_point(aes(time_relative, comment_n, col = id),  data = closed_c, shape = "square") +
  geom_line(aes(time_relative, comment_n, group = id, col = id)) +
  facet_wrap(~Approved, scales = "free") +
  guides(col = FALSE) + 
  labs(x = "Since creation of the issue (days)", y = "Comments", title = "Comments on issues")
ggsave("output/bioconductor_comments_issues.png")

## ----closing--------------------------------------------------------------------------------------
aftr <- full %>% 
  group_by(id) %>% 
  filter(event %in% c("commented", "created") & actor != "bioc-issue-bot") %>% 
  mutate(comment_n = cumsum(event == "commented" & actor != "bioc-issue-bot"),
         Closed = FALSE) %>% 
  select(id, time_relative, comment_n, event_n, Approved, Closed) %>% 
  rbind(closed_c) %>% 
  group_by(id) %>% 
  mutate(Closed = event_n > ifelse(any(Closed), event_n[Closed], Inf))
aftr %>% 
  group_by(id, Approved) %>% 
  count(Closed) %>% 
  filter(Closed) %>% 
  ggplot() + 
  geom_jitter(aes(Approved, n, col = Approved, shape = Approved), height = 0) +
  labs(x = element_blank(), y = "Comments", 
       title = "Comments after being closed",
       size = "Issues")
ggsave("output/bioconductor_comments_after_closing.png")

## ----successful_build-----------------------------------------------------------------------------
full %>% 
  group_by(id) %>% 
  summarise(success_build = any(event == "labeled" & label == "OK"),
            Approved = unique(Approved)) %>% 
  ungroup() %>% 
  count(success_build, Approved, sort = TRUE) %>% 
  mutate(success_build = ifelse(success_build, "Yes", "No"))%>% 
  arrange(Approved, -n) %>% 
  knitr::kable(col.names = c("Successful build?", "Approved?", "Submissions"))


## ----trans_builds---------------------------------------------------------------------------------
logic_nth <- function(x, n){
  y <- rep(FALSE, length(!!x))
  z <- which(!!x)
  y[z[seq_len(n)]] <- TRUE
  y
}
builds <- full %>% 
  group_by(id) %>% 
  mutate(assigners = list(reviewer[event %in% "assigned"]),
            unassigners = list(reviewer[event %in% "unassigned"]),
            reviewers = list(reviewers(unlist(assigners, FALSE, FALSE), 
                                    unlist(unassigners, FALSE, FALSE))),
         comment = event == "commented"
         ) %>% 
  filter((event == "labeled" & label == "OK") |
           (comment & actor %in% unlist(reviewers)) |
           (event == "labeled" & label == "3a. accepted")) %>% 
  arrange(id, event_n) %>% 
  mutate(keel = logic_nth(label == "OK", 1),
         keel2 = logic_nth(label == "3a. accepted", 1),
         keec = event == "commented",
         k = any(keel),
         k2 = ifelse(any(keel), event_n >= event_n[keel], FALSE),
         k3 = keel | (keec  & k2),
         k4 = logic_nth(k3, 2)) %>% 
  filter(k4 | keel2) %>% 
  mutate(event = case_when(label == "OK" ~ "Built correctly",
                           event == "commented" ~ "Reviewer comment",
                           label == "3a. accepted" ~ "Accepted")) %>% 
  ungroup()
builds %>% 
  filter(Approved != "Ongoing") %>% 
  ggplot() +
  geom_point(aes(time_relative, id, col = event, shape = event)) +
  facet_wrap(~Approved, ncol = 2, scales = "free_x") +
  labs(x = "Days since submission",
       y = "Issue",
       title = "First successful build and comment of the reviewer after",
       subtitle = "Submission approved?",
       col = "Event", shape = "Event") +
  theme_minimal()
ggsave("output/bioconductor_build_time.png")

## ----time_comment---------------------------------------------------------------------------------
bc <- builds %>% 
  select(id, time_relative, event, Approved) %>% 
  pivot_wider(names_from = event, 
              values_from = time_relative, 
              values_fill = NA) %>% 
  mutate(time_review = `Reviewer comment`-`Built correctly`,
         time_acceptance = Accepted - `Reviewer comment`)
bc %>% 
  filter(Approved != "Ongoing") %>% 
  filter(!is.na(`Built correctly`) & !is.na(`Reviewer comment`)) %>% 
  ggplot() +
  geom_linerange(aes(y = id, xmin = `Built correctly`, xmax = `Reviewer comment`)) +
  facet_wrap(~Approved, ncol = 2, scales = "free") +
  labs(title = "Time between successful build and comment from the reviewer",
       x = "Days since submission", y = "Issue", 
       subtitle = "Submission approved?") +
  theme_minimal()
ggsave("output/bioconductor_comment_reviewer_issues.png")

## ----time_accepted--------------------------------------------------------------------------------
bc %>% 
  filter(!is.na(Accepted) & !is.na(`Reviewer comment`) & Approved == "Yes") %>% 
  ggplot() +
  geom_linerange(aes(y = id, xmax = Accepted, xmin = `Reviewer comment`)) +
  labs(title = "Time since from the reviewer's comment after successful build and acceptance",
       x = "Days since submission", y = "Issue") +
  theme_minimal()
ggsave("output/bioconductor_succesfult_build_acceptance_time.png")

## ----time_build_plot------------------------------------------------------------------------------
p1 <- bc %>% 
  filter(Approved != "Ongoing") %>% 
  ggplot() +
  geom_histogram(aes(`Built correctly`), binwidth = 1) +
  facet_wrap(~Approved, scales = "free_x") +
  labs(subtitle = "Submission approved?", 
       x = "Days", y = "Issues") +
  theme_minimal()
p1 / p1 +
  coord_cartesian(xlim = c(0, 7)) +
  labs(title = "Zoom to the first week") +
  plot_annotation(title = "Days between submission and the first successful build")
ggsave("output/bioconductor_submission_time_to_build.png")

## ----time_review_plot_zoom------------------------------------------------------------------------
bc %>% 
  filter(Approved != "Ongoing") %>% 
  ggplot() +
  geom_histogram(aes(time_review)) +
  facet_wrap(~Approved, scales = "free") +
  labs(subtitle = "Submission approved?", 
       title = "Days between successful build and comment from reviwers",
       x = "Days", y = "Issues") +
  theme_minimal()
ggsave("output/bioconductor_build_time_to_comment.png")

## ----acceptance_plot------------------------------------------------------------------------------
p2 <- bc %>% 
  ggplot() +
  geom_histogram(aes(Accepted), binwidth = 1) +
  labs(title = "Days till acceptance",
       x = "Days", y = "Issues") +
  theme_minimal()
p2 / p2 + coord_cartesian(xlim = c(0, 20)) + 
  labs(title = element_blank(), subtitle = "A zoom")
ggsave("output/bioconductor_time_to_acceptance.png")

## ----time_acceptance_plot-------------------------------------------------------------------------
bc %>% 
  ggplot() +
  geom_histogram(aes(time_acceptance), binwidth = 1) +
  labs(title = "Days between comment from reviwers and acceptance",
       x = "Days", y = "Issues") +
  theme_minimal()
ggsave("output/bioconductor_comment_time_to_acceptance.png")

## ----times_phases---------------------------------------------------------------------------------
p3 <- bc %>% 
  filter(!is.na(`Built correctly`) & !is.na(time_review)) %>% 
  ggplot() + 
  geom_point(aes(`Built correctly`, time_review, 
                 col = Approved, shape = Approved)) +
  labs(y = "Time between built and review (days)", x = "Built (days)") +
  theme_minimal()
p4 <- bc %>% 
  filter(!is.na(time_review), !is.na(time_acceptance)) %>% 
  ggplot() + 
  geom_point(aes(time_review, time_acceptance), 
             col = "#00BFC4", shape = "triangle") +
  labs(x = "Time between build and review (days)",
       y = "Time between review and acceptance (days)") +
  theme_minimal()
p3 + p4
ggsave("output/bioconductor_build_time_comments_review.png")

## ----table_steps----------------------------------------------------------------------------------
bc %>% 
  group_by(Approved) %>% 
  summarize(across(`Built correctly`:time_acceptance, 
                   function(x){round(median(x, na.rm = TRUE))})) %>% 
  knitr::kable(col.names = c("Approved", 
                             "Succcessful built (days)",
                             "First reviewer comment after build (days)",
                             "Accepted (days)",
                             "Time between build and reviewer comment (days)",
                             "Time between review and acceptance (days)"))


## ----labels_general-------------------------------------------------------------------------------
labels <- full %>% 
  filter(event == "labeled") %>% 
  mutate(label = unlist(label),
         label = case_when(
           label == "1a. awaiting moderation" ~ "1. awaiting moderation", 
           label == "4a. accepted" ~ "3a. accepted",
           label == "ok_to_build" ~ "1. awaiting moderation",
           label == "awaiting moderation" ~ "1. awaiting moderation",
           label == "review-in-progress" ~ "2. review in progress",
           label == "TESTING" ~ NA_character_, 
           TRUE ~ label)) %>% 
  filter(!is.na(label))


## ----labels_plot_overview-------------------------------------------------------------------------
ord_label <- c("1. awaiting moderation",  
               "2. review in progress", "3a. accepted", 
               "3b. declined", "3c. inactive", 
               "3d. closed pending pre-review changes",
               "ABNORMAL", 
               "VERSION BUMP REQUIRED", 
               "TIMEOUT", "ERROR", 
               "WARNINGS", "OK")
labels %>% 
  group_by(id) %>% 
  count(label, sort = TRUE) %>% 
  ggplot() +
  geom_tile(aes(id, fct_relevel(label, rev(ord_label)), fill = n)) +
  scale_fill_continuous(trans = "log10") +
  labs(x = "Issue", y = element_blank(), title = "Labels per Issue",
       fill = "Times")
ggsave("output/bioconductor_labels_issues.png")

## ----label_differences----------------------------------------------------------------------------
labels %>% 
  group_by(id) %>% 
  count(label, Approved, sort = TRUE) %>% 
  ungroup() %>% 
  group_by(label) %>% 
  filter(n_distinct(Approved) >= 2) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_jitter(aes(Approved, n, col = Approved, shape = Approved), height = 0) +
  facet_wrap(~label, scales = "free", ncol = 2, drop = FALSE) +
  guides(col = FALSE, shape = FALSE) +
  labs(x = element_blank(), y = "Times", 
       title = "Times each label has appeared on each issue")
ggsave("output/bioconductor_labels_type.png")

## ----significant_label, eval=FALSE, include=FALSE-------------------------------------------------
labels %>%
  group_by(id) %>%
  count(label, Approved, sort = TRUE) %>%
  ungroup() %>%
  filter(Approved != "Ongoing") %>%
  group_by(label) %>%
  filter(n_distinct(Approved) == 2, n_distinct(n) > 2) %>%
  nest_by() %>%
  mutate(t = list(t.test(n ~ Approved, data = data))) %>%
  mutate(broom::glance(t)) %>%
  ungroup() %>%
  mutate(adj.p.value = p.adjust(p.value)) %>%
  filter(adj.p.value < 0.05) %>%
  pull(label)


## ----labels_table---------------------------------------------------------------------------------
labels %>% 
  group_by(id) %>% 
  count(label, Approved, sort = TRUE) %>% 
  ungroup() %>% 
  filter(Approved != "Ongoing") %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% 
  select(-id) %>% 
  group_by(Approved) %>% 
  summarize(across(where(is.numeric), median)) %>% 
  knitr::kable()


## ----reproducibility, echo = FALSE----------------------------------------------------------------
## Reproducibility info
options(width = 120)
sessioninfo::session_info()

