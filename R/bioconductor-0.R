## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, message = FALSE, error = FALSE,
                      cache = FALSE, echo=FALSE, out.width="120%")


## ----gh-------------------------------------------------------------------------------------------
library("gh")
PR <- gh("GET /search/issues?q=repo:Bioconductor/Contributions+is:issue") # Copied from https://developer.github.com/v3/pulls/


## ----get_issues, cache = TRUE---------------------------------------------------------------------
page <- function(query, x){paste0(query, "&page=", x)}
get_issues <- function() {
  query <- "GET /repos/Bioconductor/Contributions/issues?state=all&per_page=100"
  qp <- page(query, 1:15)
  v <- vector("list", length = 17)
  for (i in seq_along(qp)) {
    v[[i]] <- gh(qp[i])
  }
  unlist(v, use.names = FALSE, recursive = FALSE)
}

issues <- get_issues()


## ----clean----------------------------------------------------------------------------------------
clean_issue <- function(x) {
  # Omit pull request I want Package submissions
  if ("pull_request" %in% names(x)) {
    return(NA)
  }
  repos <- str_extract_all(x$body, "https?://github.com/.+/.+")
  repos <- repos[[1]]
  repos <- str_remove(repos, "https?://github.com/")
  pkg_repo <- unlist(str_extract_all(repos, "^[:alnum:]+"), FALSE, FALSE)
  p <- str_remove(unlist(str_extract_all(repos, "/.*$"), FALSE, FALSE), "^/")
  p <- gsub("\\.$", "", p)
  
  list(id = as.numeric(x$number), user = x$user$login, title = x$title, 
       n_comments = x$comments, 
       created = x$created_at, closed = x$closed_at, 
       repos = repos,
       pkg_repo = pkg_repo, 
       package = p,
       assignee = vapply(x$assignees, getElement, name = "login", character(1L)),
       labels = vapply(x$labels, getElement, name = "name", character(1L)))
 }


## ----tidy_issues----------------------------------------------------------------------------------
library("tidyverse")
library("lubridate")
l <- lapply(issues, clean_issue)
l <- l[lengths(l) == 11] # Expect to delete at least 4 issues
m <- t(simplify2array(l))
m <- as.data.frame(m)
m$created <- unlist(m$created, recursive = FALSE, use.names = FALSE)
# If it is not closed leave it empty
replace <- lengths(m$closed) != 0
unlisted_closed <- unlist(m$closed, recursive = FALSE, use.names = FALSE)
m$closed <- NA
m$closed[replace] <- unlisted_closed

df <- m %>% 
  mutate(id = as.numeric(id),
         title = str_squish(as.character(title)),
         user = as.character(user),
         n_comments = as.numeric(n_comments),
         created = as.Date(created),
         closed = as.Date(closed),
         approved = vapply(labels, function(x){"3a. accepted" %in% x}, logical(1L)),
         n_reviewers = lengths(assignee),
         n_packages = lengths(repos),
         n_labels = lengths(labels),
         package = ifelse(package == "NA", NA, package),
         pkg_repo = ifelse(pkg_repo == "NULL", NA, pkg_repo),
         Approved = case_when(approved ~ "Yes",
                              !approved & !is.na(closed) ~ "No",
                              TRUE ~ "Ongoing"),
         time_opened = if_else(is.na(closed), as.Date(today()), closed)-created,
  ) %>% 
  arrange(id) %>% 
  mutate(i = 1:n())
n_dup <- count(df, package, sort = TRUE) %>% 
  filter(package != "", n != 1) %>% nrow()


## ----check_available------------------------------------------------------------------------------
library("BiocManager")
Bioconductor <- BiocManager::repositories()
bp <- available.packages(contriburl = contrib.url(Bioconductor)[1:4])
lasted <- df %>% 
  group_by(package) %>% 
  filter(!any(approved)) %>% 
  filter(closed == max(closed)) %>% 
  ungroup() %>% 
  select(package, approved)
currently_on_bioc <- lasted$package  %in% rownames(bp)
check <- (currently_on_bioc != lasted$approved) & (currently_on_bioc == TRUE)
approved_packages_wo_label <- df %>% 
  filter(package  %in% lasted$package[check]) %>% 
  group_by(package) %>% 
  filter(!any(approved)) %>% 
  arrange(package, -n_comments) %>% 
  count(sort = TRUE) %>% 
  filter(n == 1) %>% 
  pull(package)
df$approved[df$package %in% approved_packages_wo_label] <- TRUE


## ----eda, warning=FALSE---------------------------------------------------------------------------
# If not closed add the closing time of today
releases <- data.frame(release = paste0("3.", 3:12),
           date = as.Date(c("2016/04/04", "2016/10/18", "2017/04/25", 
                            "2017/10/31", "2018/05/01", "2018/10/31", 
                            "2019/05/03", "2019/10/30", "2020/04/28",
                            "2020/10/01"), format = "%Y/%m/%d"),
           stringsAsFactors = FALSE)

scale_data <- scale_x_date(expand = expansion(add = 10), 
               limits = as.Date(c("2016-06-01", "2020-06-10"), "%Y-%m-%d"))
theme_set(theme_minimal())
df %>% 
  ggplot() +
  geom_linerange(aes(y = id, xmin = created, xmax = closed), col = "grey") + 
  geom_point(aes(y = id, created), col = "black", size = 1) + 
  geom_point(aes(y = id, closed, col = Approved, shape = Approved), size = 1) + 
  geom_vline(xintercept = releases$date, col = "#1a81c2") + # Releases dates
  geom_text(data = releases, aes(x = date, y = c(rep(1200, 5), rep(300, 5)),
           label = release)) + # Release dates

  labs(title = "Issues that are open at least a day", 
       subtitle = "Vertical blue lines indicate Bioconductor releases.",
       x = element_blank(), y = element_blank(), 
       caption = "Author: @Lluis_Revilla") +
  scale_data +
  scale_y_continuous(expand = expansion(add = 10))


## ----users_submitting-----------------------------------------------------------------------------
usr_diff_pkg <- df %>% 
  filter(!is.na(closed) & n_packages == 1) %>% 
  unnest(package) %>% 
  group_by(user) %>% 
  distinct(package) %>% 
  count(sort = TRUE) %>% 
  ungroup()

usr_diff_pkg %>% 
  count(n) %>% 
  ggplot() +
  geom_col(aes(n, nn), col = "#87b13f", fill = "#1a81c2") +
  labs(y = "Contributors", x = "Packages", 
       title = "Number of packages submitted by a contributor") +
  scale_y_continuous(expand = expansion(add = c(0, 10))) +
  scale_x_continuous(breaks = rev(unique(usr_diff_pkg$n)), 
                     expand = expansion(add = 0.05))


## ----users_ratio----------------------------------------------------------------------------------
usr_ratio <- df %>% 
  filter(!is.na(closed) & n_packages == 1) %>% 
  group_by(user) %>% 
  summarise(ratio = sum(approved)/n()) %>% 
  arrange(ratio) %>% 
  ungroup()

col_bioc <- scale_color_gradient(low = "#87b13f", high = "#1a81c2")
fill_bioc <- scale_color_gradient(low = "#87b13f", high = "#1a81c2")
usr_ratio %>% 
  ggplot() +
  geom_bar(aes(ratio, fill = ratio)) +
  fill_bioc +
  labs(title = "Authors success submitting packages",
       x = "Success ratio", y = "Users")


## ----users----------------------------------------------------------------------------------------
usr_success <- usr_diff_pkg %>% 
  inner_join(usr_ratio)

ggplot(usr_success) +
  geom_count(aes(n, ratio, col = ratio)) +
  col_bioc +
  labs(x = "Packages", y = "Approval success ratio", size = "Users",
       title = "Submitting more packages increaes approval rate",
       col = "Success ratio") +
  scale_x_continuous(expand = expansion(add = 0.25), breaks = 1:14) +
  theme(panel.grid.minor.x = element_blank())


## ----users_progression----------------------------------------------------------------------------
# Date progression for users that submit more than once (the previous plot is the final snapshot)
usr_diff_pkg2 <- rename(usr_diff_pkg, diff_pkg = n)

usr_submission_pkg <- df %>% 
  filter(!is.na(closed), n_packages == 1) %>% 
  unnest(package) %>% 
  filter(package != "yourpackagename") %>% 
  group_by(user) %>% 
  arrange(id) %>% 
  mutate(n = n(),
         user_submission = 1:n()) %>% 
  filter(n != 1) %>% 
  ungroup() %>% 
  select(user, user_submission, created, package, Approved)

usr_submission_pkg %>% 
  group_by(user_submission, Approved) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(user_submission) %>% 
  mutate(ratio = n/sum(n)) %>% 
  ungroup() %>% 
  filter(Approved == "Yes") %>% 
  select(user_submission, ratio) %>% 
  ggplot() + 
  geom_path(aes(user_submission, ratio, col = ratio)) +
  col_bioc +
  scale_x_continuous(breaks = 1:15) +
  scale_y_continuous(limits = c(0, 1))  +
  labs(title = "User submissions", y = "Approval ratio", 
       x = "Number of submission") +
  guides(col = FALSE) + 
  theme(panel.grid.minor.x = element_blank())



## ----user_submission_progression------------------------------------------------------------------
usr_submission_pkg %>% 
  group_by(user) %>% 
  mutate(max_submissions = max(user_submission)) %>% 
  group_by(user_submission, max_submissions) %>% 
  count(Approved) %>% 
  mutate(ratio = n/sum(n),
         ratio = if_else(Approved == "No", 1-ratio, ratio)) %>% 
  filter(Approved == "Yes" | ratio == 0) %>% 
  ggplot() +
  geom_point(aes(max_submissions, user_submission, size = ratio, col = ratio)) +
  scale_colour_binned(low = "#87b13f", high = "#1a81c2", 
                      guide = guide_bins(show.limits = TRUE)) +
  scale_size_binned(guide = guide_bins(show.limits = TRUE)) +
  scale_x_continuous(breaks = 1:15) +
  scale_y_continuous(breaks = 1:15) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title = "User submissions", y = "Submission", 
       x = "Max number of submissions",
       size = "Approval ratio", col = "Approval ratio")


## ----submissions----------------------------------------------------------------------------------
df %>% 
  group_by(year = year(created)) %>% 
  summarise(n = n(), ratio = sum(approved)/n) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(year, n, fill = ratio)) + 
  labs(title = "Yearly submissions and approval", 
       y = "Issues", x = element_blank(), 
       fill = "Approval ratio") +
  scale_fill_continuous(low = "#87b13f", high = "#1a81c2", limits = c(0, 1))


## ----reviewers, include=FALSE---------------------------------------------------------------------
df %>% 
  group_by(n_reviewers) %>% 
  count(Approved) %>% 
  ungroup() %>% 
  mutate(ratio = n/sum(n)) %>% 
  arrange(-ratio, n)

df %>% 
  filter(n_reviewers == 0) %>% 
  group_by(time_opened) %>% 
  count(Approved) %>% 
  ungroup() %>% 
  mutate(ratio = n/sum(n)) %>% 
  arrange(-ratio)

df %>% 
  group_by(n_reviewers) %>% 
  count(Approved) %>% 
  ungroup() %>% 
  filter(n_reviewers != 0) %>% 
  mutate(ratio = n/sum(n)) %>% 
  group_by(Approved) %>% 
  summarise(fr = sum(ratio))


## ----reviewers_frequency--------------------------------------------------------------------------
normal_reviews <- df %>% 
  filter(n_reviewers == 1, n_labels >= 1, n_comments > 1,
         !is.na(closed)) %>% 
  mutate(Reviewer = unlist(assignee))

top_reviewers <- normal_reviews %>% 
  count(Reviewer, sort = TRUE) %>% 
  top_n(9, wt = n) %>% 
  pull(Reviewer)

normal_reviews %>% 
  mutate(year = year(created)) %>% 
  group_by(year) %>% 
  count(Reviewer) %>% 
  mutate(share = n/sum(n),
         Reviewer = fct_reorder2(Reviewer, year, n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(year, share, col = Reviewer)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = element_blank(), y = element_blank(),
       title = "Share of issues reviewed")


## ----reviewer_comments----------------------------------------------------------------------------
normal_reviews %>% 
  group_by(Reviewer) %>% 
  count(Approved) %>% 
  mutate(ratio = n/sum(n), total = sum(n),
         Reviewer = fct_reorder(Reviewer, ratio)) %>% 
  ungroup() %>% 
  filter(Approved == "Yes", total  > 50) %>% 
  ggplot() +
  geom_point(aes(total, ratio, col = Reviewer)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), 
                     expand = expansion(mult = 0, add = 0)) +
  labs(x = "Issues handled", y = "Approval ratio",
       title = "Reviewers approval Ratio")


## ----reviewers_time-------------------------------------------------------------------------------
breaks <- function(limits) {
  seq(from = 0, to = floor(limits[2]), by = 100)
}

normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(Reviewer, time_opened, 
                                   col = Approved, shape = Approved), 
                               size = 0.75) +

  labs(y = "Days open", title = "Time open by reviewers")

# Focusing a bit

normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(Reviewer, time_opened, 
                                   col = Approved, shape = Approved)) +
  coord_cartesian(ylim = c(0, 150)) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  labs(y = "Days open", title = "Time open by reviewers", 
       subtitle = "A zoom")


## ----reviewer_time_diff---------------------------------------------------------------------------
reviewer_time <- normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  group_by(Reviewer) %>% 
  summarise(m = median(time_opened), me = mean(time_opened), n = n()) %>% 
  ungroup() %>% 
  arrange(m)
           
global_medians_time <- normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  group_by(Approved) %>% 
  summarise(m = median(time_opened), me = mean(time_opened), n = n())
  
reviewers_time_approved <- normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  group_by(Reviewer, Approved) %>% 
  summarise(m = median(time_opened), 
            me = mean(time_opened), 
            s = sd(time_opened),
            n = n(),
            sem = sqrt(pi/2)*s/sqrt(n)) %>% # https://stats.stackexchange.com/q/59838/105234
  ungroup() %>% 
  mutate(Reviewer = fct_relevel(Reviewer, reviewer_time$Reviewer))

reviewers_time_approved %>% 
  ggplot() +
  geom_hline(data = global_medians_time, 
             aes(yintercept = m, col = Approved), linetype = "dotted") +
  geom_point(data = reviewer_time, aes(fct_relevel(Reviewer, Reviewer), 
                                       m, size = n)) +
  geom_point(aes(Reviewer, m, col = Approved, shape = Approved, size = n)) +
  geom_errorbar(aes(x = Reviewer, ymin = m-sem, ymax = m+sem, col = Approved), width = 0.2) +
  labs(x = element_blank(), y = "Days (median)", 
       title = "Reviewers speed to close", 
       subtitle = "In black all reviews together. Errorbars are the standard error of the median",
       size = "Reviews") +
  scale_y_continuous(limits = c(0, 130), expand = expansion(add = c(1, 0)), 
                     breaks = seq(from = 0, to = 130, by = 20)) +
  scale_shape_manual(values = c(15, 17)) +
  scale_x_discrete(expand = expansion(add = 0.1))


## ----reviewers_comments---------------------------------------------------------------------------
normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(Reviewer, n_comments, 
                                   col = Approved, shape = Approved), 
                               size = 0.75) +

  labs(y = "Comments", title = "Comments on the issue", x = element_blank())


## ----reviewer_comment_diff------------------------------------------------------------------------
reviewer_comments <- normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  group_by(Reviewer) %>% 
  summarise(m = median(n_comments), me = mean(n_comments), n = n()) %>% 
  ungroup() %>% 
  arrange(m)

global_medians_comments <- normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  group_by(Approved) %>% 
  summarise(m = median(n_comments), me = mean(n_comments), n = n())
reviewers_comment_approved <- normal_reviews %>% 
  filter(Reviewer  %in% top_reviewers) %>% 
  group_by(Reviewer, Approved) %>% 
  summarise(m = median(n_comments), 
            me = mean(n_comments), 
            s = sd(n_comments),
            n = n(),
            sem = sqrt(pi/2)*s/sqrt(n)) %>% 
  ungroup() %>% 
  mutate(Reviewer = fct_relevel(Reviewer, reviewer_comments$Reviewer))
reviewers_comment_approved %>% 
  ggplot() +
  geom_hline(data = global_medians_comments, 
             aes(yintercept = m, col = Approved), linetype = "dotted") +
  geom_point(data = reviewer_comments, aes(fct_relevel(Reviewer, Reviewer), 
                                       m, size = n)) +
  geom_point(aes(Reviewer, m, col = Approved, fill = Approved, shape = Approved, size = n)) +
  geom_errorbar(aes(x = Reviewer, ymin = m-sem, ymax = m+sem, col = Approved), width = 0.2) +
  labs(x = element_blank(), y = "Comments (median)", 
       title = "Comments on the issues", 
       subtitle = "In black all reviews together. Errorbars indicate standard error of the median. ",
         size = "Reviews") +
  scale_y_continuous(limits = c(0, 70), expand = expansion(add = c(1, 0)), 
                     breaks = seq(from = 0, to = 70, by = 20)) +
  scale_shape_manual(values = c(15, 17)) + 
  scale_x_discrete(expand = expansion(add = 0.1))


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
  filter(Reviewer  %in% top_reviewers) %>% 
  ggplot() +
  geom_point(aes(time_opened, n_comments, col = Approved, shape = Approved), 
             size = 0.75) +
  scale_x_continuous(breaks = breaks, guide = guide_axis(check.overlap = TRUE)) +
  scale_y_continuous(breaks = breaks) +
  facet_wrap(~Reviewer, scales = "free") +
  labs(x = "Days opened", y = "Comments", 
       title = "Comments and open days per reviewer")


## ----mix_reviews, eval=FALSE, include=FALSE-------------------------------------------------------
## mix_reviews <- reviewers_time_approved %>%
##   rename(median_time = m, sem_time = sem, sd_time = s) %>%
##   select(-me) %>%
##   inner_join(reviewers_comment_approved %>% rename(median_comments = m, sem_comments = sem, sd_comments = s))
## 
## mix_reviews %>%
##   group_by(Reviewer) %>%
##   summarise(
##     time_diff = max(median_time)-min(median_time),
##     comments_diff = max(median_comments)-min(median_comments)) %>%
##   arrange(time_diff, comments_diff)
## 
## mix_reviews %>%
##   ggplot() +
##   geom_point(aes(median_time, median_comments, col = Reviewer, shape = Approved, size = n)) +
##   geom_path(aes(median_time, median_comments, col = Reviewer)) +
##   # geom_errorbar(aes(y = median_comments,
##   #                   xmin = median_time-sem_time,
##   #                   xmax = median_time+sem_time, col = Reviewer), width = 1) +
##   # geom_errorbar(aes(x = median_time,
##   #                   ymin = median_comments-sem_comments,
##   #                   ymax = median_comments+sem_comments, col = Reviewer), width = 1)
##   labs(y = "Comments (median)", x = "Days (median)",
##        title = "Reviewers differences")  +
##   scale_x_continuous(limits = c(0, 90), expand = expansion(add = c(1, 0)),
##                      breaks = seq(from = 0, to = 130, by = 20)) +
##   scale_y_continuous(limits = c(1, 50))


## ----submission_rate------------------------------------------------------------------------------
df %>% 
  mutate(md  = as.numeric(format(created, "%j")),
         year = year(created)) %>% 
  group_by(md) %>% 
  summarise(n = median(n())) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(md, n), col = "#1a81c2") + 
  labs(title = "Median daily submissions", 
       x = "Day of year", y = "Issues opened") +
  scale_x_continuous(expand = expansion(add = 5))


## ----margin_submission----------------------------------------------------------------------------
release_attempt <- function(x, release = releases) {
  diff_time <- x - release$date
  pre_release <- diff_time < 0
  pick <- which(diff_time[pre_release] == max(diff_time[pre_release]))
  data.frame("release" = release$release[pre_release][pick], 
    "margin" = abs(diff_time[pre_release][pick]), stringsAsFactors = FALSE)
}

ra <- lapply(df$created, release_attempt)
r <- lapply(ra, function(x){x$release})
r[lengths(r) == 0] <- NA
r <- unlist(r, FALSE, FALSE)
m <- lapply(ra, function(x){x$margin})
m[lengths(m) == 0] <- NA
m <- unlist(m, FALSE, FALSE)
m <- as.difftime(m, units = "days")
df2 <- df %>% 
  mutate(release = r, margin = m,
         devel = release == "3.11" & approved & margin < 30)
df2 %>% 
  ggplot() +
  geom_histogram(aes(margin, fill = margin, col = margin), bins = 40) +
  geom_vline(xintercept = 30) +
  fill_bioc +
  col_bioc +
  scale_y_continuous(expand = expansion(add = c(0, 5))) +
  scale_x_continuous(expand = expansion())  + 
  labs(title = "Days till next release", y = "Issues", x = "Days") +
  guides(fill = FALSE, col = FALSE )


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
  geom_hline(yintercept = latest_submission*c(1:4), col = "darkgrey") +
  geom_point(aes(margin, time_opened, col = Approved, shape = Approved)) +
  geom_point(data = ~filter(.x, devel), aes(margin, time_opened), col = "grey") +
  ggplot2::annotate(geom = "rect", xmin = 0, ymin = latest_submission,
            xmax = max(df2$margin, na.rm = TRUE), ymax = max(df2$time_opened) + 10, 
            fill = "orange", alpha = 0.25) +
  ggplot2::annotate(geom = "rect", xmin = 0, ymin = 0,
            xmax = 30, ymax = max(df2$time_opened) + 10, 
            fill = "red", alpha = 0.25) +
  ggplot2::annotate(geom = "text", x = 100, y = 540, 
                    label = "Missed release") +
  ggplot2::annotate(geom = "text", x = 13, y = 700, 
                    label = "Submitted right before a release", 
                    angle = 90, hjust = 1, vjust = 1) +
  scale_x_continuous(expand = expansion(add = 1)) +
  scale_y_continuous(expand = expansion(add = 9)) +
  scale_color_discrete(na.value = "grey") +
  labs(x = "Days till release", y = "Days open",
       title = "Packages not closed the same day as submitted",
       subtitle = "In red the worse time to submit. Each horitzonal bar indicates a missed release")


## ----open_releases--------------------------------------------------------------------------------
df2 %>% 
  ggplot() +
  geom_point(aes(created, time_opened, col = Approved, shape = Approved)) +
  geom_vline(data = releases, aes(xintercept = date), alpha = 0.5, 
             col = "darkgreen") + # Release dates
  geom_text(data = releases, aes(x = date, y = rep(600, 10),
           label = release), col = "#1a81c2") + # Release dates
  scale_y_continuous(expand = expansion(add = 6)) +
  scale_data +
  labs(x = element_blank(), y = "Days open", 
       title = "Time open") 


## ----pkg_source-----------------------------------------------------------------------------------
df2 %>% 
  filter(Approved != "Ongoing",
         n_packages == 1) %>% 
  group_by(same_submitter = ifelse(pkg_repo != user, "No", "Yes")) %>% 
  count(Approved) %>% 
  mutate(ratio = n/sum(n), total = sum(n), pos = paste0(n, collapse = "/")) %>% 
  ggplot() +
  geom_col(aes(fct_relevel(same_submitter, c("Yes", "No")), n, fill = Approved)) +
  geom_text(aes(fct_relevel(same_submitter, c("Yes", "No")), total*1.05, label = pos)) +
  labs(x = "Repository belongs to submitter", y = "Submissions",
       title = "Success if repository belongs to the submitter",
       subtitle = "The fraction indicates: Not approved/approved")


## ----time_rejected--------------------------------------------------------------------------------
df2 %>% 
  filter(Approved == "No") %>% 
  ggplot() + 
  geom_histogram(aes(time_opened), bins = 50) +
  fill_bioc +
  labs(x = "Days open", y = "Issues", 
       title = "Most not approved packages are closed the same day") 


## ----repolink-------------------------------------------------------------------------------------
rejected <- df2 %>% 
  filter(Approved == "No")
rejected %>% 
  count(n_packages) %>% 
  filter(n_packages != 1) %>% 
  knitr::kable(col.names = c("Number of packages", "Times"), 
               caption = "Issues with more than one package",
               align = "c")


## ----repolinks------------------------------------------------------------------------------------
rejected %>%
  filter(n_packages == 1) %>% 
  count(package, sort = TRUE) %>% 
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


## ----ending---------------------------------------------------------------------------------------
rejected %>% 
  filter(n_packages == 1) %>% 
  mutate(ending = str_extract(package, "\\..+$")) %>% 
  select(id, repos, package, ending) %>% 
  filter(!is.na(ending)) %>% 
  count(ending, sort = TRUE) %>% 
  head() %>% 
  knitr::kable(col.names = c("Ending", "Number of issues"), 
               caption = "Ending of rejected issues",
               align = "c")


## ----labels---------------------------------------------------------------------------------------
rejected %>% 
  count(n_labels) %>% 
  knitr::kable(col.names = c("Number of labels", "Number of issues"),
               caption = "Labels of rejected packages",
               align = "c")


## ----upset----------------------------------------------------------------------------------------
ups <- rejected %>% 
  filter(n_labels > 1) %>% 
  select(i, id, labels) %>% 
  unnest(labels) %>% 
  pivot_wider(names_from = labels, values_from = id) %>% 
  mutate(across(where(is.numeric), function(x){!is.na(x)})) %>%
  mutate(across(where(is.logical), as.integer))
ups <- ups[, -1]
ups <- as.data.frame(ups)
colnames(ups) <- make.names(colnames(ups))
library("UpSetR")
upset(ups, order.by = "freq", decreasing = TRUE, 
      nintersects = 10)


## ----reproducibility, echo = FALSE----------------------------------------------------------------
## Reproducibility info
options(width = 120)
sessioninfo::session_info()

