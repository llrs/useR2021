## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, message = FALSE, 
                      echo = FALSE, cache = FALSE)


## ----downloading, eval=FALSE,cache=FALSE----------------------------------------------------------
## library("socialGH")
## repo <- "ropensci/software-review"
## gi <- get_issues(repo)
## i <- unique(gi$id)
## # gt <- get_timelines(repo) there's a limit of 40000 so we cannot use it.
## gt <- lapply(i, get_timelines, repository = repo)
## gt2 <- do.call(rbind, gt)
## library("dplyr")
## gi2 <- gi %>%
##   select(-n_comments) %>%
##   mutate(actor = poster, event = "created")
## column <- intersect(colnames(gt2), colnames(gi2))
## g <- rbind(gi2[, colnames(gt2)], gt2) %>%
##   arrange(id, created) %>%
##   group_by(id) %>%
##   mutate(event_n = 1:n(),
##          event = unlist(event, FALSE, FALSE),
##          state = ifelse(event_n == 1, list("opened"), state))
## pr <- g %>%
##   ungroup() %>%
##   filter(event %in% c("merged", "committed")) %>%
##   pull(id)
## g2 <- filter(g, !id %in% pr)
## saveRDS(g2, file = "static/20200902_github_rOpenSci_data.RDS")


## ----read-----------------------------------------------------------------------------------------
init <- readRDS(here::here("static", "20200902_github_rOpenSci_data.RDS"))
library("tidyverse")
theme_set(theme_minimal())
get_element <- function(x, name) {
  if (!is.null(names(x))) {
    getElement(x, name)
  } else {
    NA_character_
  }
} 
init <- init %>% 
  filter(id != 26) %>%
  mutate(reviewer = vapply(assignee, get_element, name = "user", character(1)),
         actor = vapply(actor, get_element, name = "user", character(1L)))


## ----by-issue-------------------------------------------------------------------------------------
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

by_issue <- init %>% 
  group_by(id) %>% 
  summarize(time_window = difftime(max(created), min(created), units = "days"),
            events = n(), 
            diff_users = n_distinct(actor),
            diff_events = n_distinct(event),
            assignments = sum(event %in% "assigned"),
            reassigned = any(event %in% "unassigned"),
            assigners = list(reviewer[event %in% "assigned"]),
            unassigners = list(reviewer[event %in% "unassigned"]),
            editors = list(reviewers(unlist(assigners, FALSE, FALSE), 
                                    unlist(unassigners, FALSE, FALSE))),
            editor_comments = sum(event == "commented" & 
                                      actor %in% unlist(editors), na.rm = TRUE),
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
  mutate(n_reviewers = lengths(editors),
         n_closers = lengths(closer))

by_issue1 <- init %>% 
  count(event) %>% 
  filter(event != "created") %>% 
  pivot_wider(values_from = n, names_from = event, values_fill = 0) %>% 
  nest_by(.key = "event")
by_issue2 <- init %>% 
  count(actor) %>% 
  pivot_wider(values_from = n, names_from = actor, values_fill = 0) %>% 
  nest_by(.key = "actor")

by_issue <- by_issue %>% 
  inner_join(by_issue1) %>% 
  inner_join(by_issue2)


## ----by-user--------------------------------------------------------------------------------------
by_user <- init %>% 
  group_by(actor) %>% 
  summarize(
    actions = n(),
    issues_participated = n_distinct(id),
    issues = list(unique(id)),
    events_participated = n_distinct(event),
  ) %>% 
  mutate(is_editor = actor %in% unlist(by_issue$editors, FALSE, FALSE))

by_user1 <- init %>% 
  group_by(actor) %>% 
  count(event) %>% 
  pivot_wider(values_from = n, names_from = event, values_fill = 0) %>% 
  nest_by(.key = "event")
by_user2 <- init %>% 
  group_by(actor) %>% 
  count(id) %>% 
  pivot_wider(values_from = n, names_from = id, values_fill = 0) %>% 
  nest_by(.key = "ids")

by_user <- by_user %>% 
  inner_join(by_user1) %>% 
  inner_join(by_user2)


## ----first-plot-----------------------------------------------------------------------------------
init %>% 
  ggplot(aes(created, id, col = fct_lump(event, 7))) +
  geom_point() +
  labs(x = element_blank(), y = "Issue", col = "Event",
       title = "Issues opened on rOpenSci") +
  theme_minimal()


## ----by-users-------------------------------------------------------------------------------------
library("ggrepel")
editors <- by_user %>% 
  filter(is_editor) %>% 
  distinct(actor) %>% 
  pull(actor)
by_user %>% 
  filter(is_editor) %>%
  unnest(event) %>% 
  filter(commented != 0) %>% 
  ggplot() + 
  geom_point(aes(actions, issues_participated)) +
  geom_text_repel(aes(actions, issues_participated, label = actor)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(size = "Users", col = "Different events", x = "Comments", y = "Issues",
       title = "Editors involvement") +
  theme_minimal()


## ----by-user2-------------------------------------------------------------------------------------
by_user %>% 
  filter(!is_editor & actor != "bioc-issue-bot" & !is.na(actor)) %>%
  unnest(event) %>% 
  filter(commented != 0) %>% 
  ggplot() + 
  geom_count(aes(actions, issues_participated)) +
  scale_color_viridis_d() + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  geom_text_repel(aes(actions, issues_participated, label = actor), 
                data = . %>% filter(issues_participated > 10 | actions > 100)) +
  labs(size = "Users", col = "Different events", x = "Comments", y = "Issues",
       title = "Users involvement") +
  theme_minimal()


## ----events-view----------------------------------------------------------------------------------
init %>% 
  group_by(event) %>% 
  summarize(n = n(),
            diff_issues = n_distinct(id),
            diff_author = n_distinct(actor)) %>% 
  ggplot() +
  geom_abline(slope = 1, col = "grey") +
  geom_text_repel(aes(diff_issues, diff_author, label = event, col = n, size = n)) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", limits = c(1, NA)) +
  scale_color_binned(trans = "log10", guide = "bins") +
  scale_size(trans = "log10") +
  guides(size = FALSE) +
  labs(title = "Events", x = "Number of different issues", 
       y = "Number of different users", label = "Events", size = "Events",
       col = "Events") +
  theme_minimal()


## ----second-plot----------------------------------------------------------------------------------
init %>% 
  count(event) %>% 
  filter(!event == "created") %>% 
  ggplot() +
  geom_tile(aes(id, fct_reorder(event, n, sum) , col = n)) +
  scale_color_viridis_c(trans = "log10") +
  labs(x = "Issue", y = element_blank(), col = "Times", 
       title = "Events per issue") +
  theme_minimal()


## ----events-time----------------------------------------------------------------------------------
init %>% 
  count() %>% 
  ggplot() +
  geom_histogram(aes(n), binwidth = 5) +
  labs(x = "Events", y = "Issues", title = "Events per issue") +
  theme_minimal()


## ----events-time2---------------------------------------------------------------------------------
init %>% 
  summarize(time = difftime(created[created == max(created)], created[event == "created"],
                            units = "days"),
            n = n(),
            slope = n/as.numeric(time)) %>% 
  ggplot(aes(time, n)) +
  geom_point(aes(col = slope)) +
  scale_color_continuous(trans = "log10") +
    labs(y = "Events", x = "Time (days)", 
       col = "Events per days", 
       title = "Number of events and time")  +
  theme(axis.text.x = element_text())


## ----events-time3---------------------------------------------------------------------------------
init %>% 
  summarize(time = difftime(created[created == max(created)], created[event == "created"],
                            units = "days"),
            n = n(),
            slope = n/as.numeric(time)) %>% 
  ungroup() %>% 
  filter(time <= median(time)) %>% 
  ggplot(aes(time, n)) +
  geom_point(aes(col = slope)) +
  scale_color_continuous(trans = "log10") +
  labs(y = "Events", 
       x = "Time (days)", 
       col = "Events per days", 
       title = "Number of events and time",
       subtitle = "A zoom to the fastest half") +
  scale_x_continuous(breaks = 1:7*14, labels = function(x) {paste(x/7, "weeks")}) +
  theme_minimal()


## ----assignments----------------------------------------------------------------------------------
by_issue %>% 
  ggplot() +
  geom_count(aes(as.factor(n_reviewers), assignments, col = reassigned, 
                 shape = reassigned)) +
  labs(x = "Final editor", y = "Assigned", title = "Editors", 
       col = "Reassigned?", shape = "Reassigned?", size = "Submissions") +
  scale_color_brewer(labels = c("No", "Yes"), type = "qual") +
  scale_shape(labels = c("No", "Yes")) +
  scale_size(trans = "log10")


## ----submission-acceptance------------------------------------------------------------------------
revi <- by_issue %>% 
  filter(lengths(editors) != 0)

reviwer_didnt_close <- revi %>% 
  filter(!is.na(closer),
         closer %in% unlist(editors, FALSE, FALSE)) %>% 
  pull(id)

author_closed <- revi %>% 
  filter(!is.na(closer), submitter %in% closer) %>% 
  pull(id)

revi_sum <- revi %>% 
  filter(!id %in% author_closed,
         n_reviewers == 1) %>% 
  mutate(editor = unlist(editors, FALSE, FALSE),
         editor_commented = ifelse(editor_comments != 0, "commented", "not commented")) %>% 
  group_by(editor, editor_commented) %>% 
  count() %>% 
  group_by(editor) %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(editor, editor_commented)

revi %>% 
  ggplot() +
  geom_histogram(aes(editor_comments, fill = n_reviewers), binwidth = 1) +
  labs(x = element_blank(), y = "Editor comments", 
       title = "Comments from editors") + 
  theme_minimal()


## ----events-days----------------------------------------------------------------------------------
trelative <- function(x) {
  created <- x$created
  event <- x$event
  start <- created[event == "created"]
  k <- event == "closed"
  if (any(k)){
    closing <- created[which.max(k)]
  } else {
    closing <- max(created)
  }
  
  o <- difftime(created[!is.na(created)], start, units = "days")
  as.numeric(o)
}
full <- init %>% 
  nest_by() %>% 
  summarize(time_relative = trelative(data), created = data$created) %>% 
  inner_join(init) %>% 
  unique() %>% 
  mutate(presubmission = any(grepl("[Pp]re-?[Ss]ubmission", title))) %>% 
  mutate(presubmission = ifelse(presubmission, "Presubmission", "Submission"))
full %>% 
  arrange(id, created, event) %>% 
  mutate(event_n = 1:n()) %>% 
  filter(event_n <= event_n[which.max(event == "closed")]) %>% 
  ggplot() +
  geom_line(aes(time_relative, event_n, col = id, group = id)) +
  facet_wrap(~presubmission, scales = "free") +
  labs(title = "Events along time till closed",
       x = "Days", y = element_blank(), col = "Issue") + 
  theme_minimal()


## ----events-user-distribution---------------------------------------------------------------------
p1 <- ggplot(by_issue) +
  geom_bar(aes(as.factor(diff_events))) +
  labs(y = "Issues", x = element_blank(), 
       title = "Different events in the issue")
p2 <- ggplot(by_issue) +
  geom_bar(aes(as.factor(diff_users))) +
  labs(y = element_blank(), x = element_blank(), 
       title = "Different users involved in the issue")
library("patchwork")
p1 + p2


## ----actor-event-types----------------------------------------------------------------------------
by_issue %>% 
  mutate(diff_actors = factor(diff_users, levels = seq_len(max(diff_users)))) %>% 
  ggplot() +
  geom_count(aes(diff_actors, as.factor(diff_events))) +
  labs(x = "Users", y = "Events", title = "Users involved and different events",
       size = "Issues") + 
  scale_radius() +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()


## ----actors-events--------------------------------------------------------------------------------
by_issue %>% 
  mutate(diff_actors = factor(diff_users, levels = seq_len(max(diff_users)))) %>% 
  group_by(diff_actors) %>% 
  count(events) %>% 
  ggplot() +
  geom_jitter(aes(as.factor(diff_actors), events, size = n), 
             height = 0) +
  labs(x = "Users", y = "Events", size = "Issues",
       title = "Users involved on the issues and events") +
  scale_x_discrete(drop = FALSE) + 
  scale_size(breaks = c(seq(0, 300, by = 50)))


## ----who------------------------------------------------------------------------------------------
top_events <- 35
by_user %>%
  arrange(-actions) %>% 
  top_n(top_events, actions) %>% 
  select(actor, event) %>% 
  unnest(event) %>% 
  pivot_longer(names_to = "event", cols = created:unlabeled, values_to = "n") %>% 
  filter(n != 0) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_tile(aes(fct_reorder2(event, -n, actor), 
                fct_reorder(actor, n, .fun = sum), 
                fill = n)) +
  scale_fill_viridis_c(trans = "log10", expand = expansion()) +
  labs(title = "Events by users", y = element_blank(), x = element_blank(),
       fill = "Events") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # coord_flip()


## ----comments-plot--------------------------------------------------------------------------------
comments <- init %>%
  mutate(n = sum(event %in% "assigned"),
         assigners = list(reviewer[event == "assigned"]),
         unassigners = list(reviewer[event == "unassigned"]),
         reasigned  = any(event %in% "unassigned"),
         reviewers = list(reviewers(unlist(assigners, FALSE, FALSE), 
                                    unlist(unassigners, FALSE, FALSE))),
         reviewers_n = lengths(assigners)-lengths(unassigners),
         creator = unique(actor[event == "created"])) %>% 
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
  geom_abline(slope = 1, col = "grey") +
  geom_count(aes(author, reviewer)) +
  labs(title = "Comments", x = "Authors", y = "Editor(s)", size = "Issues") +
  theme_minimal() 


## ----closing--------------------------------------------------------------------------------------
closed_c <- full %>% 
  mutate(comment_n = cumsum(event == "commented")) %>% 
  filter(event == "closed") %>% 
  filter(event_n == max(event_n)) %>% 
  select(id, time_relative, comment_n, event_n)

closed_c <- mutate(closed_c, Closed = TRUE)
aftr <- full %>% 
  filter(event %in% c("commented", "created") & actor != "bioc-issue-bot") %>% 
  mutate(comment_n = cumsum(event == "commented" & actor != "bioc-issue-bot"),
         Closed = FALSE) %>% 
  select(id, time_relative, comment_n, event_n, Closed) %>% 
  rbind(closed_c) %>% 
  group_by(id) %>% 
  mutate(Closed = event_n > ifelse(any(Closed), event_n[Closed], Inf))
aftr %>% 
  group_by(id) %>% 
  count(Closed) %>% 
  filter(n_distinct(Closed) >= 2) %>% 
  pivot_wider(names_from = Closed, values_from = n) %>% 
  ggplot() + 
  geom_count(aes(`TRUE`, `FALSE`)) +
  labs(title = "Comments before and after being closed",
       size = "Issues", x = "Comments after being closed",
       y = "Comments before being closed")


## ----mentions-------------------------------------------------------------------------------------
mentioned <- init %>% 
  ungroup() %>% 
  filter(event == "mentioned") %>% 
  distinct(actor) %>% 
  pull()
non_editors <- mentioned[!mentioned %in% editors]
creators <- init %>% 
  filter(event == "created")
init %>% 
  filter(actor %in% non_editors,
         !event %in% c("mentioned", "subscribed")) %>% 
  anti_join(creators) %>% 
  count(event) %>% 
  ggplot() +
  geom_tile(aes(id, fct_reorder(event, n, sum), col = n)) +
  labs(x = "Issue", y = "Event", col = "Times", 
       title = "What do people mentioned?")


## ----labels-general-------------------------------------------------------------------------------
labels <- full %>% 
  ungroup() %>% 
  filter(event == "labeled") %>% 
  mutate(label = unlist(label),
         label = case_when(
           label == "reviewer-requested" ~ "2/seeking-reviewer(s)",
           label == "seeking-reviewers" ~ "2/seeking-reviewer(s)",
           label == "2/seeking-reviewers" ~ "2/seeking-reviewer(s)",
           label == "3/reviewers-assigned" ~ "3/reviewer(s)-assigned",
           label == "4/review-in-awaiting-changes" ~ "4/review(s)-in-awaiting-changes",
           label == "review-in-awaiting-changes" ~ "4/review(s)-in-awaiting-changes",
           label == "changes-in-awaiting-response" ~ "4/review(s)-in-awaiting-changes",
           label == "5/awaiting-reviewer-response" ~ "5/awaiting-reviewer(s)-response",
           label == "approved" ~ "6/approved",
           label == "topic:linquistics" ~ "topic:linguistics",
           TRUE ~ label
         ))


## ----labels_plot_overview-------------------------------------------------------------------------
ord_label <- c("0/presubmission",
               "1/editor-checks",  
               "2/seeking-reviewer(s)", "3/reviewer(s)-assigned", 
               "4/review(s)-in-awaiting-changes",
               "5/awaiting-reviewer(s)-response", 
               "6/approved", 
               "TIMEOUT", "ERROR", 
               "WARNINGS", "OK")
labels %>% 
  group_by(id) %>% 
  count(label, sort = TRUE) %>% 
  filter(label %in% ord_label) %>% 
  ggplot() +
  geom_tile(aes(id, label, fill = n)) +
  labs(x = "Issue", y = element_blank(), title = "Labels related to the review process",
       fill = "Times")


## ----labels_topic---------------------------------------------------------------------------------
labels %>% 
  group_by(id) %>% 
  count(label, sort = TRUE) %>% 
  filter(grepl(pattern = "^topic", label)) %>% 
  mutate(label = gsub(pattern = "topic:", replacement = "", x = label)) %>% 
  ggplot() +
  geom_tile(aes(id, fct_reorder(label, n, .fun = sum), fill = n)) +
  labs(x = "Issue", y = element_blank(), title = "Topics", fill = "Times")


## ----topics-freq----------------------------------------------------------------------------------
labels %>% 
  group_by(id) %>% 
  count(label, sort = TRUE) %>% 
  filter(grepl(pattern = "^topic", label)) %>% 
  mutate(Topic = gsub(pattern = "topic:", replacement = "", x = label)) %>% 
  ungroup() %>% 
  count(Topic, sort = TRUE) %>% 
  slice_head(n = 10) %>% 
  knitr::kable()


## ----labels-other---------------------------------------------------------------------------------
labels %>% 
  group_by(id) %>% 
  count(label, sort = TRUE) %>% 
  filter(!grepl(pattern = "^topic", label) & !label %in% ord_label) %>% 
  mutate(label = gsub(pattern = "topic:", replacement = "", x = label)) %>% 
  ggplot() +
  geom_tile(aes(id, fct_reorder2(label, n, n), fill = n)) +
  labs(x = "Issue", y = element_blank(), title = "Other topics", fill = "Times")


## ----labels-step----------------------------------------------------------------------------------
labels %>% 
  filter(label %in% ord_label) %>% 
  group_by(label) %>% 
  summarize(days = median(time_relative)) %>% 
  filter(label != "0/presubmission") %>% 
  mutate(d = days -lag(days),
         time = round(if_else(is.na(d), days, d), 1),
         `Total days` = round(cumsum(time), 1)) %>% 
  select(label, time, `Total days`) %>% 
  knitr::kable(col.names = c("Step", "Median days", "Total days"))


## ----labels_ind-----------------------------------------------------------------------------------
lab_wide <- labels %>% 
  ungroup() %>% 
  filter(label %in% ord_label[2:7]) %>% 
  pivot_wider(id_cols = id, values_from = time_relative, names_from = label,
              values_fn = last) %>% 
  select(id, ord_label[2:7])
colnames(lab_wide)[2:7] <- as.character(c(1:6))
res <- lab_wide %>% 
  summarize(
    s1 = median(`1`, na.rm = TRUE),
    s2 = median(`2` - `1`, na.rm = TRUE),
    s3 = median(`3` - `2`, na.rm = TRUE),
    s4 = median(`4` - `3`, na.rm = TRUE),
    s5 = median(`5` - `4`, na.rm = TRUE),
    s6 = median(`6` - `5`, na.rm = TRUE))
colnames(res) <- ord_label[2:7]
res %>% 
  pivot_longer(cols = ord_label[2:7]) %>% 
  mutate(`Median days` = round(value, 1),
         `Total days` = round(cumsum(value), 1)) %>% 
  select(name, `Median days`, `Total days`) %>% 
  knitr::kable()


## ----reproducibility, echo = FALSE----------------------------------------------------------------
## Reproducibility info
options(width = 120)
sessioninfo::session_info()

