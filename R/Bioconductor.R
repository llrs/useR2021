## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, message = FALSE, 
                      echo = FALSE, cache = TRUE)


## ----downloading, eval=FALSE----------------------------------------------------------------------
## library("socialGH")
## repo <- "Bioconductor/Contributions"
## gi <- get_issues(repo)
## i <- unique(gi$id)
## # gt <- get_timelines(repo) there's a limit of 40000 so we cannot use it.
## gt <- lapply(i, get_timelines, repository = repo)
## gt2 <- do.call(rbind, gt)
## library("tidyverse")
## theme_set(theme_minimal())
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
## g2 <- g %>%
##   filter(!id %in% pr) %>%
##   mutate(approved = any(label == "3a. accepted"),
##          Approved = case_when(any(approved) ~ "Yes",
##                               sum(event == "closed") >= sum(event == "reopened") ~ "No",
##                               TRUE ~ "Ongoing"))
## saveRDS(g2, file = "static/20200818_github_data.RDS")


## ----reading--------------------------------------------------------------------------------------
library("socialGH")
library("tidyverse")
theme_set(theme_minimal())
# If not closed add the closing time of today
g2 <- readRDS(here::here("static", "20200818_github_data.RDS")) %>% 
  mutate(
    approved = any(str_detect(unlist(label[event %in% c("labeled", "unlabeled")]), "accept")),
    Approved = case_when(
      any(approved) ~ "Yes",
      sum(event == "closed") == 0 ~ "Ongoing",
      sum(event == "closed") >= sum(event == "reopened") ~ "No",
      TRUE ~ "Ongoing"))


## ----first_plot-----------------------------------------------------------------------------------
link_issue <- function(x) {
  socialGH::link_issue("Bioconductor/Contributions", x)
}
releases <- data.frame(release = paste0("3.", 3:12),
           date = as.POSIXct(c("2016/04/04", "2016/10/18", "2017/04/25", 
                            "2017/10/31", "2018/05/01", "2018/10/31", 
                            "2019/05/03", "2019/10/30", "2020/04/28",
                            "2020/10/01"), format = "%Y/%m/%d"),
           stringsAsFactors = FALSE)

scale_data <- scale_x_datetime(expand = expansion(add = 10), 
               limits = as.POSIXct(c("2016-06-01", "2020-06-10"), "%Y-%m-%d"))

cut <- 5
g2 %>% 
  ggplot() +
  geom_point(aes(created, id, 
                 col = fct_lump_n(event, cut),
                 shape = fct_lump_n(event, cut))) + 
  geom_vline(xintercept = releases$date, col = "#1a81c2") + # Releases dates
  geom_text(data = releases, aes(x = date, y = c(rep(1200, 5), rep(300, 5)),
           label = release)) + # Release dates
  scale_data +
  labs(x = "Events", y = "Issue", col = "Type", shape = "Type",
       title = "Events on issues") +
  scale_color_viridis_d() +
  theme(legend.position = "bottom", legend.direction = "horizontal") + 
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))


## ----exclude_testing------------------------------------------------------------------------------
g2 <- filter(g2, !id %in% c(1:5, 51, 587, 764, 1540, 1541))


## ----summarizing----------------------------------------------------------------------------------
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
  if (any(k)){
    closing <- created[which.max(k)]
  } else {
    closing <- max(created)
  }
  
  o <- difftime(created[!is.na(created)], start, units = "days")
  as.numeric(o)
}
g2 <- g2 %>% 
  mutate(reviewer = vapply(assignee, get_element, name = "user", character(1)),
         actor = vapply(actor, get_element, name = "user", character(1L)))
full <- g2 %>%  
  nest_by() %>% 
  summarize(time_relative = trelative(data), created = data$created) %>% 
  inner_join(g2) %>% 
  unique()


## ----by_issue-------------------------------------------------------------------------------------
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

by_issue <- g2 %>% 
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

by_issue1 <- g2 %>% 
  count(event) %>% 
  filter(event != "created") %>% 
  pivot_wider(values_from = n, names_from = event, values_fill = 0) %>% 
  nest_by(.key = "event")
by_issue2 <- g2 %>% 
  count(actor) %>% 
  pivot_wider(values_from = n, names_from = actor, values_fill = 0) %>% 
  nest_by(.key = "actor")

by_issue <- by_issue %>% 
  inner_join(by_issue1) %>% 
  inner_join(by_issue2)


## ----by_user--------------------------------------------------------------------------------------
by_user <- g2 %>%
  group_by(actor) %>% 
  summarize(
    actions = n(),
    issues_participated = n_distinct(id),
    issues = list(unique(id)),
    events_participated = n_distinct(event),
  ) %>% 
  mutate(is_reviewer = actor %in% unlist(by_issue$reviewers, FALSE, FALSE))

by_user1 <- g2 %>% 
  group_by(actor) %>% 
  count(event) %>% 
  pivot_wider(values_from = n, names_from = event, values_fill = 0) %>% 
  nest_by(.key = "event")
by_user2 <- g2 %>% 
  group_by(actor) %>% 
  count(id) %>% 
  pivot_wider(values_from = n, names_from = id, values_fill = 0) %>% 
  nest_by(.key = "ids")

by_user <- by_user %>% 
  inner_join(by_user1) %>% 
  inner_join(by_user2)
library("ggrepel")
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


## ----by_event-------------------------------------------------------------------------------------
by_event1 <- g2 %>% 
  group_by(event) %>% 
  count(actor) %>% 
  pivot_wider(names_from = actor, values_from = n, values_fill = 0) %>% 
  nest_by(.key = "actor")
by_event2 <- g2 %>% 
  group_by(event) %>% 
  count(id) %>% 
  pivot_wider(names_from = id, values_from = n, values_fill = 0) %>% 
  nest_by(.key = "id") 

by_event <- g2 %>% 
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
by_actor_event_id <- g2 %>% 
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


## ----second_plot----------------------------------------------------------------------------------
full %>% 
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


## ----events_time----------------------------------------------------------------------------------
unit <- "days"
g2 %>% 
  group_by(id) %>% 
  filter(event_n == max(event_n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_histogram(aes(event_n), bins = 40) +
  labs(x = "Events", y = "Issues", title = "Events per issue") + 
  theme_minimal()


## ----events_time2---------------------------------------------------------------------------------
diff_time <- g2 %>% 
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


## ----submission_acceptance------------------------------------------------------------------------
revi <- by_issue %>% 
  filter(lengths(reviewers) != 0)

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
      geom_col(aes(reviewer, perc)) +
      labs(title = paste(Approved, reviewer_commented), x = element_blank(), y = "Percentage") +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_brewer(type =  "qual") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) ),
    plot_abs = list(
    ggplot(data) +
      geom_col(aes(reviewer, n)) +
      labs(title = paste(Approved, reviewer_commented), x = element_blank(), y = "Issues") +
      scale_x_discrete(drop = FALSE) +
      scale_fill_brewer(type =  "qual") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) ))
library("patchwork")

r$plot_relative[[1]] <- r$plot_relative[[1]] + theme(axis.text.x = element_blank())
r$plot_relative[[2]] <- r$plot_relative[[2]] + theme(axis.text.x = element_blank())
r$plot_relative[[2]] <- r$plot_relative[[2]] + labs(y = element_blank())
r$plot_relative[[4]] <- r$plot_relative[[4]] + labs(y = element_blank())
ptch <- wrap_plots(r$plot_relative)
ptch  

r$plot_abs[[1]] <- r$plot_abs[[1]] + theme(axis.text.x = element_blank())
r$plot_abs[[2]] <- r$plot_abs[[2]] + theme(axis.text.x = element_blank())
r$plot_abs[[2]] <- r$plot_abs[[2]] + labs(y = element_blank())
r$plot_abs[[4]] <- r$plot_abs[[4]] + labs(y = element_blank())
ptch2 <- wrap_plots(r$plot_abs)
ptch2 


## ----events_days----------------------------------------------------------------------------------
full %>% 
  ggplot() +
  geom_line(aes(time_relative, event_n, col = id, group = id)) +
  facet_wrap(~Approved) +
  labs(title = "Events along time", subtitle = "Approved?",
       x = "Days", y = element_blank(), col = "Issue") + 
  theme_minimal()


## ----events_user_distribution---------------------------------------------------------------------
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


## ----actor_event_types----------------------------------------------------------------------------
by_issue %>% 
  mutate(diff_actors = factor(diff_users, levels = 1:13)) %>% 
  ggplot() +
  geom_count(aes(diff_actors, as.factor(diff_events))) +
  labs(x = "Users", y = "Events", title = "Users involved and different events",
       size = "Issues") + 
  scale_radius() +
  scale_x_discrete(drop = FALSE) 


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
  geom_tile(aes(fct_reorder2(event, -n, actor), 
                fct_reorder(actor, n, .fun = sum), 
                fill = n)) +
  scale_fill_viridis_c(trans = "log10", expand = expansion()) +
  labs(title = "Events by users", y = element_blank(), x = element_blank(),
       fill = "Events") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # coord_flip()


## ----comments_plot--------------------------------------------------------------------------------
comments <- full %>%
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
  geom_count(aes(author, reviewer)) +
  labs(title = "Comments", x = "Authors", y = "Reviewer(s)", size = "Issues") +
  theme_minimal()


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


## ----user_events----------------------------------------------------------------------------------
bm3 <- bioc_bot %>% 
  filter(Approved != "Ongoing") %>% 
  group_by(id, reason) %>% 
  summarize(n = n(), Approved = unique(Approved)) %>% 
  mutate(total = sum(n)) %>% 
  group_by(id) %>% 
  mutate(perc = n/total)

bm3 %>% 
  group_by(reason) %>% 
  filter(n_distinct(Approved) >= 2 | all(Approved == "No")) %>% 
  ggplot() +
  geom_jitter(aes(Approved, n, col = Approved, shape = Approved), height = 0) +
  facet_wrap(~reason, scales = "free", ncol = 3) +
  labs(x = element_blank(), y = "Comments", 
       title = "Diferences between accepted and non accepted issues by bioc-issue-bot comments")


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


## ----delays---------------------------------------------------------------------------------------

closed_c <- full %>% 
  mutate(comment_n = cumsum(event == "commented" & actor != "bioc-issue-bot")) %>% 
  filter(event == "closed") %>% 
  filter(event_n == max(event_n)) %>% 
  select(id, time_relative, comment_n, event_n, Approved)

full %>% 
  filter(event %in% c("commented", "created") & actor != "bioc-issue-bot") %>% 
  mutate(comment_n = cumsum(event == "commented" & actor != "bioc-issue-bot")) %>% 
  ggplot() +
  geom_point(aes(time_relative, comment_n, col = id),  data = closed_c, shape = "square") +
  geom_line(aes(time_relative, comment_n, group = id, col = id)) +
  facet_wrap(~Approved, scales = "free") +
  guides(col = FALSE) + 
  labs(x = "Since creation of the issue (days)", y = "Comments", title = "Comments on issues")


## ----closing--------------------------------------------------------------------------------------
closed_c <- mutate(closed_c, Closed = TRUE)
aftr <- full %>% 
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


## ----successful_build-----------------------------------------------------------------------------
full %>% 
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


## ----time_accepted--------------------------------------------------------------------------------
bc %>% 
  filter(!is.na(Accepted) & !is.na(`Reviewer comment`) & Approved == "Yes") %>% 
  ggplot() +
  geom_linerange(aes(y = id, xmax = Accepted, xmin = `Reviewer comment`)) +
  labs(title = "Time comment from the reviewer after successful build and acceptance",
       x = "Days since submission", y = "Issue") +
  theme_minimal()


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


## ----acceptance_plot------------------------------------------------------------------------------
p2 <- bc %>% 
  ggplot() +
  geom_histogram(aes(Accepted), binwidth = 1) +
  labs(title = "Days till acceptance",
       x = "Days", y = "Issues") +
  theme_minimal()
p2 / p2 + coord_cartesian(xlim = c(0, 20)) + 
  labs(title = element_blank(), subtitle = "A zoom")


## ----time_acceptance_plot-------------------------------------------------------------------------
bc %>% 
  ggplot() +
  geom_histogram(aes(time_acceptance), binwidth = 1) +
  labs(title = "Days between comment from reviwers and acceptance",
       x = "Days", y = "Issues") +
  theme_minimal()


## ----times_phases---------------------------------------------------------------------------------
p3 <- bc %>% 
  filter(!is.na(`Built correctly`) & !is.na(time_review)) %>% 
  ggplot() + 
  geom_point(aes(`Built correctly`, time_review, 
                 col = Approved, shape = Approved)) +
  labs(y = "Time between built and review (days)") +
  theme_minimal()
p4 <- bc %>% 
  filter(!is.na(time_review), !is.na(time_acceptance)) %>% 
  ggplot() + 
  geom_point(aes(time_review, time_acceptance), 
             col = "#00BFC4", shape = "triangle") +
  labs(x = "Time between built and review (days)",
       y = "Time between review and acceptance (days)") +
  theme_minimal()
p3 + p4


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
  ungroup() %>% 
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
               "3b. declined", "3c. inactive", "ABNORMAL", 
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


## ----significant_label, eval=FALSE, include=FALSE-------------------------------------------------
## labels %>%
##   group_by(id) %>%
##   count(label, Approved, sort = TRUE) %>%
##   ungroup() %>%
##   filter(Approved != "Ongoing") %>%
##   group_by(label) %>%
##   filter(n_distinct(Approved) == 2, n_distinct(n) > 2) %>%
##   nest_by() %>%
##   mutate(t = list(t.test(n ~ Approved, data = data))) %>%
##   mutate(broom::glance(t)) %>%
##   ungroup() %>%
##   mutate(adj.p.value = p.adjust(p.value)) %>%
##   filter(adj.p.value < 0.05) %>%
##   pull(label)


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

