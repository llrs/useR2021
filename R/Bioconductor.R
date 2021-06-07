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
  for (reviwer in names(ta)) {
    x <- ta[reviwer] - tu[reviwer]
    if (x >= 1 | is.na(x)) {
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
