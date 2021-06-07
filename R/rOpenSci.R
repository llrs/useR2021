library("socialGH")
library("tidyverse")
library("ggrepel")
library("patchwork")

## ----downloading, eval=FALSE,cache=FALSE----------------------------------------------------------
repo <- "ropensci/software-review"
gi <- get_issues(repo)
i <- unique(gi$id)
# gt <- get_timelines(repo) there's a limit of 40000 so we cannot use it.
gt <- lapply(i, get_timelines, repository = repo)
gt2 <- do.call(rbind, gt)
gi2 <- gi %>%
  select(-n_comments) %>%
  mutate(actor = poster, event = "created")
column <- intersect(colnames(gt2), colnames(gi2))
g <- rbind(gi2[, colnames(gt2)], gt2) %>%
  arrange(id, created) %>%
  group_by(id) %>%
  mutate(event_n = 1:n(),
         event = unlist(event, FALSE, FALSE),
         state = ifelse(event_n == 1, list("opened"), state))
pr <- g %>%
  ungroup() %>%
  filter(event %in% c("merged", "committed")) %>%
  pull(id)
g2 <- filter(g, !id %in% pr)
saveRDS(g2, file = "output/github_rOpenSci_data.RDS")


## ----read-----------------------------------------------------------------------------------------
theme_set(theme_minimal())
get_element <- function(x, name) {
  if (!is.null(names(x))) {
    getElement(x, name)
  } else {
    NA_character_
  }
} 
init <- g2 %>% 
  filter(id != 26) %>%
  mutate(reviewer = vapply(assignee, get_element, name = "user", character(1)),
         actor = vapply(actor, get_element, name = "user", character(1L))) %>% 
  filter(!id %in% c(1:4, 6:8, 16, 14, 49, 84, 135))
saveRDS(init, file = "output/github_rOpenSci_data__cleaned.RDS")
