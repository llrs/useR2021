## ----setup, include = FALSE-----------------------------------------------------------------------


## ----prepare, eval = FALSE------------------------------------------------------------------------
# Downloading the cransays repository branch history
download.file("https://github.com/lockedata/cransays/archive/history.zip",
              destfile = "output/cransays-history.zip")
path_zip <- here::here("output", "cransays-history.zip")
# We unzip the files to read them
dat <- unzip(path_zip, exdir = "static")
csv <- dat[grepl("*.csv$", x = dat)]
f <- lapply(csv, read.csv)
m <- function(x, y) {
  merge(x, y, sort = FALSE, all = TRUE)
}
updates <- Reduce(m, f) # Merge all files (Because the file format changed)
write.csv(updates, file = "output/cran_till_now.csv",  row.names = FALSE)
# Clean up
unlink("static/", recursive = TRUE)
unlink("output/cransays-history/", recursive = TRUE)
unlink("output/cransays-history.zip", recursive = TRUE)


## ----load-----------------------------------------------------------------------------------------
library("tidyverse")
library("lubridate")
library("hms")
path_file <- here::here("output", "cran_till_now.csv")
cran_submissions <- read.csv(path_file)
theme_set(theme_minimal()) # For plotting
col_names <- c("package", "version", "snapshot_time", "folder", "subfolder")
cran_submissions <- cran_submissions[, col_names]


## ----cran-holidays--------------------------------------------------------------------------------
holidays <- data.frame(
  start = as.POSIXct("18/12/2020", format = "%d/%m/%Y", tz = "UTC"), 
  end = as.POSIXct("04/01/2021", format = "%d/%m/%Y", tz = "UTC")
)


## ----reformat-------------------------------------------------------------------------------------
# Use appropriate class
cran_submissions$snapshot_time <- as.POSIXct(cran_submissions$snapshot_time,
                                             tz = "UTC")
# Fix subfolders structure
cran_submissions$subfolder[cran_submissions$subfolder %in% c("", "/")] <- NA
# Remove files or submissions without version number
cran_submissions <- cran_submissions[!is.na(cran_submissions$version), ]
cran_submissions <- distinct(cran_submissions, 
                             snapshot_time, folder, package, version, subfolder,
                             .keep_all = TRUE)


## ----duplicated-packages--------------------------------------------------------------------------
packges_multiple_versions <- cran_submissions %>% 
  group_by(package, snapshot_time) %>% 
  summarize(n = n_distinct(version)) %>% 
  filter(n != 1) %>% 
  distinct(package) %>% 
  pull(package)

## new packages -------
new_packages <- cran_submissions %>% 
  filter(folder == "newbies") %>% 
  pull(package) %>% 
  unique()

## ----package-multiple-folders---------------------------------------------------------------------
package_multiple <- cran_submissions %>% 
  group_by(snapshot_time, package) %>% 
  count() %>% 
  group_by(snapshot_time) %>% 
  count(n) %>% 
  filter(n != 1) %>% 
  summarise(n = sum(nn)) %>% 
  ungroup()


## ----remove-duplicated-packages-version------------------------------------------------------------
cran_submissions <- cran_submissions %>% 
  arrange(package, snapshot_time, version, folder) %>% 
  group_by(package, snapshot_time) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == n()) %>% 
  ungroup() %>% 
  select(-n)


## ----remove-duplicated-packges-folder-------------------------------------------------------------
cran_submissions <- cran_submissions %>% 
  arrange(package, snapshot_time, folder) %>% 
  group_by(package, snapshot_time) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == n()) %>% 
  ungroup() %>% 
  select(-n)


## ----submissions_cleanup--------------------------------------------------------------------------
diff0 <- structure(0, class = "difftime", units = "hours")
cran_submissions <- cran_submissions %>% 
  arrange(package, version, snapshot_time) %>% 
  group_by(package) %>% 
  # Packages last seen in queue less than 24 ago are considered same submission
  mutate(diff_time = difftime(snapshot_time,  lag(snapshot_time), units = "hour"),
         diff_time = if_else(is.na(diff_time), diff0, diff_time), # Fill NAs
         diff_v = version != lag(version),
         diff_v = ifelse(is.na(diff_v), TRUE, diff_v), # Fill NAs
         near_t = near(diff_time, 1, tol = 24),
         resubmission = !near_t | diff_v, 
         resubmission = if_else(resubmission == FALSE & diff_time == 0, 
                               TRUE, resubmission),
         resubmission_n = cumsum(as.numeric(resubmission)),
         new_version = !near_t & diff_v, 
         new_version = if_else(new_version == FALSE & diff_time == 0, 
                               TRUE, new_version),
         submission_n = cumsum(as.numeric(new_version))) %>%
  ungroup() %>% 
  select(-diff_time, -diff_v, -new_version, -resubmission)
saveRDS(cran_submissions, "output/CRAN_data_cleaned.RDS")


ggplot(package_multiple) +
  geom_point(aes(snapshot_time, n), size = 1) +
  geom_rect(data = holidays, aes(xmin = start, xmax = end, ymin = 0, ymax = 6),
            alpha = 0.25, fill = "red") +
  annotate("text", x = holidays$start + (holidays$end - holidays$start)/2, 
           y = 3.5, label = "CRAN holidays") +
  scale_x_datetime(date_labels = "%Y/%m/%d", date_breaks = "2 weeks", 
                   expand = expansion()) +
  scale_y_continuous(expand = expansion()) +
  labs(title = "Packages in multiple folders and subfolders", 
       x = element_blank(), y = element_blank())
ggsave("output/cran_packages_multiple_folders.png")


## ----cran-queues----------------------------------------------------------------------------------
cran_queue <- cran_submissions %>% 
  group_by(snapshot_time) %>% 
  summarize(n = n_distinct(package))
ggplot(cran_queue) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 250),
            alpha = 0.5, fill = "red", data = holidays) +
  annotate("text", x = holidays$start + (holidays$end - holidays$start)/2, 
           y = 150, label = "CRAN holidays") +
  geom_path(aes(snapshot_time, n)) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "2 weeks", 
                   expand = expansion()) +
  scale_y_continuous(expand = expansion()) +
  labs(x = element_blank(), y = element_blank(), 
       title = "Packages on CRAN review process")
ggsave("output/cran_packages_submissions_time.png")


cran_submissions %>% 
  filter(folder == "newbies") %>% 
  distinct(package, .keep_all = TRUE) %>% 
  mutate(month_year = format(snapshot_time, "%Y-%m")) %>% 
  group_by(month_year) %>% 
  summarize(new = sum(package %in% new_packages & submission_n == 1), 
            other = n() - new, total = n()) %>% 
  ggplot() +
  geom_col(aes(month_year, new)) +
  labs(x = "Month", y = "Submissions", 
       col = "Type", shape = "Type",
       title = "CRAN new submissions") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
ggsave("output/cran_new_submissions.png")

## ----cran-submissions-----------------------------------------------------------------------------
man_colors <- RColorBrewer::brewer.pal(8, "Dark2")
names(man_colors) <- unique(cran_submissions$folder)
cran_submissions %>% 
  group_by(folder, snapshot_time) %>% 
  summarize(packages = n_distinct(package)) %>% 
  ggplot() +
  geom_rect(data = holidays, aes(xmin = start, xmax = end, ymin = 0, ymax = 200),
            alpha = 0.25, fill = "red") +
  annotate("text", x = holidays$start + (holidays$end - holidays$start)/2, 
           y = 105, label = "CRAN holidays") +
  geom_path(aes(snapshot_time, packages, col = folder)) +
  scale_x_datetime(date_labels = "%Y/%m/%d", date_breaks = "2 weeks", 
                   expand = expansion()) +
  scale_y_continuous(expand = expansion()) +
  scale_color_manual(values = man_colors) +
  labs(x = element_blank(), y = element_blank(),
       title = "Packages by folder", col = "Folder") +
  theme(legend.position = c(0.6, 0.7))
ggsave("output/cran_packages_folder_time.png")

## ----cran-holidays-zoom---------------------------------------------------------------------------
cran_submissions %>% 
  group_by(folder, snapshot_time) %>% 
  summarize(packages = n_distinct(package)) %>% 
  filter(snapshot_time >= holidays$start) %>% 
  ggplot() +
  geom_path(aes(snapshot_time, packages, col = folder)) +
  geom_rect(data = holidays, aes(xmin = start, xmax = end, ymin = 0, ymax = 200),
            alpha = 0.25, fill = "red") +
  annotate("text", x = holidays$start + (holidays$end - holidays$start)/2, 
           y = 105, label = "CRAN holidays") +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 week", 
                   expand = expansion()) +
  scale_y_continuous(expand = expansion(), limits = c(0, NA)) +
  scale_color_manual(values = man_colors) +
  labs(x = element_blank(), y = element_blank(),
       title = "Holidays", col = "Folder") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.7))
ggsave("output/cran_packages_after_holidays.png")

## ----cran-monthly---------------------------------------------------------------------------------
cran_times <- cran_submissions %>% 
  mutate(seconds = seconds(snapshot_time),
         month = month(snapshot_time),
         mday = mday(snapshot_time),
         wday = wday(snapshot_time, locale = "en_GB.UTF-8"),
         week = week(snapshot_time),
         date = as_date(snapshot_time))
cran_times %>% 
  arrange(folder, date, mday) %>% 
  filter(snapshot_time < holidays$start | snapshot_time  > holidays$end) %>% 
  group_by(folder, date, mday) %>% 
  summarize(packages = n_distinct(package),
            week = unique(week)) %>% 
  group_by(folder, mday) %>% 
  ggplot() +
  geom_smooth(aes(mday, packages, col = folder, linetype = folder)) +
  labs(x = "Day of the month", y = "Packages", col = "Folder", linetype = "Folder",
       title = "Evolution by month day") +
  scale_color_manual(values = man_colors) +
  coord_cartesian(ylim = c(0, NA), xlim = c(1, NA)) +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion()) 
ggsave("output/cran_packages_folder_day_month.png")

## ----cran-wday------------------------------------------------------------------------------------
cran_times %>% 
  filter(snapshot_time < holidays$start | snapshot_time  > holidays$end) %>% 
  group_by(folder, date, wday) %>% 
  summarize(packages = n_distinct(package),
            week = unique(week)) %>% 
  group_by(folder, wday) %>% 
  ggplot() +
  geom_smooth(aes(wday, packages, col = folder, linetype = folder)) +
  labs(x = "Day of the week", y = "Packages", col = "Folder", linetype = "Folder",
       title = "Evolution by week day") +
  scale_color_manual(values = man_colors) +
  scale_x_continuous(breaks = 1:7, expand = expansion()) +
  scale_y_continuous(expand = expansion(), limits = c(0, NA))
ggsave("output/cran_pacakge_week_day.png")

## ----subfolder-pattern----------------------------------------------------------------------------
cran_members <- c("LH", "GS", "JH")
cran_times %>% 
  filter(subfolder %in% cran_members) %>% 
  group_by(subfolder, snapshot_time) %>% 
  summarize(packages = n_distinct(package)) %>% 
  ggplot() +
  geom_smooth(aes(snapshot_time, packages, col = subfolder)) +
    labs(x = element_blank(), y = element_blank(), col = "Folder",
       title = "Packages on folders") +
  scale_y_continuous(expand = expansion(), breaks = 0:10) +
  coord_cartesian(y = c(0, NA))  +
  scale_x_datetime(date_labels = "%Y/%m/%d", date_breaks = "2 weeks", 
               expand = expansion(add = 2)) +
  theme(legend.position = c(0.1, 0.8))


## ----subfolder-mday-------------------------------------------------------------------------------
cran_times %>% 
  filter(subfolder %in% cran_members) %>% 
  group_by(subfolder, mday) %>% 
  summarize(packages = n_distinct(package)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_smooth(aes(mday, packages, col = subfolder)) +
  labs(x = "Day of the month", y = "Pacakges", col = "Subfolder",
       title = "Packages on subfolers by day of the month") +
  scale_y_continuous(expand = expansion()) +
  scale_x_continuous(expand = expansion(), breaks = c(1,7,14,21,29)) +
  coord_cartesian(ylim = c(0, NA))


## ----subfolder-wday, warning=FALSE----------------------------------------------------------------
cran_times %>% 
  filter(subfolder %in% cran_members) %>% 
  group_by(subfolder, wday) %>% 
  summarize(packages = n_distinct(package)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_smooth(aes(wday, packages, col = subfolder)) +
  labs(x = "Day of the week", y = "Pacakges", col = "Subfolder",
       title = "Evolution by week day") +
  scale_y_continuous(expand = expansion()) +
  scale_x_continuous(breaks = 1:7, expand = expansion()) +
  coord_cartesian(ylim =  c(0, NA))


## ----time-submission------------------------------------------------------------------------------
subm <- cran_times %>%
  arrange(snapshot_time) %>% 
  select(package, version, submission_n, snapshot_time) %>% 
  group_by(package, submission_n) %>% 
  filter(row_number() %in% c(1, last(row_number()))) %>% 
  arrange(package, submission_n)


## ----resubm---------------------------------------------------------------------------------------
rsubm <- subm %>% 
  filter(n_distinct(snapshot_time) %% 2 == 0) %>%
  select(-version) %>% 
  mutate(time = c("start", "end")) %>% 
  pivot_wider(values_from = snapshot_time, names_from = time) %>% 
  ungroup() %>% 
  mutate(r = row_number(), 
         time  =  round(difftime(end, start, units = "hour"), 0)) %>% 
  ungroup()
lv <- levels(fct_reorder(rsubm$package, rsubm$start, .fun = min, .desc = FALSE))
ggplot(rsubm) +
  geom_rect(data = holidays, aes(xmin = start, xmax = end), 
            ymin = first(lv), ymax = last(lv), alpha = 0.5, fill = "red") +
  geom_linerange(aes(y = fct_reorder(package, start, .fun = min, .desc = FALSE),
                      x = start, xmin = start, xmax = end, 
                     col = as.factor(submission_n))) + 
  labs(x = element_blank(), y = element_blank(), title = 
         "Packages on the queue", col = "Submissions") +
  scale_x_datetime(date_labels = "%Y/%m/%d", date_breaks = "2 weeks", 
                   expand = expansion(add = 2)) +
  scale_colour_viridis_d() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.15, 0.7))
ggsave("output/cran_submissions_time.png")

## ----rate-----------------------------------------------------------------------------------------
rsubm %>% 
  arrange(start) %>% 
  filter(start < holidays$start, # Look only before the holidays
    submission_n == 1,# Use only the first submission
    start > min(start)) %>%   # Just new submissions
  mutate(r = row_number(),
         start1 = as.numeric(seconds(start))) %>% 
  lm(start1 ~ r, data = .) %>% 
  broom::tidy() %>%  
  mutate(estimate = estimate/(60*60)) # Hours


## ----package-time-queue---------------------------------------------------------------------------
library("patchwork")
p1 <- rsubm %>% 
  group_by(package) %>% 
  summarize(time = sum(time)) %>% 
  ggplot() +
  geom_histogram(aes(time), bins = 100) +
  labs(title = "Packages total time on queue", x = "Hours", 
       y = element_blank()) +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion())
p2 <- rsubm %>% 
  group_by(package) %>% 
  summarize(time = sum(time)) %>% 
  ggplot() +
  geom_histogram(aes(time), binwidth = 24) +
  coord_cartesian(xlim = c(0, 24*7)) +
  labs(subtitle = "Zoom", x = "Hours", y = element_blank()) +
  scale_x_continuous(expand = expansion(), breaks = seq(0, 24*7, by = 24)) +
  scale_y_continuous(expand = expansion()) +
  theme(panel.background = element_rect(colour = "white"))
p1 + inset_element(p2, 0.2, 0.2, 1, 1)
ggsave("output/cran_packages_time_queue.png")

## ----submission-queue-----------------------------------------------------------------------------
p1 <- rsubm %>% 
  group_by(package, submission_n) %>% 
  summarize(time = sum(time)) %>% 
  ggplot() +
  geom_histogram(aes(time), bins = 100) +
  labs(title = "Submission time on queue", x = "Hours", 
       y = element_blank()) +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion())
p2 <- rsubm %>% 
  group_by(package, submission_n) %>% 
  summarize(time = sum(time)) %>%  
  ggplot() +
  geom_histogram(aes(time), binwidth = 24) +
  coord_cartesian(xlim = c(0, 24*7)) +
  labs(subtitle = "Zoom", x = "Hours", y = element_blank()) +
  scale_x_continuous(expand = expansion(), breaks = seq(0, 24*7, by = 24)) +
  scale_y_continuous(expand = expansion()) +
  theme(panel.background = element_rect(colour = "white"))
p1 + inset_element(p2, 0.2, 0.2, 1, 1)
ggsave("output/cran_submission_time.png")



p1 <- rsubm %>% 
  filter(submission_n == 1 & package %in% new_packages) %>% 
  group_by(package,submission_n) %>% 
  summarize(time = median(time)) %>% 
  ggplot() +
  geom_histogram(aes(time), bins = 100) +
  labs(title = "First submission time on queue", x = "Hours", 
       y = element_blank()) +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion())
p2 <- rsubm %>% 
  filter(submission_n == 1 & package %in% new_packages) %>% 
  group_by(package, submission_n) %>% 
  summarize(time = sum(time)) %>%  
  ggplot() +
  geom_histogram(aes(time), binwidth = 24) +
  coord_cartesian(xlim = c(0, 24*7)) +
  labs(subtitle = "Zoom", x = "Hours", y = element_blank()) +
  scale_x_continuous(expand = expansion(), breaks = seq(0, 24*7, by = 24)) +
  scale_y_continuous(expand = expansion()) +
  theme(panel.background = element_rect(colour = "white"))
p1 + inset_element(p2, 0.2, 0.2, 1, 1)
ggsave("output/cran_submission_time_first_submission.png")


rsubm %>% 
  group_by(submission_n) %>% 
  summarize(time = median(time)) %>% 
  ungroup() %>% 
  mutate(submission_n =  forcats::fct_relevel(as.character(submission_n), 
                                              as.character(1:10))) %>% 
  ggplot() +
  geom_col(aes(submission_n, time)) +
  labs(title = "Submission time on queue", x = "Submissions", 
       y = "Hours") +
  scale_y_continuous(expand = expansion())
ggsave("output/cran_submission_time_each_n.png")

## ----resubm2--------------------------------------------------------------------------------------
subm2 <- cran_times %>%
  group_by(package, submission_n, folder) %>% 
  arrange(snapshot_time) %>% 
  select(package, version, submission_n, snapshot_time, folder) %>% 
  filter(row_number() %in% c(1, last(row_number()))) %>% 
  arrange(submission_n)
rsubm2 <- subm2 %>% 
  filter(n_distinct(snapshot_time) %% 2 == 0) %>%
  mutate(time = c("start", "end")) %>% 
  pivot_wider(values_from = snapshot_time, names_from = time) %>% 
  ungroup() %>% 
  mutate(r = row_number(), 
         time  =  round(difftime(end, start, units = "hour"), 0)) %>% 
  ungroup() %>% 
  filter(!is.na(start), !is.na(end))
lv <- levels(fct_reorder(rsubm2$package, rsubm2$start, .fun = min, .desc = FALSE))
ggplot(rsubm2) +
  geom_rect(data = holidays, aes(xmin = start, xmax = end), 
            ymin = first(lv), ymax = last(lv), alpha = 0.5, fill = "red") +
  geom_linerange(aes(y = fct_reorder(package, start, .fun = min, .desc = FALSE),
                      x = start, xmin = start, xmax = end, col = folder)) + 
  labs(x = element_blank(), y = element_blank(), title = 
         "Packages on the queue") +
  scale_color_manual(values = man_colors) +
  scale_x_datetime(date_labels = "%Y/%m/%d", date_breaks = "2 weeks", 
               expand = expansion(add = 2)) +
  labs(col = "Folder") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2, 0.7))
ggsave("output/cran_submision_time_type.png")

## ----submissions-n-folders------------------------------------------------------------------------
rsubm2 %>% 
  group_by(package, submission_n) %>% 
  summarize(n_folder = n_distinct(folder)) %>% 
  ggplot() + 
  geom_histogram(aes(n_folder), bins = 5) +
  labs(title = "Folders by submission", x = element_blank(), 
       y = element_blank())
ggsave("output/cran_packages_folders.png")

## ----submissions-folders--------------------------------------------------------------------------
compact_folders <- function(x) {
  y <- x != lag(x)
  y[1] <- TRUE
  x[y]
}
cran_times %>% 
  group_by(package, submission_n) %>% 
  summarize (folder = list(compact_folders(folder))) %>% 
  ungroup() %>% 
  count(folder, sort = TRUE) %>% 
  top_n(5) %>% 
  rename(Folders = folder, Frequency = n) %>% 
  as.data.frame()


## ----cran-pressure--------------------------------------------------------------------------------
subm3 <- cran_times %>%
  arrange(snapshot_time) %>% 
  group_by(package) %>% 
  mutate(autor_change = submission_n != lag(submission_n),
         cran_change = folder != lag(folder)) %>% 
  mutate(autor_change = ifelse(is.na(autor_change), TRUE, autor_change),
         cran_change = ifelse(is.na(cran_change), FALSE, cran_change)) %>% 
  mutate(cran_change = case_when(subfolder != lag(subfolder) ~ TRUE,
                                 TRUE ~ cran_change)) %>% 
  ungroup()
subm3 %>% 
  group_by(snapshot_time) %>% 
  summarize(autor_change = sum(autor_change), cran_change = sum(cran_change)) %>% 
  filter(row_number() != 1) %>% 
  filter(autor_change != 0 | cran_change != 0) %>% 
  ggplot() +
  geom_rect(data = holidays, aes(xmin = start, xmax = end), 
            ymin = -26, ymax = 26, alpha = 0.5, fill = "grey") +
  geom_point(aes(snapshot_time, autor_change), fill = "blue", size = 0) +
  geom_area(aes(snapshot_time, autor_change), fill = "blue") +
  geom_point(aes(snapshot_time, -cran_change), fill = "red", size = 0) +
  geom_area(aes(snapshot_time, -cran_change), fill = "red") +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "2 weeks", 
                   expand = expansion(add = 2)) +
  scale_y_continuous(expand = expansion(add = c(0, 0))) + 
  coord_cartesian(ylim = c(-26, 26)) +
  annotate("text", label = "CRAN's", y = 20, x = as_datetime("2020/11/02")) +
  annotate("text", label = "Maintainers'", y = -20, x = as_datetime("2020/11/02")) +
  labs(y = "Changes", x = element_blank(), title = "Activity on CRAN:")
ggsave("output/cran_work.png")

## ----review-process-------------------------------------------------------------------------------
cran_times %>% 
  ungroup() %>% 
  group_by(package, submission_n) %>% 
  arrange(snapshot_time) %>% 
  filter(1:n() == last(n())) %>% 
  ungroup() %>% 
  count(folder, sort = TRUE) %>% 
  knitr::kable(col.names = c("Last folder", "Submissions"))


## ----cran-public----------------------------------------------------------------------------------
package_submissions <- cran_times %>% 
  group_by(package, submission_n) %>% 
  summarise(submission_period = difftime(max(snapshot_time), 
                                         min(snapshot_time), 
                                         units = "hour"),
            submission_time = min(snapshot_time)) %>% 
  ungroup() %>% 
  filter(submission_period != 0)


## ----time-time------------------------------------------------------------------------------------
package_submissions %>% 
  # filter(submission_time < holidays$start) %>% 
  ggplot() +
  geom_point(aes(submission_time, submission_period, col = submission_n)) +
  geom_rect(data = holidays, aes(xmin = start, xmax = end),
            ymin = 0, ymax = 5000, alpha = 0.5, fill = "red") + 
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "2 weeks",
                   expand = expansion(add = 10)) +
  scale_y_continuous(expand = expansion(add = 10)) +
  labs(title = "Time on the queue according to the submission",
       x = "Submission", y = "Time (hours)", col = "Submission") +
  theme(legend.position = c(0.7, 0.8))
ggsave("output/cran_time_queue.png")

## ----daily-submission-----------------------------------------------------------------------------
package_submissions %>% 
  filter(submission_n == 1) %>% 
  ungroup() %>%
  mutate(d = as.Date(submission_time)) %>%
  group_by(d) %>% 
  summarize(m = median(submission_period)) %>% 
  ggplot() +
  geom_rect(data = holidays, aes(xmin = as.Date(start), xmax = as.Date(end)),
            ymin = 0, ymax = 120, alpha = 0.5, fill = "red") + 
  geom_smooth(aes(d, m), span = 0.2) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "2 weeks",
                   expand = expansion(add = 1)) +
  scale_y_continuous(expand = expansion()) +
  labs(x = element_blank(), y = "Daily median time in queue (hours)", 
       title = "Submission time")
ggsave("output/cran_time_submission.png")

## ----submission-progression-----------------------------------------------------------------------
package_submissions %>% 
  group_by(submission_n) %>% 
  mutate(submission_n = forcats::fct_relevel(as.character(submission_n), 
                                             as.character(1:10))) %>% 
  ggplot() +
  geom_jitter(aes(submission_n, submission_period), height = 0) +
  scale_y_continuous(limits = c(1, NA), expand = expansion(add = c(1, 10)),
                     breaks = seq(0,  5500, by = 24*7)) +
  labs(title = "Submission time in queue", y = "Hours", x = element_blank())
ggsave("output/cran_time_queue.png")

## ----submission_median_time-----------------------------------------------------------------------
package_submissions %>% 
  filter(submission_period != 0) %>% 
  group_by(submission_n) %>% 
  mutate(submission_n = as.character(submission_n)) %>% 
  filter(n() > 5) %>% 
  summarize(median = round(median(submission_period), 2)) %>% 
  knitr::kable(col.names = c("Submission", "Median time (h)"))
