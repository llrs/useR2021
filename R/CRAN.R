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

l <- lapply(unique(cran_submissions$package),
            function(x){
              y <- tryCatch(pkgsearch::cran_package_history(x), 
                            error = function(e){FALSE})
              if (!isFALSE(y)) {
                z <- y$`Date/Publication`
                a <- z[!is.na(z)]
                return(as_datetime(z))
              } else {
                return(NA)
              }
            })
names(l) <- unique(cran_submissions$package)
saveRDS(l, "output/CRAN_archival_dates.RDS")
