library(jsonlite)

source("./R/get_country_page.R")

app_id <- "app_id"
app_key <- "app_key"
keyword <- "hgv driver"
max_age <- 1
page <- 1

dat <- get_country_page(keyword = "hgv driver",app_id = app_id, app_key = app_key, n_results = 9000, max_age = 1)
saveRDS(dat, paste0("./data/raw/",format(Sys.time(), "%Y-%m-%d-%H%M%S-"),"sample.rds"))
