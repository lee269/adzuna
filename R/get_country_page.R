get_country_page <- function(
    keyword,
    country = "gb",
    app_id, app_key,
    n_results = 50,
    max_age = 365
) {
  
  total_runs <- 1
  if(n_results > 51) total_runs <- ceiling(n_results / 50)
  
  makeURL <- function(page = 1, ...) {
    this_url <<- paste0("http://api.adzuna.com:80/v1/api/jobs/",
                        country,
                        "/search/",
                        page, "?",
                        "app_id=", app_id,
                        "&app_key=", app_key,
                        "&results_per_page=50",
                        "&what=", sub(" ", "%20", keyword),
                        "&max_days_old=", max_age)
  }
  
  cat("\ndowloading...")
  makeURL()
  dat <- jsonlite::fromJSON(this_url)
  
  n <- dat$count
  if(n < n_results) total_runs <- ceiling(n / 50)
  
  results <- list(dat$results)
  if(total_runs > 1) {
    for(i in 2:total_runs) {
      cat("\n  page ", i)
      makeURL(page = i)
      results[[i]] <- jsonlite::fromJSON(this_url)$results
    }
  }
  
  if(n < n_results){ cat("\n    your search returned ", n, " results\n") }else{ cat("\n    your search returned", n_results, "results\n") }
  
  return(jsonlite::rbind_pages(results))
  
}
