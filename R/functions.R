path_to_raw_data <- "./data/raw/"


# generate a table of the contents of the raw data dir
# tibble: filename, full path filename, datetime as text and posix
# A key input to most functions
index_raw_data <- function(){
  x <- dplyr::tibble(filename = list.files("./data/raw/")) %>%
    dplyr::mutate(filepath = paste0("./data/raw/", filename)) %>%
    dplyr::mutate(date_time = stringr::str_remove(filename, "-?sample.rds"),
                  datetime = lubridate::ymd_hms(date_time, truncated = 3))
  
  return(x)
}

# tidy up a raw data frame
# applies the standard cleaning to a raw df as downloaded with the API
tidy_raw_data <- function(df){
  message("Cleaning raw data")
  tidy <- df %>% 
    dplyr::mutate(company_name = company$display_name,
                  area = location$area,
                  category = category$label) %>% 
    dplyr::select(-company, -location) %>% 
    tidyr::separate(area, into = c("country", "region", "area", "town"), sep = ",") %>% 
    dplyr::mutate(created = lubridate::ymd_hms(created),
                  date = lubridate::as_date(created)) %>% 
    dplyr::select(-c(`__CLASS__`, adref, redirect_url)) %>% 
    unique() %>% 
    dplyr::mutate(country = stringr::str_remove_all(country, "c\\(\""),
                  region = stringr::str_remove_all(region, "\""),
                  area = stringr::str_remove_all(area, "\""),
                  town = stringr::str_remove_all(town, "\""),
                  country = stringr::str_remove_all(country, "\""),
                  region = stringr::str_remove_all(region, "\\)"))
  
  return(tidy)
}

# read in a list of raw data files sourced from the API 
# input is full path filename via index_raw_data
read_raw_data <- function(raw_data = index_raw_data()){
  message(paste("Reading", length(raw_data$filename), "files"))
  purrr::map_dfr(raw_data$filepath, readRDS)
  
}

# read and tidy a set of files
# raw data via read_raw_data, tidied via tidy_raw_data
tidy_data <- function(raw_data = index_raw_data()){
  read_raw_data(raw_data) %>% 
    tidy_raw_data()
  
}


# not sure this is needed
process_all_raw_data <- function(raw_data = index_raw_data()){
  unique(raw_data$date_time) %>% 
    purrr::map_df(~tidy_raw_data(.x))
}


# build an archive from the complete contents of a folder
# archive structure is:
# cleaned adzuna data
# metadata df with a 'new archive generated' message
# vector of file names included in the data
generate_new_archive <- function(raw_data = index_raw_data()){
  data <- tidy_data()
  
  archive <- list(
    data = data,
    metadata =
      dplyr::tibble(
        user = Sys.info()["user"],
        event = paste("Archive generated with ", length(raw_data$date_time), "files"),
        time = Sys.time()
      ),
    file_list = c(raw_data$filename)
  )
  
}


# produce a new archive object incorporating new data input param is an existing
# archive object.
#
# compares data directory listing with archive file list identifies files not
# currently in the archive, runs tidy_data on those and binds the two together.
# Also updates the archve metadata and file list components
create_updated_archive <- function(archive){
  
  raw_data_index = index_raw_data()
  
  # internal function  
  detect_new_files <-
    function(path_to_raw_data,
             files_in_archive,
             raw_data_index=index_raw_data()) {
      
      `%not_in%` <- function(x, y) {
        !('%in%'(x, y))
      }
      
      all_files <- list.files(path_to_raw_data)
      
      files_not_in_archive <-
        all_files [all_files %not_in% files_in_archive]
      
      n_new <- length(files_not_in_archive)
      if (n_new == 0) {
        message(paste0("There are 0 new files to add to the archive."))
      }
      if (n_new > 0) {
        message(paste("There are", n_new, "new files to add to the archive."))
      }
      return(files_not_in_archive)
      
    }
  # end of internal function
  
  new_file_names <-
    detect_new_files(
      path_to_raw_data = path_to_raw_data,
      files_in_archive = archive$file_list,
      raw_data_index
    )  
  
  if (length(new_file_names) > 0) {
    new_file_paths <- paste0(path_to_raw_data, new_file_names)
    
    
    index_of_new_files <- filter(raw_data_index,
                                 filepath %in% new_file_paths)
    
    new_data <- tidy_data(index_of_new_files)
    
    message("Generating new archive object")
    
    archive <- list(
      data = bind_rows(archive$data,
                       new_data) %>%
        unique(),
      
      metadata = bind_rows(
        archive$metadata,
        dplyr::tibble(
          user = Sys.info()["user"],
          event = paste(length(new_file_names), "new files added"),
          time = Sys.time()
        )),
      
      file_list = unique(c(archive$file_list, new_file_names))
      
    )
    
  }
  
  return(archive)
  
}