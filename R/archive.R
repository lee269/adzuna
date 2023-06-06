rm(list=ls())
#Packages
library(tidyverse)
library(readr)
library(here)
# only needed for the pipe

source(here("R", "functions.R"))

#-----------------------------------------------------------------------
##Create a backup of the archive already there in case changes need to be made to the datasets and the archive update needs to be rerun
archive_backup <- read_rds(here("data", "archive.rds"))

now <- Sys.time() %>%
  gsub(":","",.) %>%
  gsub(" ","-",.)

write_rds(archive_backup,here("data", paste0("archive-",now,".rds")))

##Pipeline for updating the archive and generating the summary tables
archive <- create_updated_archive(archive_backup)

write_rds(archive,here("data", "archive.rds"))