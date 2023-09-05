library(tidyverse)
library(daymetr)

# this was a first stab at pulling in Daymet data and cleaning it up. everything here is now in nicer functions in example_in_R/add_daymet_cdd_functions.R





# modifying the daymetr function to accept only a few vars
download_daymet_v <- function (site = "Daymet", lat = 36.0133, lon = -84.2625, start = 2000, end = as.numeric(format(Sys.time(), "%Y")) - 2, vars = "tmax,tmin,dayl,prcp,srad,swe,vp", path = tempdir(), internal = TRUE, silent = FALSE, force = FALSE, simplify = FALSE) 
{
  if (!silent & !internal & identical(path, tempdir())) {
    message("NOTE: by default data is stored in tempdir() ...")
  }
  url <- daymetr:::server()
  if (!force) {
    max_year <- as.numeric(format(Sys.time(), "%Y")) - 1
  }
  else {
    max_year <- as.numeric(format(Sys.time(), "%Y"))
  }
  if (start < 1980) {
    stop("Start year preceeds valid data range!")
  }
  if (end > max_year) {
    stop("End year exceeds valid data range!")
  }
  year_range <- paste(seq(start, end, by = 1), collapse = ",")
  query <- list(lat = lat, lon = lon, vars = vars, 
                year = year_range)
  daymet_file <- file.path(normalizePath(path), sprintf("%s_%s_%s.csv", 
                                                        site, start, end))
  daymet_tmp_file <- file.path(normalizePath(tempdir()), sprintf("%s_%s_%s.csv", 
                                                                 site, start, end))
  if (!silent) {
    message(paste("Downloading DAYMET data for: ", site, 
                  " at ", lat, "/", lon, " latitude/longitude !\n", 
                  sep = ""))
  }
  error <- httr::GET(url = url, query = query, httr::write_disk(path = daymet_tmp_file, 
                                                                overwrite = TRUE))
  if (httr::status_code(error) == 400) {
    file.remove(daymet_tmp_file)
    stop("Your requested data is outside DAYMET spatial coverage.\n\n            Check the requested coordinates.")
  }
  if (httr::status_code(error) > 400) {
    file.remove(daymet_tmp_file)
    stop("The server is unreachable, check your connection.")
  }
  if (!silent) {
    message("Done !\n")
  }
  if (internal) {
    tmp_struct <- read_daymet(daymet_tmp_file, site = site, 
                              simplify = simplify)
    return(tmp_struct)
  }
  else {
    if (!identical(daymet_tmp_file, daymet_file)) {
      file.copy(daymet_tmp_file, daymet_file, overwrite = TRUE, 
                copy.mode = FALSE)
      invisible(file.remove(daymet_tmp_file))
    }
    else {
      message("Output path == tempdir(), file not copied or removed!")
    }
  }
}

a <- read_csv("example_in_R/data/raw/Aphids.csv")

# these are the distinct site-dates we need to pull
ad <- a %>% 
  distinct(SiteID, Latitude, Longitude, Year)

# for each distinct site-date, download the Daymet data and store it in a list-column
ad <- ad %>% 
  mutate(data = pmap(., .f = function(SiteID, Latitude, Longitude, Year){
    download_daymet_v(site = SiteID, lat = Latitude, lon = Longitude,
                    start = Year, end = Year, internal = T, vars = "tmax,tmin") %>% 
      .$data %>% 
      select(yday, starts_with("t"))
  }))

# set CDD bounds
upperbound <- 82.4
lowerbound <- 41.9

# make avg temp in F
# get temps within bounds
# generate dd and cdd columns
acdd <- ad %>% 
  unnest(data) %>% 
  mutate(tavg = ((tmax..deg.c. + tmin..deg.c.)/2) * (9/5) + 32) %>% 
  select(-tmax..deg.c., -tmin..deg.c., Julian_Day = yday) %>% 
  filter(tavg >= lowerbound, tavg <= upperbound) %>% 
  mutate(dd = tavg - lowerbound) %>% 
  group_by(SiteID, Year) %>% 
  mutate(cdd = cumsum(dd)) %>% 
  select(-tavg, -dd)

# join cdd data to original observations and write to CSV
a %>% 
  left_join(acdd) %>% 
  write_csv("example_in_R/data/cleaned/aphids_cdd.csv")
