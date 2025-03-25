# This module requires some setup and third-party API setup.
# In the body of this repo you will find a file called .env-example
# Copy that into .env, and fill out the requisite information
# You can request a US Census API key here: https://api.census.gov/data/key_signup.html
# You can request an IPUMS API key following this guide: https://developer.ipums.org/docs/v2/get-started/
# The IPUMS API key will then need to be authorized to access NHGIS data here: https://uma.pop.umn.edu/nhgis/registration
# IMPORTANT: DO NOT SHARE .env WITH ANYONE AND DO NOT COMMIT IT BACK TO GITHUB
# These API keys act as your logins for these services.

list.of.packages <- c(
  "openxlsx", "data.table",
  "RSocrata", "jsonlite",
  "httr", "rvest",
  "censusapi", "tidycensus",
  "dplyr", "tidyverse",
  "dotenv",
  "remotes","rJava"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
# Watch the output of this lapply. If you see any that return `FALSE` that means
# At least one package failed to install. It will most likely be rJava, which requires
# you install and set JAVA_HOME. See the commented section below for how I do this:

# Sys.setenv(JAVA_HOME="C:/Java/jdk-23.0.2/")
# install.packages("rJava")

# This package can only be installed through Github now.
# Comment out this remotes line once you have it installed.
# If you cannot get it working, skip the section on PDF scraping.
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
library(tabulapdf)


getCurrentFileLocation =  function()
{
  this_file = commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file = rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())
setwd("..")

# First, we're going to extract tables from a PDF

# If you look in the "data" folder in this repo, you can find "finregannrep2024.pdf"
# Which is the latest annual report from OFR.

pdf_name = "data/finregannrep2024.pdf"

# Tabulizer can try and guess where the data you want to extract is by default
# If your data is a bit messy, you will need to use the `locate_areas` function first
# To find the boundaries of the columns.
tabulapdf::locate_areas(pdf_name, pages=51)

# First run the command to get the whole area you want to extract
# And then run it a few more times to get the right-edge of each column
# Once you have those values, we can plug them into the extract_tables function below

pdf_tables = tabulapdf::extract_tables(
  pdf_name,
  pages=51,
  area=list(c(98, 32, 632, 574)),
  guess=F,
  columns = list(c(204, 270, 334, 403, 460, 519, 574))
)
asset_dat_2024 = pdf_tables[[1]] # The result is a list (in case you're doing multiple pages). We just need the first index.

# Take a look at the result. Even specifying the area it's a little messy
# We need to cut out the header and fix a few things
asset_dat_2024 = asset_dat_2024[7:nrow(asset_dat_2024),] # Cut out header
names(asset_dat_2024) = c("Bank", "Location", "Assets", "Deposits", "Branches", "LMI_Branches", "CRA") # Add our own
asset_dat_2024$Bank[5] = "Calvin B. Taylor Banking Company of Berlin, Maryland" # Fix a bank name that's over two lines
asset_dat_2024 = asset_dat_2024[c(1:3, 5, 7:nrow(asset_dat_2024)),] # Remove the rows from the incorrect bank name

# These are useful patterns for data cleaning.
# gsub removes certain text from strings, in this case we're removing the comma and dollar sign
# from financial values before casting them to a numeric type
asset_dat_2024$Assets = as.numeric(gsub(",","",gsub("$","",asset_dat_2024$Assets, fixed=T)))
asset_dat_2024$Deposits = as.numeric(gsub(",","",gsub("$","",asset_dat_2024$Deposits, fixed=T)))
asset_dat_2024$Branches = as.numeric(asset_dat_2024$Branches)
asset_dat_2024$LMI_Branches = as.numeric(gsub(" \\(.*?\\)", "", asset_dat_2024$LMI_Branches, perl=T))
asset_dat_2024$Year = 2024
asset_dat_2024$Type = "State-chartered bank"

# Take a look at our final product
View(asset_dat_2024)


# Our next useful skill is web scraping, or gathering data from web pages.
# Some data access is very straightforward, especially if the provider
# has a documented API. One such documented API is the FDIC. You can
# read the full documentation here: https://banks.data.fdic.gov/docs/

# You just define the parameters from the API docs
fdic_json_payload = list(
  "limit" = 10000,
  "offset" = 0,
  "filters" = "YEAR:\"2024\" AND STALPBR:\"MD\"",
  "search" = "",
  "agg_by" = "",
  "agg_term_fields" = "",
  "agg_sum_fields" = "",
  "agg_limit" = "",
  "sort_by" = "BRNUM",
  "sort_order" = "ASC"
)

# And send them to the server with the appropriate request type
fdic_response = POST(
  "https://pfabankapi.app.cloud.gov/api/sod",
  body=fdic_json_payload,
  encode="json"
)

# Check the HTTP status code of the response
status_code(fdic_response)
# 200 is a successful response.
# 400 range means you've made a mistake
# 500 range means they've made a mistake

# Here I read the content from the response
fdic_response_content = content(fdic_response, as="text")

# Flatten out the hierarchical data structure
fdic_response_json = fromJSON(fdic_response_content, flatten=T)

# And pull it into a data frame
fdic_dat = fdic_response_json$data
fdic_dat$score = NULL

# View your results
View(fdic_dat)


# If you're extracting from an HTML table, you might need to imitate the web request
# that created the table in the first place. Below is an example extraction from
# the US DOL UI quality table

# This imitates the initial request my browser made to load the page
headers = add_headers(
  'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
  'Accept-Language' = 'en-US,en;q=0.9',
  'Cache-Control' = 'no-cache',
  'Connection' = 'keep-alive',
  'Content-Type' = 'application/x-www-form-urlencoded',
  'Cookie' = 'cookieOUI=cookieOUIValue; mycookie=cookie_value',
  'Origin' = 'https://oui.doleta.gov',
  'Pragma' = 'no-cache',
  'Referer' = 'https://oui.doleta.gov/unemploy/ranking.asp',
  'Sec-Fetch-Dest' = 'document',
  'Sec-Fetch-Mode' = 'navigate',
  'Sec-Fetch-Site' = 'same-origin',
  'Sec-Fetch-User' = '?1',
  'Upgrade-Insecure-Requests' = '1',
  'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36',
  'sec-ch-ua' = '"Google Chrome";v="131", "Chromium";v="131", "Not_A Brand";v="24"',
  'sec-ch-ua-mobile' = '?0',
  'sec-ch-ua-platform' = '"Linux"'
)

# Define some parameters so we can scrape other pages later
start_quarter = "01/01"
end_quarter = "12/31"
year = 2023

# Make an intial POST request to the server and retrieve the HTML
res <- POST("https://oui.doleta.gov/unemploy/ranking/rankingrpt.asp",
  headers,
  body = list(
    "category[]" = 4, # Nonmonetary Separation Quality
    "category[]" = 5, # Nonmonetary Nonseparation Quality
    "strtqtr" = start_quarter, 
    "strtyear" = year, 
    "endqtr" = end_quarter,
    "endyear" = year,
    "submit" = "Submit"
  ),
  encode = "form"
)
res_text = content(res, as="text")
res_html = read_html(res_text)
# This is the important bit that extracts the tables as a data frame
html_tables = html_table(res_html)
sep_quality_table = html_tables[[1]]
names(sep_quality_table) = c("state", "nonmonetary_separation_quality_percent")
sep_quality_table = sep_quality_table[2:nrow(sep_quality_table),]
nonsep_quality_table = html_tables[[2]]
names(nonsep_quality_table) = c("state", "nonmonetary_nonseparation_quality_percent")
nonsep_quality_table = nonsep_quality_table[2:nrow(nonsep_quality_table),]
quality_dat = merge(sep_quality_table, nonsep_quality_table, all=T)
quality_dat$year = year
quality_dat$start_quarter = start_quarter
quality_dat$end_quarter = end_quarter

# Clean up data flags
quality_dat$nonmonetary_separation_quality_percent = as.numeric(
  gsub(
    "*",
    "",
    gsub("^","",quality_dat$nonmonetary_separation_quality_percent, fixed=T),
    fixed=T
  )
)
quality_dat$nonmonetary_nonseparation_quality_percent = as.numeric(
  gsub(
    "*",
    "",
    gsub("^","",quality_dat$nonmonetary_nonseparation_quality_percent, fixed=T),
    fixed=T
  )
)

# Clean up dates
quality_dat$start_date = as.Date(
  paste(quality_dat$start_quarter, quality_dat$year, sep="/"), format="%m/%d/%Y"
)
quality_dat$end_date = as.Date(
  paste(quality_dat$end_quarter, quality_dat$year, sep="/"), format="%m/%d/%Y"
)

# Take a look at our final product
View(quality_dat)

# Lastly, there are some special APIs that have built in R packages
# One such one is the US Census. Make sure you've received and filled out
# your Census API key in the .env file before running these lines
# Also be careful with any .Rhistory files after this point, they may leak
# your API key

# We load the .env file into the environment
load_dot_env()
# Fetch the key from the environment
api_key = Sys.getenv("CENSUS_API_KEY")
# And then authenticate our census requests with it
census_api_key(api_key)

# You can check what variables are available from the ACS with this command:
acs_variables = load_variables(2023, "acs5")
View(acs_variables)

# Load and rename some ACS5 data for Maryland and 5 surrounding states
acs_dat = get_acs(
  geography="tract",
  variables = c(total_population="B01003_001", self_employed_incorp="C24070_029", self_employed_unincorp="C24070_071"),
  state = c("DE", "DC", "MD", "PA", "VA", "WV"),
  year = 2023
)
View(acs_dat)

# If you're working with US Census tracts across the 2010-2020, I've developed a crosswalk
# function that utilizes the IPUMS NHGIS crosswalk files. Make sure your IPUMS key
# is filled out in .env and it's authorized to use NHGIS data
# Please note this also only works with counts or sums. It's not correct to use it to
# to aggregate medians
source("code/util_nhgis_census_crosswalk.R")

acs_2019 = get_acs(
  geography="tract",
  variables = c(total_population="B01003_001", self_employed_incorp="C24070_029", self_employed_unincorp="C24070_071"),
  state = c("DE", "DC", "MD", "PA", "VA", "WV"),
  year = 2019
)
acs_2019$year = 2019
acs_2019 = dcast(data.table(acs_2019), GEOID+year~variable, value.var="estimate")

acs_2020 = get_acs(
  geography="tract",
  variables = c(total_population="B01003_001", self_employed_incorp="C24070_029", self_employed_unincorp="C24070_071"),
  state = c("DE", "DC", "MD", "PA", "VA", "WV"),
  year = 2020
)
acs_2020$year = 2020
acs_2020 = dcast(data.table(acs_2020), GEOID+year~variable, value.var="estimate")


acs_2019_crosswalked = acs_2019 %>% nhgis_crosswalk(
  start_year=2010,
  end_year=2020,
  GEOID_col="GEOID",
  value_cols=c("total_population", "self_employed_incorp", "self_employed_unincorp"),
  states = c(
    "10", # DE
    "11", # DC
    "24", # MD
    "42", # PA
    "51", # VA
    "54" # WV
  )
)
acs_2019_crosswalked$GEOID = acs_2019_crosswalked$end_GEOID
acs_2019_crosswalked = subset(acs_2019_crosswalked, !is.na(GEOID))
acs_2019_crosswalked = acs_2019_crosswalked[,.(
  total_population=sum(crosswalk_total_population, na.rm=T),
  self_employed_incorp=sum(crosswalk_self_employed_incorp, na.rm=T),
  self_employed_unincorp=sum(crosswalk_self_employed_unincorp, na.rm=T)
), by=.(GEOID, year)]

acs_2019_2020 = rbindlist(list(acs_2019_crosswalked, acs_2020), fill=T)
acs_2019_2020_long = melt(acs_2019_2020, id.vars=c("GEOID", "year"))
acs_wide = dcast(acs_2019_2020_long, GEOID~variable+year)
