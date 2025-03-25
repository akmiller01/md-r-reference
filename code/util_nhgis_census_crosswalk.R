list.of.packages = c(
  "data.table", "dotenv", "dplyr" 
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(lapply(list.of.packages, require, character.only=T))

# Data source: https://www.nhgis.org/geographic-crosswalks
# Target-Density Weighting Interpolation and Uncertainty Evaluation for Temporal Analysis of Census Data
# https://onlinelibrary.wiley.com/doi/10.1111/j.1538-4632.2007.00706.x

load_dot_env()
ipums_key = Sys.getenv("IPUMS_KEY")

download_nhgis_crosswalk = function(start_year, end_year, states=c("24")){
  for(state in states){
    filename = paste0(
      "nhgis_blk",
      start_year,
      "_blk",
      end_year,
      "_",
      state,
      ".zip"
    )
    url = 
      paste0(
        "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk",
        start_year,
        "_blk",
        end_year,
        "_state/",
        filename
      )
    dir.create("nhgis_crosswalks", showWarnings=F)
    file_location = paste0("nhgis_crosswalks/", filename)
    if(!file.exists(file_location)){
      download.file(url, file_location, headers = c(Authorization = ipums_key))
    }
    unzip(file_location, exdir = "nhgis_crosswalks")
  }
}


nhgis_crosswalk = function(
    .data,
    start_year,
    end_year,
    GEOID_col,
    value_cols,
    geography="tr",
    states=c("24")
){
  download_nhgis_crosswalk(start_year, end_year, states)
  crosswalk_list = list()
  crosswalk_index = 1
  for(state in states){
    filename = paste0(
      "nhgis_crosswalks/nhgis_blk",
      start_year,
      "_blk",
      end_year,
      "_",
      state,
      ".csv"
    )
    
    crosswalk_tmp = fread(filename)
    crosswalk_list[[crosswalk_index]] = crosswalk_tmp
    crosswalk_index = crosswalk_index + 1
  }
  crosswalk_dat = rbindlist(crosswalk_list)
  
  names(crosswalk_dat) = c(
    "start_NHGIS",
    "start_GEOID",
    "end_NHGIS",
    "end_GEOID",
    "parea",
    "weight"
  )
  crosswalk_dat[,c("start_NHGIS", "end_NHGIS", "parea")] = NULL
  
  if(geography=="tr"){
    crosswalk_dat$start_GEOID = substr(
      crosswalk_dat$start_GEOID, 1, 11
    )
    crosswalk_dat$end_GEOID = substr(
      crosswalk_dat$end_GEOID, 1, 11
    )
    
    crosswalk_dat[,start_sum_weight:=sum(weight), by=.(start_GEOID)]
    crosswalk_dat = crosswalk_dat[,.(weight=sum(weight/start_sum_weight)), by=.(start_GEOID, end_GEOID)]
    crosswalk_dat$weight[which(is.nan(crosswalk_dat$weight))] = 0
    sanity_check = crosswalk_dat[,.(weight=sum(weight)), by=.(start_GEOID)]
    stopifnot({
      all(round(sanity_check$weight, digits = 6) %in%  c(0,1))
    })
  }
  if(is.data.table(.data)){
    .data[,GEOID_col] = as.character(.data[,GEOID_col,with=F][[1]])
  }else{
    .data[,GEOID_col] = as.character(.data[,GEOID_col])
  }
  
  crosswalk_dat$start_GEOID = as.character(crosswalk_dat$start_GEOID)
  setnames(crosswalk_dat, "weight", "crosswalk_weight")
  
  .data = merge(.data, crosswalk_dat, by.x=GEOID_col, by.y="start_GEOID", all.x=T, allow.cartesian=T)
  crosswalk_value_cols = paste("crosswalk",value_cols,sep="_")
  if(is.data.table(.data)){
    .data[,crosswalk_value_cols] = .data$crosswalk_weight * .data[,value_cols,with=F]
  }else{
    .data[,crosswalk_value_cols] = .data$crosswalk_weight * .data[,value_cols]
  }

  return(.data)
}


