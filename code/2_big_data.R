list.of.packages <- c(
  "data.table", "tidyverse", "tibble", "tidyr", "dplyr", "rstudioapi", "R.utils", "ggplot2"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

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

# We hardly work with what most experts would consider big data
# Three Vs (volume, velocity, and variety)
# But R can be useful for data too big to fit into Excel (> ~1 million rows)

# First we need to download a lot of data! This gzip is the job-to-job origin-destination dataset from the US Census
options(timeout=60000)
if(!file.exists("output/j2jod_md_all.csv.gz")){
  download.file(
    "https://lehd.ces.census.gov/data/j2j/latest_release/md/j2jod/j2jod_md_all.csv.gz",
    destfile="output/j2jod_md_all.csv.gz"
  )
}

# Unzip the gzip
if(!file.exists("output/j2jod_md_all.csv")){
  R.utils::gunzip(
    "output/j2jod_md_all.csv.gz",
    remove=F
  )
}

# Use the data.table fread package to "fast read" the large CSV
j2jod_md = fread("output/j2jod_md_all.csv")

message("Total rows: ",format(nrow(j2jod_md), big.mark = ","))
# Save it all as a compressed RData file
save(j2jod_md,file="output/j2jod_md.RData")

# Once it's saved, we can programmatically clear the environment
rm(list=ls())

# This command clears your memory, to ensure we're not holding onto any unnecessary data
# It stands for "Garbage collection"
gc()

# To load the file we saved 
load("output/j2jod_md.RData")

# I recommend you don't `View` this dataset.
# Rather, use `names` and other tools to inspect it
names(j2jod_md)
summary(j2jod_md$year)
year.freq = data.frame(table(j2jod_md$year, j2jod_md$quarter))

# Let's subset the data to an aggregation level appropriate to look at sex and sector-level flows
# agg_level is described in codebook here: https://lehd.ces.census.gov/data/schema/j2j_latest/lehd_public_use_schema.html#_j2j
j2jod_md = j2jod_md[which(j2jod_md$agg_level==196866),]

# Load industry labels from here: https://lehd.ces.census.gov/data/schema/j2j_latest/lehd_public_use_schema.html#_industry
industry_labels = fread("data/label_industry.csv")
industry_labels = subset(industry_labels, ind_level=="S")
industry_labels$ind_level = NULL
setnames(industry_labels, "label", "industry_label")
j2jod_md = merge(j2jod_md, industry_labels, by="industry", all.x=T)

# Create a decimal year for later graphing
j2jod_md$decimal_year = j2jod_md$year + (j2jod_md$quarter - 1) * 0.25

# What is the industry that each sex has most frequently left the state of Maryland for since 2000?
j2jod_md_aggregate = data.table(j2jod_md)[,.(sumEE = sum(EE, na.rm=T)), by=.(sex, industry_label)]
j2jod_md_aggregate_men = subset(j2jod_md_aggregate, sex==1)
j2jod_md_aggregate_men$industry_label[which.max(j2jod_md_aggregate_men$sumEE)]

j2jod_md_aggregate_women = subset(j2jod_md_aggregate, sex==2)
j2jod_md_aggregate_women$industry_label[which.max(j2jod_md_aggregate_women$sumEE)]

# Some exploratory plots
md_men_retail_trade = subset(j2jod_md, sex==1 & industry_label=="Retail Trade" & year > 2005)
ggplot(md_men_retail_trade, aes(x=decimal_year, y=EE)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at X-axis
  expand_limits(y=c(0, max(md_men_retail_trade$EE*1.1))) + # Start at 0, add 10% padding to max
  labs(
    x="Year",
    y="Job-to-job flows",
    title="Job-to-job flows of Maryland men to other states in Retail Trade"
  )

md_women_hc = subset(j2jod_md, sex==2 & industry_label=="Health Care and Social Assistance" & year > 2005)
ggplot(md_women_hc, aes(x=decimal_year, y=EE)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at X-axis
  expand_limits(y=c(0, max(md_women_hc$EE*1.1))) + # Start at 0, add 10% padding to max
  labs(
    x="Year",
    y="Job-to-job flows",
    title="Job-to-job flows of Maryland women to other states in Health Care and Social Assistance"
  )
