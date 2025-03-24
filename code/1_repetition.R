# In this module we'll discuss various methods of repetition.
# From here on out, rather than adding in `library` or `require` for each package,
# I'm going to use this stock piece of code that will check and install required libraries.
list.of.packages = c(
  "data.table","ggplot2", "tidyverse", "tibble", "tidyr", "dplyr", "rstudioapi"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# This is another stock piece of code that I use to set the working directory
# to wherever the code is saved, regardless of where you saved it or how
# your operating system determines paths.
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

# For testing purposes, some ACS 5-year data on total population has been placed in the data folder.
dat = read.csv(
  "data/acs5_year_md_pop.csv",
  na.strings="",
  as.is=T,
  colClasses = c("GEOID"="character")
)
# We tell R to treat GEOID as a character so it doesn't drop leading zeros or apply scientific notation

# Check the column names
names(dat)

# This is called a for loop. You can incrementally assign a value to a variable from a vector, and operate on it within the loop
vector = c(1,2,3)
for(element in vector){
  message(element)
}

# Alternatively, you can use the length of the vector to index it, and loop through the index
for(i in 1:length(vector)){
  message(vector[i])
}

# Looping through column names
for(nam in names(dat)){
  message(nam)
}

# Here is a function. You can use it to operate on a series of inputs called `arguments`
xyz = function(x,y,z){
  if(x > y){
    return(z)
  }else{
    return(1-z)
  }
}
xyz(1,2,0.25)
xyz(2,1,0.25)

# Once defined, you can use a function any number of times. It's great for keeping your code tidy, especially in loops
# Here's a function to tell whether a vector is able to be converted to a number
numericable = function(vec){
  vec = vec[complete.cases(vec)]
  num.vec = as.numeric(vec)
  num.vec = num.vec[complete.cases(num.vec)]
  if(length(num.vec)==length(vec)){
    return(T)
  }
  return(F)
}

numericable(c(1,2,3))
numericable(c("1","2","3"))
numericable(c("Hello","Goodbye")) # This will give you a warning message.

indicators = c("total_population", "urban_class")
# Loop through indicators in dat, check whether data is numericable, if so, print the mean
for(ind in indicators){
  message("Checking ", ind, " to see if it is numeric...")
  indicator_vector = dat[,ind]
  if(numericable(indicator_vector)){
    message("It is! The mean is ", mean(as.numeric(indicator_vector),na.rm=T))
  }else{
    message("It is not :(")
  }
  message("")
}

# My preferred way to operate on subgroups of data is with the data.table package
# Let's calculate the sum of census tract total population by county
dat.tab = data.table(dat)
dat.means = dat.tab[,.(total_population=sum(total_population, na.rm=T)),by=.(year,county_name)]

# We'll touch more on ggplot2 in later modules, but I quickly want to show how you can create charts in a loop
for(dat_county_name in unique(dat.means$county_name)){
  sub.dat = subset(dat.means,county_name==dat_county_name)
  p = ggplot(sub.dat,aes(x=year,y=total_population)) + geom_point() + geom_line() + theme_classic()
  ggsave(paste0("output/",dat_county_name,"_pop.png"), p)
}

# Check the output folder and see all the charts you created!
