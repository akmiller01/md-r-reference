list.of.packages <- c(
  "data.table", "tidyverse", "tibble", "tidyr", "dplyr", "rstudioapi","ggplot2"
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

# Tired of VLOOKUPs and linked Excel spreadsheets?
# Merging files in R allows you to keep references to data in memory, and merge them once
# Instead of having Excel constantly check for linked files.

# This will be a short module, but merging is often one of the most performed
# operations in data science, so it's critically important!

# Let's start with some sample datasets from the ACS 5-year

# First, our population data from the repetition module
pop = read.csv(
  "data/acs5_year_md_pop.csv",
  na.strings="",
  as.is=T,
  colClasses = c("GEOID"="character")
)
View(pop)

# And now total small business loans from the FFIEC
loans = read.csv(
  "data/ffiec_loans.csv",
  na.strings="",
  as.is=T,
  colClasses = c("GEOID"="character")
)
View(loans)

# Critically important in merging datasets is understanding that the text must match verbatim
# We can check to see how well two columns match using set theory
# First, the `unique` function reduces a column into only the unique observations
non_unique_set = c("a","a","a","b","b","b")
unique(non_unique_set)
# and second, the `setdiff` function shows us any differences between two sets.
set1 = c("a","b","c")
set2 = c("a","b","d")
setdiff(set1,set2)
setdiff(set2,set1)

# Note, that setdiff shows you observations of the first argument that are different from the second
# So therefore `setdiff(set1,set2)` is not equal to `setdiff(set2,set1)`
# When checking country name variance, you might want to check both directions

setdiff(unique(pop$GEOID), unique(loans$GEOID))
# This should return `character(0)`, meaning a character vector of length 0, this means there are no differences.
setdiff(unique(loans$GEOID), unique(pop$GEOID))
# This had a ton of differences!! What's happening?
# I'll give you a clue, the first two letters of GEOID signify the state
# And Maryland's state code is 24

# Now, before we merge the datasets, always check to see how many observations you currently have for each
nrow(pop)
nrow(loans)

# By default, the merge function will drop observations that it cannot find a match for. To prevent this default,
# you must set either `all=TRUE` or one of `all.x=TRUE` or `all.y=TRUE`
# `all=TRUE` means you keep all observations of both datasets, regardless of match.
# The first dataset you put into the merge function is treated as `x` so `all.x=TRUE` means keep all observations
# from the first dataset, but only keep the matching observations from `y`. And `all.y` is vice versa.

# So let's try the default merge
default_merged_pop = merge(pop,loans,by=c("GEOID","year"))

# And check to see the length of the result
nrow(default_merged_pop)
nrow(default_merged_pop) == nrow(pop)
nrow(pop) - nrow(default_merged_pop) # The number of rows hasn't changed.

# This would be fine in the context of calculating loans per capita. Because we cannot calculate it without valid
# observations of both variables, so dropping missing rows is of no consequence.
default_merged_pop$loans_per_capita = default_merged_pop$total_small_business_loan_dollars/default_merged_pop$total_population

# Where population was zero, it's produced an infinite value. Let's set it to zero
default_merged_pop$loans_per_capita[
  which(is.infinite(default_merged_pop$loans_per_capita))
] = 0

# Here's what the code looks like if you want to keep all obs of X
# This is the same as our default because all matches from X had a match in Y
x_merged = merge(pop,loans,by=c("GEOID","year"), all.x=TRUE)
# And likewise for all observations
all_merged = merge(pop,loans,by=c("GEOID","year"), all=T)


# Now, remember how I said to always look at the original number of observations?
# This is not only important for missing data, it's important for duplicated data as well.
# The `merge` function assumes that your `y` dataset has unique observations of the columns
# you specify in the `by` argument. Let's try doubling up loans and see what happens
loans_double = rbind(loans,loans)
test_merge = merge(pop,loans_double,by=c("GEOID","year"))
nrow(test_merge)
# Wow! 29360 rows, despite the fact that we only have 14680 observations of pop.
# This is because it will match duplicates of y to every observation of x, and make a new observation of x
# for every duplicate observation of y.

# So the moral of the story is to first check whether the names you're merging on match
# And second to always check the observations before and after the merge

# Let's make a small practical aggregation, manipulation, and chart of the data
merged_agg = data.table(default_merged_pop)[,.(
  total_population=sum(total_population, na.rm=T),
  total_small_business_loan_dollars=sum(total_small_business_loan_dollars, na.rm=T)
),by=.(county_name, year)]

merged_agg = subset(merged_agg, year==2023)

merged_agg$loans_per_capita = merged_agg$total_small_business_loan_dollars/merged_agg$total_population

# This is how you order a plot, first order the data, and then factor the x-label
merged_agg = merged_agg[order(-merged_agg$loans_per_capita),]
merged_agg$county_name = factor(merged_agg$county_name, levels=merged_agg$county_name)

ggplot(merged_agg, aes(x=county_name, y=loans_per_capita)) +
  geom_bar(stat="identity") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
