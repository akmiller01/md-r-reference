list.of.packages <- c(
  "data.table", "tidyverse", "tibble", "tidyr", "dplyr", "rstudioapi","ggplot2", "scales"
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

# As with module 3, let's grab some test data ACS 5-year
pop = read.csv(
  "data/acs5_year_md_pop.csv",
  na.strings="",
  as.is=T,
  colClasses = c("GEOID"="character")
)

# Quick aggregation by urban/mixed/rural classification
pop_agg = data.table(pop)[,.(total_population=sum(total_population, na.rm=T)), by=.(urban_class, year)]

pop_agg_urban = subset(pop_agg, urban_class=="Urban")
# R comes with some default options for creating charts. Namely `plot` and a few others.
plot(y=pop_agg_urban$total_population,x=pop_agg_urban$year)

# By default, it's a scatter plot. You can also make it a line
plot(y=pop_agg_urban$total_population,x=pop_agg_urban$year,type="l")

# And you can also use function notation `y~x` if you specify the `data` argument
plot(total_population~year,data=pop_agg_urban,type="l")

# But these charts are ugly and boring. Let me formally introduce you to ggplot2!
ggplot(pop_agg_urban,aes(x=year,y=total_population)) +
  geom_line() +
  scale_y_continuous(labels=number_format(big.mark=",")) +
  theme_bw() +
  labs(x="Year",y="Urban population",title="Maryland urban population over time")

# ggplot2 is additive, meaning you can start with your default configuration and add details later
config = ggplot(pop_agg_urban,aes(x=year,y=total_population))
config + geom_line()
config + geom_point()
config + geom_smooth(method="loess")

# It's also useful for making quite complicated charts. Let's say we want to all urban classes on the same chart
ggplot(pop_agg,aes(x=year,y=total_population,colour=urban_class,group=urban_class)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels=number_format(big.mark=",")) +
  theme_bw() +
  labs(x="Year",y="Population",title="Maryland population by urban-class over time")

# Here's an example of a styled chart I recently made based on
# data from the Maryland Office for Financial Regulation annual reports
bank_branch_dat = fread("data/ofr_bank_branches.csv")

ggplot(bank_branch_dat,aes(x=Year,y=Branches,group=Type,fill=Type)) +
  geom_bar(stat="identity") + # This let's bar charts plot the actual number instead of counting instances
  scale_fill_manual(values=c("#3C4586", "#C84A35")) + # Here we manually define the fill colors
  scale_y_continuous(expand = c(0, 0)) + # This makes the Y-axis begin at the X-axis
  scale_x_continuous(breaks=c(2022, 2023, 2024)) + # Enforcing round year labels
  expand_limits(y=c(0, 320)) + # Making sure the chart goes down to 0 and up to 320
  theme_bw() + # Followed by some styles and labels
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,panel.grid.major.y = element_line(colour = "grey80")
    ,panel.grid.minor.y = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
  ) +
  labs(
    y="Number of branches",
    x="",
    fill=""
  )

# Also importantly, you can save these charts as print quality vector files
# By default `ggsave` will save the last chart you displayed, or you could store your chart as a variable and save it that way
ggsave("output/maryland_bank_branch_consolidation.png")
ggsave("output/maryland_bank_branch_consolidation.svg")

# Ultimately, the options with ggplot are endless, so instead of exhaustively showing you options, I would
# Google around to see what's possible!
