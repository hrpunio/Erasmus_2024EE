## Fertility
library("ggplot2")
library("dplyr")
library("scales")
library("tidyr")
library("knitr")
library("eurostat")


fert_0 <- get_eurostat("demo_fmonth",  stringsAsFactors = FALSE)  

fert.pl <- fert_0 %>% filter (geo == 'PL') 


mutate (time = as.character(time)) %>%
mutate (year = as.numeric(substr(time, 1, 4)), 
     month = as.numeric(substr(time, 6,7)),
     geo = as.factor(geo)) %>%
  select (geo, year, month, value=values)

head(hicp)

## dane wczytane
