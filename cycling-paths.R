# Cycling paths in PL
# -------------------
# 288080 = drogi dla rowerów ogółem (km) / cycling paths total (kms)
# 288082 = drogi dla rowerów na 10 tys. ludności / CP per 10 ths
# 395402 = drogi gminne i powiatowe o twardej nawierzchni na 100km2
#          roads per 100kmsq
# 2018   = powierzchnia km2 (podział terytorialny)
#          area  kmsq
library("bdl")
library("tidyverse")
#install.packages("bdl")
options(bdl.api_private_key ='####################################')

# Download data from BDL database

# county-level vars
p.vars <- c("288080", "288082", "395402", "2018")
# province-level vars
w.vars <- c("2018")

d0 <- get_data_by_variable(p.vars, unitParentId=NULL, unitLevel=5)
d1 <- d0 %>% select(id, name, year,
         cp=val_288080,
         cp10=val_288082,
         r100=val_395402,
         area=val_2018) %>%
  filter (as.numeric(year) == 2022)  %>%
  mutate (r = r100 * area /100, cpr = cp /r * 100)

## write to csv
#
##write.csv(d1, file='cycpath.csv', sep=';', row.names = F)
## write.csv has comma as column separator hard coded
#write.table(d1, file='cycpath.csv', sep = ';', row.names = F)
##?write.csv
#cp0 <- read.csv('cycpath.csv', sep=';')
### inspect (str)uctrure
#str(cp0)

## we need only names so limit download to the last year
#
w0 <- get_data_by_variable(w.vars, unitParentId=NULL, unitLevel=2, year = 2022)

w1 <- w0 %>% select (id, name, area=val) %>%
  mutate(wojId = substr(id, 3,4))

## join w1 (provinces) with d1 (counties)

d2 <- d1 %>%
  mutate(wojId = substr(id, 3,4)) %>%
  left_join(w1, by='wojId')
##
## better names:
## start again
w1 <- w0 %>% select (wid=id, wname=name, warea=val) %>%
  mutate(wojId = substr(wid, 3,4))

d2 <- d1 %>%
  mutate(wojId = substr(id, 3,4)) %>%
  left_join(w1, by='wojId')

## OK add macroregions (7)
m0 <- get_data_by_variable(w.vars, unitParentId=NULL, unitLevel=1, year = 2022) %>%
  mutate(mname = gsub("MAKROREGION ", "", name)) %>%
  ## Going international
  mutate (mname = recode(mname,
    "POŁUDNIOWY" = "South",
    "PÓŁNOCNO-ZACHODNI" = "North-West",
    "POŁUDNIOWO-ZACHODNI" ='South-West',
    "PÓŁNOCNY" = 'North',
    "CENTRALNY" = 'Central',
    "WSCHODNI" = 'East',
    "WOJEWÓDZTWO MAZOWIECKIE" = 'Mazovian'
)) %>%
  mutate(mId = substr(id, 1,2)) %>%
  select (mId, mname, marea=val)

## Print column mname
m0$mname

## Join with d2
d3 <- d2 %>%
  mutate(mId = substr(id, 1,2)) %>%
  left_join(m0, by='mId')

## Descriptive statistics
## mean median sd max min

d3s <- d3 %>%
  group_by(mname) %>%
  summarise( mean = mean(cpr),
             median = median(cpr),
             sd = sd(cpr),
             min = min(cpr),
             max = max(cpr),
             n = n())

library("knitr")

## Insert table sorted by 1st column (mname)
kable(d3s)
## Insert table sortted by mean of cpr
table1 <- d3s %>% arrange(desc(mean)) %>%
  kable(col.names = c('Name', 'Mean', 'Median', 'SD', 'Min', 'Max', 'Counties'))
table1

## Graphics
## Histogram
ggplot(d1, aes(x=cpr)) +
  geom_histogram(binwidth = 5, color='skyblue', fill='darkblue')

## Boxplot
ggplot(d3, aes(x=mname, y=cpr)) +
  geom_boxplot(color='skyblue') +
  ggtitle("Cycling path length as % of road length")

## Dotplot
ggplot(d3, aes(x=mname, y=cpr)) +
  geom_jitter(color='skyblue', cex=.8, width=.2) +
  ggtitle("Cycling path length as % of road length")

## mean % by region
## barplot
d3 %>% group_by(mname) %>% summarise(cpr=mean(cpr)) %>%
ggplot(aes(x=mname, y=cpr)) +
  geom_bar(color='skyblue', stat="identity") +
  ggtitle("Cycling path length as % of road lengths")

## Is a relation between cpr and r100?
ggplot(d3, aes(x=r100, y=cpr)) +
  geom_point(color='red') +
  ggtitle("Cycling path length as % of road lengths")

## No [strong] relation

ggplot(d3, aes(x=log(r100), y=log(cpr)) ) +
  geom_point(color='red') +
  geom_smooth(method = "lm") +
  ggtitle("Cycling path length as % of road lengths")

## correlation coefficient
## between r100 and cpr
d3 %>% select (r100, cpr) %>% cor(use = "complete.obs")

## What next?
## ----------
## add text
## Publish @ RPubs.com
## Upload @ github

## END
