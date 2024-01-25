## Fertility
##
library("ggplot2")
library("dplyr")
library("scales")
library("tidyr")
library("knitr")
library("eurostat")

##
ex <- c('CH', 'LU', 'ME', 'MK', 'MT', 'RS', 'CY')

fert_0 <- get_eurostat("tgs00100") %>%
  mutate (time = as.character(time)) %>%
  mutate (year = as.numeric(substr(time, 1, 4)), 
          month = as.numeric(substr(time, 6,7)),
          geo = as.factor(geo)) %>%
  select (geo, year, values) %>%
  filter (year == 2020) %>%
  mutate (member = substr(geo, 1, 2)) %>%
  filter (! member %in% ex )


levels(as.factor(fert_0$member))
##levels(as.factor(fert_0$geo))
##fert.pl <- fert_0 %>% filter (geo == 'PL') 
p0 <- ggplot(fert_0, aes(x=member, y=values )) + 
    ggtitle("Fertility rate by NUTS2 regions (2020)", subtitle="Eurostat: tgs00100 table") +
  geom_hline(yintercept = 2.15, color="navyblue", alpha=.25, size=1.2) +
  geom_boxplot(color='deeppink') + ylab("") + xlab("");

ggsave(p0, file="eurofert.png", width=10)
p0

 
## https://ec.europa.eu/eurostat/databrowser/view/DEMO_FIND/default/table?lang=en
## Fertility indicators

fert_1 <- get_eurostat("demo_find")
## Mean age of women at birth of first child [AGEMOTH1]
## Proportion of live births outside marriage [NMARPCT]
## Total fertility rate [TOTFERRT]

## btw
## https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Marriage_and_divorce_statistics
## 
fert_1 <- get_eurostat("demo_find") %>%
  filter (indic_de == "TOTFERRT") %>%
  mutate (time = as.character(time)) %>%
  mutate (year = as.numeric(substr(time, 1, 4)), 
          month = as.numeric(substr(time, 6,7)),
          geo = as.factor(geo)) %>%
  select (geo, year, values) %>%
  mutate (member = substr(geo, 1, 2)) %>%
  filter (! member %in% ex )

p1 <- fert_1 %>%
  filter (geo == 'PL') %>%
  ggplot( aes(x=year, y=values )) + 
    geom_line() +
  ggtitle ("Totale fertility rate for Poland")
p1

p1 <- fert_1 %>%
  filter (geo %in% c('PL', 'DE', 'CZ', 'LT', 'SK')) %>%
  ggplot( aes(x=year, y=values, color=geo )) + 
  geom_line() +
  ggtitle ("Totale fertility rate for Poland")
p1

## get rid of PL/DE etc
## https://ec.europa.eu/eurostat/data/metadata/code-lists
geo_dic <- get_eurostat_dic("geo")

fert_2 <- left_join(fert_1, geo_dic, by=c('geo'='code_name'))

p1x <- fert_2 %>%
  filter (geo %in% c('PL', 'DE', 'CZ', 'LT', 'SK')) %>%
  ggplot( aes(x=year, y=values, color=full_name )) + 
  geom_line() +
  ggtitle ("Totale fertility rate for Poland")
p1x
