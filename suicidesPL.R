#
# title: youth suicides in Poland
#
library("tidyverse")
library("sf")
library("bdl")
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')
#```
## NUTS id : 00	00	0	 00	 00	000
##            1. 2.	3.	4.	5.	6.
##
##1. Symbol makroregionu (01, 02, ..., 07)
##2. Symbol województwa (00, 02, ..., 32)
##3. Symbol regionu (1, 2)
##4. Symbol podregionu (00, 01, ..., 73)	- dwa znaki
##5. Symbol powiatu (00, 01, ..., 59 - powiat; 60, ..., 99 - miasto na prawach powiatu)
##6. Symbol gminy/części gmin (000......, gdzie ostatni znak stanowi symbol rodzaju jednostki)
##
## TERYT id:  00	00	000
##             1   2    3
##1. Symbol województwa (00, 02, ..., 32)	- dwa znaki
##2. Symbol powiatu (00, 01, ..., 59 - powiat; 60, ..., 99 - miasto na prawach powiatu)
##3. Symbol gminy/części gminy (000......, gdzie ostatni znak oznacza rodzaj jednostki)
#

## Shapes
## https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/
powiaty.PL <-read_sf("/home/tomek/Data/GiS","Powiaty")

## Data layer

# Suicides by age groups
#  1365482 - total
#  1365483 - 0-12 years
#  1365484 - 13-18 years
s.cats <- c("1365482", "1365483", "1365484")

suicides <- get_data_by_variable(s.cats,unitParentId=NULL, unitLevel=5)
suicides0 <- suicides %>%
  select(id, name, year,
         suicides.t=val_1365482,
         suicid12=val_1365483,
         suicid18=val_1365484)
## Recompute per 10000 population IN THIS age group
## Population
## 72306 = 0-4
## 72307 = 5-9
## 81, 82, 83 = 10..12
## 84, 85, 86, 87, 88, 89 = 13..18
c1 <- c('72306', '72307', '81', '82', '83')
## Pop 0--12
l0_12 <- get_data_by_variable(c1,unitParentId=NULL, unitLevel=5, year=yy)
l0_12.0 <- l0_12 %>% select(id, name, year, val_72306, val_72307, val_81, val_82, val_83) %>%
  mutate (ppl12 = val_72306 + val_72307 + val_81 + val_82 + val_83) %>%
  select (id, year, ppl12)
## Pop 13--18
c2 <- c('84', '85', '86', '87', '88', '89')
l13_18 <- get_data_by_variable(c2,unitParentId=NULL, unitLevel=5, year=yy)
l13_18.0 <- l13_18 %>% select (id, name, year, val_84, val_85, val_86, val_87, val_88, val_89) %>%
  mutate (ppl18 =  val_84 + val_85 + val_86 + val_87 + val_88 + val_89) %>%
  select (id, year, ppl18)

## Pop total
l.t <- get_data_by_variable('72305', unitParentId=NULL, unitLevel=5, year=yy)
l.t.0 <- l.t %>% select (id, year, totalppl=val)

## We could get all above  in one command but
## as we did not, we have to join:
data <- suicides0 %>%
  left_join (l0_12.0, by=c("id","year")) %>%
  left_join (l13_18.0, by=c("id","year")) %>%
  left_join (l.t.0, by=c("id","year"))

## Write for subsequnt usage w/o dowloading from BDL
write.csv(data, file='suicidesPL.csv', row.names = F)

##
##
##

s0 <- read.csv("suicidesPL.csv", sep=',',  colClasses = c('id'='character')) %>%
  mutate (name = gsub('Powiat ','', name)) %>%
  mutate (woj = substr(id,3,4),
          powiat = paste0(woj, substr(id, 8,9)))
## structure
str(s0)

## Per 10,000 persons
M1 <- 10000

s1 <- s0 %>%
  group_by(year) %>%
  summarise(suicides.t = sum(suicides.t),
            suicid12 = sum(suicid12),
            suicid18 = sum(suicid18),
            ppl12 = sum (ppl12),
            ppl18 = sum (ppl18),
            totalppl = sum (totalppl),
            ## Per capita
            rs.t = suicides.t/totalppl * M1,
            rs12 = suicid12 / ppl12 * M1,
            rs18 = suicid18/ ppl18 * M1
  )

##
##

s2 <- s0 %>%
  #filter (year == 2018 | year == 2022) %>%
  mutate (
    tsr = suicides.t /totalppl,
    sr = suicid18 / ppl18 * M1) %>%
  select (powiat, name, sr, year)

## box-plot
pow <- s2 %>%
  ggplot(aes(y=sr, x=as.factor(year) )) +
  geom_boxplot() +
  ylab("#") +
  ggtitle("") +
  xlab('')
pow

## ## ##


s4 <- s0 %>%
  mutate (
    tsr = suicides.t /totalppl,
    sr = suicid18 / ppl18 * M1) %>%
  select (powiat, name, sr, year)
##%>%  filter (sr > 5)

## Join data frame with shapes frame
## by common columns JPT_KOD_JE/powiat:
## 2018
ep <- s4 %>%  filter (year == 2018) %>%
  left_join(powiaty.PL, by=c('powiat'="JPT_KOD_JE"))

r2018 <- ggplot(data=ep, aes(fill=sr)) +
  geom_sf() +
  scale_fill_viridis_c()

r2018
##
## 2022
ep <- s4 %>%  filter (year == 2022) %>%
  left_join(powiaty.PL, s4, by=c("JPT_KOD_JE"='powiat'))

r2022 <- ggplot(data=ep, aes(fill=sr)) +
  geom_sf() +
  scale_fill_viridis_c()

r2022

ep_symbol_pos <- st_centroid(ep, of_largest_polygon = TRUE)




r5 <- ggplot() +
  ##facet_wrap(~year) +
  ## Rysuje mapę
  geom_sf(data = powiaty.PL, fill = "grey90")   +
  ##
  geom_sf(data = ep_symbol_pos,
          pch = 21, #### kształt
          ## http://www.sthda.com/english/wiki/ggplot2-point-shapes
          aes(size = sr),
          fill = alpha("red", 0.3),
          col = alpha("red", 0.3))
r5
#####################################

s4 <- s0 %>%
  filter (year == 2022) %>%
  mutate (
    tsr = suicides.t /totalppl,
    sr = suicid18 / ppl18 * M1) %>%
  select (powiat, name, sr, year) %>%
  filter (sr > 5)


ep <- left_join(powiaty.PL, s4, by=c("JPT_KOD_JE"='powiat'))
ep_symbol_pos <- st_centroid(ep, of_largest_polygon = TRUE)

r51 <- ggplot() +
  ##facet_wrap(~year) +
  ## Rysuje mapę
  geom_sf(data = powiaty.PL, fill = "grey90")   +
  ##
  geom_sf(data = ep_symbol_pos,
          pch = 21, #### kształt
          ## http://www.sthda.com/english/wiki/ggplot2-point-shapes
          aes(size = sr),
          fill = alpha("red", 0.3),
          col = alpha("red", 0.3))
r51
