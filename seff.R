## Processing google forms with R
## The form is available at
## https://docs.google.com/spreadsheets/d/152N9dmaqIgiKZghmY6-D_3kSsONeeBp2Mmn9WIubdjc/edit?usp=sharing
## and contains
## 10 questions with General SelfEfficiacy Scale (psychological concept)
## plus questionis about age and sex

library("tidyverse")
library("knitr")
library("ggplot2")
## library for reading googlesheets
library("googlesheets4")

ZeroHour <- as.POSIXct("2021-12-31 00:00:00", format = "%Y-%m-%d %H:%M:%S")

## names for columns
## p1-p10 for GSE scale plus sex and age
qnames <- paste0("p", 1:10)
qqnames <- c('x1', qnames, "sex", 'age' )

## read the data:
##s0 <- read_sheet('152N9dmaqIgiKZghmY6-D_3kSsONeeBp2Mmn9WIubdjc' )
##
## or read and process at once:
##  change column names (col_names option)
##  filter-out responses older than ZeroHour
##  recode answers to numbers 1..4
##  sum-up scores for GSE 
s1 <- read_sheet('152N9dmaqIgiKZghmY6-D_3kSsONeeBp2Mmn9WIubdjc',
                       col_names = qqnames ) %>% 
  filter(x1 > ZeroHour ) %>%
  mutate(across(all_of(qnames), ~ case_when(
    . == "Strongly disagree"       ~ 1,
    . == "Disagree"                ~ 2,
    . == "Agree"                   ~ 3,
    . == "Strongly agree"          ~ 4
  ))) %>%
  mutate ( se = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10) 
 
  
  
## number of complete forms
Ntotal <- nrow(s1)

## Womens more self-efficient?
##
s.sex <- s1 %>% group_by(sex) %>%
     summarise (se = mean(se, na.rm=T) )

## Younger persons are more self-efficient?
## linear regression between se and age
model1 <- lm (data=s1, se ~ age)
summary(model1)

## or compare mean values
## but first recode age into two categories
## young (< 40) and old

ear.se.f <- s1 %>%
  select (se, age) %>%
  mutate( age=case_when(age >= 40 ~ "old",  TRUE ~ "young") ) %>%
  group_by(age)%>%
  summarize(mse = mean(se))



## end
