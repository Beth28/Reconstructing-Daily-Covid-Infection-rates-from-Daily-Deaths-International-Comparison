library('dplyr')
library('readr')
library('tidyverse')
library('mgcv')
MarApr <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-March&April.csv")
May <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-May.csv")
Jun <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-June.csv")
Jul <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-July.csv")
Aug <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-August.csv")
Sep <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-September.csv")
Oct <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-October.csv")
Nov <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-November.csv")
Dec <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2020-December.csv")
Jan_21 <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2021-January.csv")
Feb_21 <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2021-February.csv")
Mar_21 <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2021-March.csv")
Apr_21 <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2021-April.csv")
May_21 <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2021-May.csv")
Jun_21 <- read.csv(file = "Data/Cum_deaths_by_occurrence_dates_2021-June.csv")


cum_data <- rbind(MarApr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Jan_21, Feb_21, Mar_21, Apr_21, May_21, Jun_21)

## England and Wales Data

## NHS daily deaths data
## https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
## first entry is Monday 2nd March (nothing before) NHS exact date retrieved 26/2/2021
ed_ENG <-c(1,2,0,2,2,0,4,4,1,9,14,20,22,27,40,46,66,63,105,103,149,159,204,263,326,353,360,437,498,576,645,647,700,778,743,727,813,900,792,740,779,718,699,646,686,639,610,571,524,566,486,502,451,438,386,380,345,341,325,313,306,268,251,259,252,266,256,215,203,196,166,183,162,180,172,167,138,160,144,153,150,122,128,116,133,139,122,124,117,92,83,94,109,111,83,86,83,80,73,69,78,49,52,43,58,56,62,51,50,42,45,36,43,53,54,46,37,32,37,29,31,18,36,19,22,25,24,23,21,38,16,13,16,23,13,21,10,10,17,11,11,10,15,15,5,12,12,9,9,6,8,5,7,11,11,8,5,9,6,8,6,8,11,4,4,5,11,3,10,8,3,3,6,5,10,5,10,10,7,5,2,4,7,3,9,4,6,8,7,9,11,7,9,7,8,14,11,12,16,20,19,11,24,17,26,37,25,22,30,34,38,35,41,43,48,38,38,40,50,58,57,51,61,76,72,70,81,91,91,130,91,116,132,146,146,146,138,162,172,168,179,209,206,212,222,222,209,235,218,233,272,256,311,267,241,272,262,271,296,272,293,329,294,301,267,320,298,312,325,284,285,313,312,269,273,267,300,296,260,274,253,292,273,302,289,299,288,306,317,274,340,332,311,337,385,367,373,376,417,442,457,438,452,475,514,493,499,468,581,610,636,655,653,682,708,759,812,772,771,745,827,810,831,860,825,830,795,729,726,727,693,667,701,659,575,558,571,570,591,482,492,452,442,457,395,405,395,376,325,318,342,306,307,282,250,225)




## Scotland data

SCOT <- read_csv("Data/Scotland_data.csv")
  
ed_SCOT <- as.vector(SCOT$deaths)
#par(mar=c(5,5,1,1),mfrow=c(1,1))
#plot(1:477, ed_SCOT)


## Belgium data loading and cleaning

BEL <- filter(cum_data, country_code == 'BEL') %>%
  select(death_occurrence_date, death_publication_date, cum_death_total_occ_date)

BEL <- BEL %>%
  group_by(death_occurrence_date) %>%
  filter(death_publication_date == max(as.Date(death_publication_date, format = "%Y-%m-%d"))) %>%
  mutate(deaths = 1)

BEL <- BEL[order(as.Date(BEL$death_occurrence_date, format="%Y-%m-%d")),]

for (i in 1:475) {
  BEL$deaths[i + 1] <- BEL$cum_death_total_occ_date[i+1]-BEL$cum_death_total_occ_date[i]
}

ed_BEL <- as.vector(BEL$deaths)

## Spain data

ESP <- filter(cum_data, country_code == 'ESP') %>%
  select(death_occurrence_date, death_publication_date, cum_death_total_occ_date)

ESP <- ESP %>%
  group_by(death_occurrence_date) %>%
  filter(death_publication_date == max(as.Date(death_publication_date, format = "%Y-%m-%d"))) %>%
  mutate(deaths = 1)

ESP <- ESP[order(as.Date(ESP$death_occurrence_date, format="%Y-%m-%d")),]

for (i in 1:502) {
  ESP$deaths[i + 1] <- ESP$cum_death_total_occ_date[i+1]-ESP$cum_death_total_occ_date[i]
}

ed_ESP <- as.vector(ESP$deaths)
#par(mar=c(5,5,1,1),mfrow=c(1,1))
#plot(1:503, ed_ESP)


## Italy data

Italy <- read_csv("Data/Italy.csv", col_types = cols(date_of_occurence = col_date(format = "%d/%m/%Y")))
Italy <- Italy %>%
  mutate(deaths = 5) 

Italy <- Italy[order(as.Date(Italy$date_of_occurence, format="%m-%d-%Y")),]

for (i in 1:491) {
  Italy$deaths[i + 1] <- Italy$cum_deaths[i+1]-Italy$cum_deaths[i]
}

ed_ITA <- as.vector(Italy$deaths)
#par(mar=c(5,5,1,1),mfrow=c(1,1))
#plot(1:492, ed_ITA)


## Denmark Data

DEN <- filter(cum_data, country == 'Denmark') %>%
  select(death_occurrence_date, death_publication_date, cum_death_total_occ_date)

DEN <- DEN %>%
  group_by(death_occurrence_date) %>%
  filter(death_publication_date == max(as.Date(death_publication_date, format = "%Y-%m-%d"))) %>%
  mutate(deaths = 1)

DEN <- DEN[order(as.Date(DEN$death_occurrence_date, format="%Y-%m-%d")),]

for (i in 1:476) {
  DEN$deaths[i + 1] <- DEN$cum_death_total_occ_date[i+1]-DEN$cum_death_total_occ_date[i]
}

ed_DEN <- as.vector(DEN$deaths)



## SWEDEN ##

SWED <- filter(cum_data, country == 'Sweden') %>%
  select(death_occurrence_date, death_publication_date, cum_death_total_occ_date)

SWED <- SWED %>%
  filter(death_publication_date == "2021-06-29") %>%
  mutate(deaths = 0)



SWED <- SWED[order(as.Date(SWED$death_occurrence_date, format="%Y-%m-%d")),]

SWED <- SWED[-(1:65),]
for (i in 1:476) {
  SWED$deaths[i + 1] <- SWED$cum_death_total_occ_date[i+1]-SWED$cum_death_total_occ_date[i]
}

ed_SWED <- as.vector(SWED$deaths)
#par(mar=c(5,5,1,1),mfrow=c(1,1))
#plot(1:477, ed_SWED)

## Switzerland ##


SWIT <- filter(cum_data, country == 'Switzerland') %>%
  select(death_occurrence_date, cum_death_total_occ_date, death_publication_date)

SWIT <- SWIT %>%
  group_by(death_occurrence_date) %>%
  filter(death_publication_date == max(as.Date(death_publication_date, format = "%Y-%m-%d"))) %>%
  mutate(deaths=2)


SWIT <- SWIT[order(as.Date(SWIT$death_occurrence_date, format="%Y-%m-%d")),]

for (i in 1:482) {
  SWIT$deaths[i + 1] <- SWIT$cum_death_total_occ_date[i+1]-SWIT$cum_death_total_occ_date[i]
}

ed_SWIT <- as.vector(SWIT$deaths)

#par(mar=c(5,5,1,1),mfrow=c(1,1))
#plot(1:483, ed_SWIT)


## PORTUGAl ##
POR <- read.csv("Data/Portugal_covid_death_cum.csv")


POR <- POR %>%
  mutate(deaths=Cum_deaths[1])


for (i in 1:471) {
  POR$deaths[i + 1] <- POR$Cum_deaths[i+1]-POR$Cum_deaths[i]
}



ed_POR <- as.vector(POR$deaths)




## Netherlands  

NLD <- filter(cum_data, country == 'Netherlands') %>%
  select(death_occurrence_date, death_publication_date, cum_death_total_occ_date)

NLD <- NLD %>%
  group_by(death_occurrence_date) %>%
  filter(death_publication_date == max(as.Date(death_publication_date, format = "%Y-%m-%d"))) %>%
  mutate(deaths = 0)

NLD <- NLD[order(as.Date(NLD$death_occurrence_date, format="%Y-%m-%d")),]

for (i in 1:488) {
  NLD$deaths[i + 1] <- NLD$cum_death_total_occ_date[i+1]-NLD$cum_death_total_occ_date[i]
}

ed_NLD <- as.vector(NLD$deaths)
#par(mar=c(5,5,1,1),mfrow=c(1,1))
#plot(1:489, ed_NLD)

