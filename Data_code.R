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


## England data 

eng <- read_csv("Data/england_deaths.csv") %>%
  select(date, newDailyNsoDeathsByDeathDate)
eng <- eng[order(as.Date(eng$date, format = "%d/%m/%Y")),]
# start on second March as previous deaths not community aquired, from international travel, up to 30th June
eng <- eng[33:518, ]
ed_ENG <- as.vector(eng$newDailyNsoDeathsByDeathDate)




## Scotland data

SCOT <- read_csv("Data/Scotland_data.csv", show_col_types = FALSE)
  
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

