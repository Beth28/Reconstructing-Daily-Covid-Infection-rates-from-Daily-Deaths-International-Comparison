source('Data_code.R')
source('Functions_for_simple.R')
source('functions_for_complex.R')

## England

lock_eng <- c(julian(as.Date("2020-3-24"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-11-5"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-1-5"),origin=as.Date("2019-12-31")))

dat_eng <- simple_model_dat(ed_ENG, "2020-03-02", 1) # returns data in required format for plotting
b_eng <- simple_model_b(ed_ENG, "2020-03-02", 1) # fits simple model



## Scotland

lock_scot <- c(julian(as.Date("2020-3-24"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-01-05"),origin=as.Date("2019-12-31")))

dat_scot <- simple_model_dat(ed_SCOT, "2020-03-11", 4)
b_scot <- simple_model_b(ed_SCOT, "2020-03-11", 4)




## Belgium

lock_bel <- c(julian(as.Date("2020-3-18"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-11-2"),origin=as.Date("2019-12-31")))


dat_bel <- simple_model_dat(ed_BEL, "2020-03-10", 2)
b_bel <- simple_model_b(ed_BEL, "2020-03-10", 2)


## Denmark

lock_dnk <- c(julian(as.Date("2020-3-18"),origin=as.Date("2019-12-31")))

dat_dnk <- simple_model_dat(ed_DEN, "2020-03-11", 1)
b_dnk <- simple_model_b(ed_DEN, "2020-03-11", 1)

## Italy

lock_ita <- c(julian(as.Date("2020-3-11"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-12-24"),origin=as.Date("2019-12-31")))

dat_ita <- simple_model_dat(ed_ITA, "2020-02-24", 1)
b_ita <- simple_model_b(ed_ITA, "2020-02-24", 1)


## Netherlands

lock_nld <- c(julian(as.Date("2020-03-23"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-12-15"),origin=as.Date("2019-12-31")))

dat_nld <- simple_model_dat(ed_NLD, "2020-02-27", 4)
b_nld <- simple_model_b(ed_NLD, "2020-02-27", 4)

## Portugal

lock_por <- c(julian(as.Date("2020-3-18"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-1-15"),origin=as.Date("2019-12-31")))
#julian(as.Date("2021-3-11"),origin=as.Date("2019-12-31")))

dat_por <- simple_model_dat(ed_POR, "2020-03-16", 4)
b_por <- simple_model_b(ed_POR, "2020-03-16", 4)




## Spain

lock_esp <- c(julian(as.Date("2020-3-14"),origin=as.Date("2019-12-31")))
             # julian(as.Date("2020-10-25"),origin=as.Date("2019-12-31")))

dat_esp <- simple_model_dat(ed_ESP, "2020-02-13", 4)
b_esp <- simple_model_b(ed_ESP, "2020-02-13", 4)



## Sweden

dat_swed <- simple_model_dat(ed_SWED, "2020-03-9", 4)
b_swed <- simple_model_b(ed_SWED, "2020-03-9", 4)

## Switzerland

lock_swit <- c(julian(as.Date("2020-3-19"),origin=as.Date("2019-12-31")))

dat_swit <- simple_model_dat(ed_SWIT, "2020-03-5", 4)
b_swit <- simple_model_b(ed_SWIT, "2020-03-5", 4)



