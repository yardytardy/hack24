library(tidyverse)
library(dplyr)
library(tidycensus)
library(tigris)
library(MatchIt)
library(scales)
library(gridExtra)

census_api_key("c700feda3d10ad225c1cb8ace1f5dbb438673611")

commdata <- read.csv("commdata.csv", header = TRUE)
electric <- read.csv("electric.csv", header = TRUE)
gas <- read.csv("gas.csv", header = TRUE)
ghg <- read.csv("ghg.csv", header = TRUE)
vmt <- read.csv("vmt.csv", header = TRUE)
evs <- read.csv("evs.csv", header = TRUE)
solar <- read.csv("solar.csv", header = TRUE)
lifetimeres <- read.csv("lifetimeres.csv", header = TRUE)
lifetimecomm <- read.csv("lifetimecomm.csv", header = TRUE)

commdata <- commdata |> filter(year == 2020) |>
  mutate(muni = tolower(muni),
         medinc = case_when(medinc != "250,000+" ~ as.numeric(medinc),
                            TRUE ~ 250000),
         ciprops = as.numeric(ciprops),
         lmi = case_when(lmi == "Yes" ~ 1,
                         TRUE ~ 0),
         less75own = case_when(less75own == "Yes" ~ 1,
                               TRUE ~ 0))
electric <- electric |> filter(year == 2020) |>  
  mutate(muni = tolower(muni),
        reselec = as.numeric(reselec),
        commelec = as.numeric(commelec),
        induselec = as.numeric(induselec),
        streetelec = as.numeric(streetelec),
        totelec = as.numeric(totelec)
)
electric <- electric |> 
  mutate(reselec = case_when(reselec > -1 ~ reselec,
                             TRUE ~ 0),
         commelec = case_when(commelec > -1 ~ commelec,
                              TRUE ~ 0),
         induselec = case_when(induselec > -1 ~ induselec,
                               TRUE ~ 0),
         streetelec = case_when(streetelec > -1 ~ streetelec,
                                TRUE ~ 0),
         totelec = case_when(totelec > -1 ~ totelec,
                             TRUE ~ 0)
         )
gas <- gas |> filter(year == 2020) |>  
  mutate(muni = tolower(muni),
         resgas = as.numeric(resgas),
         commgas = as.numeric(commgas),
         indusgas = as.numeric(indusgas),
         streetgas = as.numeric(streetgas),
         totgas = as.numeric(totgas)
  )
gas <- gas |> 
  mutate(resgas = case_when(resgas > -1 ~ resgas,
                             TRUE ~ 0),
         commgas = case_when(commgas > -1 ~ commgas,
                              TRUE ~ 0),
         indusgas = case_when(indusgas > -1 ~ indusgas,
                               TRUE ~ 0),
         streetgas = case_when(streetgas > -1 ~ streetgas,
                                TRUE ~ 0),
         totgas = case_when(totgas > -1 ~ totgas,
                             TRUE ~ 0)
  )
ghg <- ghg |> filter(year == 2020) |> 
  mutate(muni = tolower(muni),
         reselecghg = as.numeric(reselecghg),
         commelecghg = as.numeric(commelecghg),
         induselecghg = as.numeric(induselecghg),
         streetelecghg = as.numeric(streetelecghg),
         resgasghg = as.numeric(resgasghg),
         commgasghg = as.numeric(commgasghg),
         indusgasghg = as.numeric(indusgasghg),
         streetgasghg = as.numeric(streetgasghg),
         otherheatghg = as.numeric(otherheatghg),
         vehiclesghg = as.numeric(vehiclesghg),
         totghg = as.numeric(totghg))
ghg <- ghg |> mutate(
  reselecghg = case_when(reselecghg > -1 ~ reselecghg,
                      TRUE ~ 0),
  commelecghg = case_when(commelecghg > -1 ~ commelecghg,
                       TRUE ~ 0),
  induselecghg = case_when(induselecghg > -1 ~ induselecghg,
                        TRUE ~ 0),
  streetelecghg = case_when(streetelecghg > -1 ~ streetelecghg,
                         TRUE ~ 0),
  resgasghg = case_when(resgasghg > -1 ~ resgasghg,
                     TRUE ~ 0),
  commgasghg = case_when(commgasghg > -1 ~ commgasghg,
                      TRUE ~ 0),
  indusgasghg = case_when(indusgasghg > -1 ~ indusgasghg,
                       TRUE ~ 0),
  streetgasghg = case_when(streetgasghg > -1 ~ streetgasghg,
                        TRUE ~ 0),
  otherheatghg = case_when(otherheatghg > -1 ~ otherheatghg,
                           TRUE ~ 0),
  vehiclesghg = case_when(vehiclesghg > -1 ~ vehiclesghg,
                           TRUE ~ 0),
  totghg = case_when(totghg > -1 ~ totghg,
                           TRUE ~ 0)
)
vmt <- vmt |> filter(year > 2017) |>
  mutate(year = 2020,
         muni = tolower(muni))
evs <- evs |> filter(year == 2020) |> select(muni, totpass, evs, pctev) |>
  mutate(muni = tolower(muni))
solar <- solar |> filter(year == 2020) |> 
  mutate(muni = tolower(muni))
lifetimeres <- lifetimeres |> 
  mutate(muni = tolower(muni))
lifetimecomm <- lifetimecomm |> 
  mutate(muni = tolower(muni))

consoldata <- left_join(commdata, electric, by = c("muni", "county", "year"))
consoldata <- left_join(consoldata, gas, by = c("muni", "county", "year"))
consoldata <- left_join(consoldata, ghg, by = c("muni", "county", "year"))
consoldata <- left_join(consoldata, vmt, by = c("muni", "county", "year"))
consoldata <- left_join(consoldata, evs, by = "muni")
consoldata <- left_join(consoldata, solar, by = c("muni", "county", "year"))
consoldata <- left_join(consoldata, lifetimeres, by = c("muni", "county"))
consoldata <- left_join(consoldata, lifetimecomm, by = c("muni", "county"))

write.csv(consoldata, file = "consoldata.csv")
