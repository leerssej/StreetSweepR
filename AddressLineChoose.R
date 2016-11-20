# R Script - dplyr predominant
# Author: leerssej
# Date: updated - 20 Nov 2016
# Desc: Validate address columns for proper street address information. 
# Desc: (particulary useful for where there are more than one address line available to use for geocoding.)
# Desc: I have left some of the standard code I have used for building in flag columns as suggestions for possible approaches to apply;
# Desc: as well, there are references to additional postal coding tables that will improve results in some countries with special postal zonation.

options(stringsAsFactors = FALSE)
library(magrittr)
library(tidyverse)

###### Test Street Columns and Determine which is more likely to be the better column ######
# Build Valid Logic Switches 
vldStTyp <- function (chkColNm) {
x <- 1*(grepl("(^|\\s+)((Rd|Rr(uga)*|Str*|Dr|Av(\\.|e)*|Avda|Carretera|Calle|Rue(da)*|Rua|V(ia(lle)*)*|Passeig|P|Plaza|Place|Blvd|Bvd|Bv|Marg|Ct|Ul\\.*|Pl|Highway|Hwy|Pkwy|Parkway|Way|Paseo|Terr(ace)*|Park)|(\\w+(strasse|strabe|park|plein)))(\\s+|$)", chkColNm, ignore.case = T))
}
vldNumStr <- function (chkColNm) {
x <- 1*(grepl("(^\\d+\\s+\\w+\\s*$)|(^\\w+\\s+\\d+\\s*$)", chkColNm, perl = T, ignore.case = T))
}
vldNonNA <- function (chkColNm) {
x <- 1*!is.na(chkColNm)
}

# Build Columns to integrate switches into columns
BestClndAddrData <- ClndAddrData %>% mutate(vldStTyp1 = vldStTyp(swpAddrLn1))
BestClndAddrData %<>% mutate(vldStTyp2 = vldStTyp(swpAddrLn2))
BestClndAddrData %<>% mutate(vldNumStr1 = vldNumStr(swpAddrLn1))
BestClndAddrData %<>% mutate(vldNumStr2 = vldNumStr(swpAddrLn2))
BestClndAddrData %<>% mutate(vldNonNA1 = vldNonNA(swpAddrLn1))
BestClndAddrData %<>% mutate(vldNonNA2 = vldNonNA(swpAddrLn2))
    
### BetterAddrLn SelectR ###
## Compare switch results to preferentially select the most probably valid of the columns
BestClndAddrData %<>% 
    mutate(PrefNmStrStTyp = ifelse(vldStTyp1 == 1 & vldStTyp2 == 1, swpAddrLn1,
                            ifelse(vldStTyp1 == 1 & vldStTyp2 == 0, swpAddrLn1, 
                            ifelse(vldStTyp1 == 0 & vldStTyp2 == 1, swpAddrLn2, 
                            ifelse(vldNumStr1 == 1 & vldNumStr2 == 1, swpAddrLn1,
                            ifelse(vldNumStr1 == 1 & vldNumStr2 == 0, swpAddrLn1,
                            ifelse(vldNumStr1 == 0 & vldNumStr2 == 1, swpAddrLn2,
                            ifelse(vldNonNA1 == 1 & vldNonNA2 == 1, swpAddrLn1,
                            ifelse(vldNonNA1 == 1 & vldNonNA2 == 0, swpAddrLn1,
                            ifelse(vldNonNA1 == 0 & vldNonNA2 == 1, swpAddrLn2,
                            ifelse(vldStTyp1 == 0 & vldStTyp2 == 0, swpAddrLn1,
                                   swpAddrLn2)))))))))))

### Add in State data for countries where this appears to matter
# Load in Lookup Table
LookupTableCountryCity2State_exUS <- read.csv("LookupTableCountryCitytoState_exUS.csv", stringsAsFactors = F)glimpse(LookupTableCountryCity2State_exUS)
# abbreviate the AUS states again
LookupTableCountryCity2State_exUS$lkpState %<>% sub("New South Wales","NSW", ., ignore.case = T) 
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Queensland","QLD", ., ignore.case = T) 
LookupTableCountryCity2State_exUS$lkpState %<>% sub("South Australia","SA", ., ignore.case = T) 
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Tasmania","TAS", ., ignore.case = T) 
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Victoria","VIC", ., ignore.case = T) 
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Western Australia","WA", ., ignore.case = T)
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Australian Capital Territory","ACT", ., ignore.case = T)
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Northern Territory","NT", ., ignore.case = T)
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Provincia di ","", ., ignore.case = T)
LookupTableCountryCity2State_exUS$lkpState %<>% sub("Citta Metropolitana di ","", ., ignore.case = T)
LookupTableCountryCity2State_exUS$lkpState %<>% sub("^$", NA, ., ignore.case = T)
# Load in the ITA postal provinces abbreviation table
PostalProvincias_ITA <- read.csv("PostalProvincia_ITA.csv", stringsAsFactors = F)
glimpse(PostalProvincias_ITA)
# Load in Chilean Regions
PostalRegions_CHL <- read.csv("PostalRegions_CHL.csv", stringsAsFactors = F)
glimpse(PostalRegions_CHL)

# join in to the Best table and then replace stdState whenever empty
# except in Italy where CAP happens and Metro will be used and states listed in database are most likely
# completely irrelevant to the postal data
# then drop in Chilean postal Region Data
## NB - Can't get Egypt Governorates in as the are subCity sized
BestClndAddrData_Final <-  
    BestClndAddrData %>% 
    left_join(LookupTableCountryCity2State_exUS)%>%
    left_join(PostalProvincias_ITA) %>% 
    # select(stdCountry, stdCity, stdState, lkpState, stdST, stdMetro) %>% 
    mutate(stdState = ifelse(stdCountry != "Italy", 
                                                    ifelse(!is.na(stdST), stdST,
                                                    ifelse(!is.na(lkpState), lkpState, 
                                                    stdState)),
                      stdState),
          stdCAP = ifelse(!is.na(stdST), stdST, stdMetro)
          ) %>% 
    select(-lkpState, -stdST, -stdMetro) %>% 
    left_join(PostalRegions_CHL)

save(BestClndAddrData_Final, file = "BestClndAddrData_Final")                                            
write.csv(BestClndAddrData_Final, "BestClndAddrData_Final.csv", row.names = F)
