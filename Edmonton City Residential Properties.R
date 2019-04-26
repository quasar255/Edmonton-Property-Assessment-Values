# Call Packages

library(lubridate)
library(tidyverse)
library(xlsx)

# Read Source Data
X2014_18_Edmonton_Res_Assessment <- read_csv("~/resources/data/Edmonton City Property Assessments/2014-18_Edmonton_Res_Assessment.csv")

# Clean Data
## Remove spaces in Variable Names
names(X2014_18_Edmonton_Res_Assessment)<-str_replace_all(names(X2014_18_Edmonton_Res_Assessment), c(" " = "." , "," = "" ))

#Filter Data by Year to different files
Edm_Res_Assessment_2014 <- X2014_18_Edmonton_Res_Assessment %>% filter(Assessment.Year == 2014)
Edm_Res_Assessment_2015 <- X2014_18_Edmonton_Res_Assessment %>% filter(Assessment.Year == 2015)
Edm_Res_Assessment_2016 <- X2014_18_Edmonton_Res_Assessment %>% filter(Assessment.Year == 2016)
Edm_Res_Assessment_2017 <- X2014_18_Edmonton_Res_Assessment %>% filter(Assessment.Year == 2017)
Edm_Res_Assessment_2018 <- X2014_18_Edmonton_Res_Assessment %>% filter(Assessment.Year == 2018)

#Write/Download data to WD for use in Tableau - Had to split big file to small files in R due to size limits on Tableau student version
write.csv(Edm_Res_Assessment_2014, "Edm_Res_Assessment_2014.csv", row.names = FALSE)
write.csv(Edm_Res_Assessment_2014, "Edm_Res_Assessment_2015.csv", row.names = FALSE)
write.csv(Edm_Res_Assessment_2014, "Edm_Res_Assessment_2016.csv", row.names = FALSE)
write.csv(Edm_Res_Assessment_2014, "Edm_Res_Assessment_2017.csv", row.names = FALSE)
write.csv(Edm_Res_Assessment_2014, "Edm_Res_Assessment_2018.csv", row.names = FALSE)

#Convert 2018 Lot Size from double to Integer(Data Size reasons)
Edm_Res_Assessment_2018 <- Edm_Res_Assessment_2018 %>% mutate(Lot.Size, Lot.Size.Int = as.integer(Lot.Size))
X2014_18_Edmonton_Res_Assessment <- X2014_18_Edmonton_Res_Assessment %>% mutate(Lot.Size, Lot.Size.Int = as.integer(Lot.Size))

#Filter 2018 data to SF only
Edm_Res_Assessment_2018_SF <- Edm_Res_Assessment_2018 %>% filter(Zoning == "RF1" | Zoning == "RSL")
Edm_Res_Assessment_2018_SFHalfAcre <- Edm_Res_Assessment_2018_SF %>% filter(Lot.Size.Int < 1000  & Lot.Size.Int >100 & Actual.Year.Built==2017)

lmLotSize <- lm(Assessed.Value ~ Lot.Size.Int, data = Edm_Res_Assessment_2018_SFHalfAcre )
summary(lmLotSize)
ggplot(Edm_Res_Assessment_2018_SFHalfAcre, aes(Lot.Size.Int, Assessed.Value)) + geom_point(aes(color=Actual.Year.Built)) +labs(title = "Assessed Value  vs Lot Size", x = "Lot Size sq.m.", y = "Assessed Value $")

