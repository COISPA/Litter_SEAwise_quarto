rm(list=ls(all=TRUE))
library(BioIndex)
library(lubridate)
library(dplyr)

# set time series extension
years <- c(2013:2024)
wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\SEAwise\\_____ARTICOLO_LITTER\\___Analysis_2025___\\Litter_SEAwise_quarto"
setwd(wd)
# set results directory
resdir <- file.path(wd,"input")

# Read data ----------------
# read TA file
ta <- read.table(file.path(wd,"data","TA_GSA18_1994-2024.csv"),sep=";", header=TRUE)
# read TL file
tl <- read.table(file.path(wd,"data","TL_GSA18_2012-2024.csv"),sep=";", header=TRUE)
#--------------------------

## TA

# filter TA for the selected time series
ta <- ta[ta$YEAR %in% years,]
# transform TA coordinates (in MEDITS format) to decimal degrees
ta <- BioIndex::MEDITS.to.dd(ta)
# estimate mean hauls' oordinates
ta$x <- rowMeans(ta[, which(colnames(ta) %in% c("SHOOTING_LONGITUDE", "HAULING_LONGITUDE"))])
ta$y <- rowMeans(ta[, which(colnames(ta) %in% c("SHOOTING_LATITUDE", "HAULING_LATITUDE"))])
# estimate haul swept area
ta$swept <- (ta$DISTANCE/1000)*(ta$WING_OPENING/10000)
#estimate mean depth per haul
ta$depth <- rowMeans(cbind(ta$HAULING_DEPTH,ta$SHOOTING_DEPTH))
# create a unique haul id
id <- paste(ta$AREA,ta$COUNTRY,ta$YEAR,"_", ta$VESSEL, ta$MONTH, ta$DAY,"_", ta$HAUL_NUMBER, sep = "")
ta <- cbind(id,ta)
# create continuous time variable (ctime)
date <- paste(ta$YEAR,ta$MONTH,ta$DAY,sep="-")
date <- yday(date)/365
ta$yday <- date
ta$ctime <- rowSums(ta[,which(colnames(ta) %in% c("YEAR","yday"))])
# colnames(ta)[which(colnames(ta)=="lon")] <- "x"
# colnames(ta)[which(colnames(ta)=="lat")] <- "y"

#--------------------------

## TL


# create a unique haul id
id <- paste(tl$AREA,tl$COUNTRY,tl$YEAR,"_", tl$VESSEL, tl$MONTH, tl$DAY,"_", tl$HAUL_NUMBER, sep = "")
tl <- cbind(id,tl)
# filter TL for the selected time series
tl <- tl[tl$YEAR %in% years,]

## TL post-classification
# SUB-CATEGORY MEAN WEIGHT
tl$MEAN_WEIGHT_SUBCATEGORY <- tl$TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY/tl$TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY
tl[is.na(tl$MEAN_WEIGHT_SUBCATEGORY),"MEAN_WEIGHT_SUBCATEGORY"] <- 0

# SUP
tl$SUP <- paste0(toupper(tl$LITTER_CATEGORY),toupper(tl$LITTER_SUB.CATEGORY))
tl[tl$SUP %in% c("L1A","L1B","L1C","L1D","L1E","L1J","L2B"),"SUP"] <- "YES"
tl[tl$SUP != "YES","SUP"] <- "NO"

# FR
tl$Fishing.related <- paste0(toupper(tl$LITTER_CATEGORY),toupper(tl$LITTER_SUB.CATEGORY))
tl[tl$Fishing.related %in% c("L1F","L1G","L1H","L3F","L5C"),"Fishing.related"] <- "YES"
tl[tl$Fishing.related != "YES","Fishing.related"] <- "NO"

# Entanglement
tl$Entanglement <- paste0(toupper(tl$LITTER_CATEGORY),toupper(tl$LITTER_SUB.CATEGORY))
tl[tl$Entanglement %in% c("L1F","L1G"),"Entanglement"] <- "YES"
tl[tl$Entanglement != "YES","Entanglement"] <- "NO"

# Ingestion.fish.and.birds
tl$Ingestion.fish.and.birds <- paste0(toupper(tl$LITTER_CATEGORY),toupper(tl$LITTER_SUB.CATEGORY))
tl[(tl$MEAN_WEIGHT_SUBCATEGORY >0 & tl$MEAN_WEIGHT_SUBCATEGORY < 5) & (tl$Ingestion.fish.and.birds %in% c("L1A","L1C","L1D","L1I","L5C")) ,"Ingestion.fish.and.birds"] <- "YES"
tl[tl$Ingestion.fish.and.birds != "YES","Ingestion.fish.and.birds"] <- "NO"

# Ingestion.sharks.and.cetaceans
tl$Ingestion.sharks.and.cetaceans <- paste0(toupper(tl$LITTER_CATEGORY),toupper(tl$LITTER_SUB.CATEGORY))
tl[(tl$MEAN_WEIGHT_SUBCATEGORY >0 & tl$MEAN_WEIGHT_SUBCATEGORY < 20) & (tl$Ingestion.sharks.and.cetaceans %in% c("L1A","L1C","L1D","L1I","L5C")),"Ingestion.sharks.and.cetaceans"] <- "YES"
tl[tl$Ingestion.sharks.and.cetaceans != "YES","Ingestion.sharks.and.cetaceans"] <- "NO"

# Advection
tl$Advection <- paste0(toupper(tl$LITTER_CATEGORY),toupper(tl$LITTER_SUB.CATEGORY))
tl[tl$Advection %in% c("L1A","L1C","L1D"),"Advection"] <- "YES"
tl[tl$Advection != "YES","Advection"] <- "NO"

write.table(tl,file.path(resdir,"TL_post-classified.csv"),sep=";",row.names = FALSE)
#--------------------------

### Create a data frame for each litter category
## SUP ------------------------------
tl_SUP <- tl[tl$SUP =="YES",]
tl_SUP <- tl_SUP %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dSUP <- merge(ta,tl_SUP,id="id",all.x=TRUE)
dSUP[is.na(dSUP$n),"n"] <- 0
dSUP[is.na(dSUP$kg),"kg"] <- 0

colnames(dSUP)<- tolower(colnames(dSUP))
dSUP$category <- "SUP"
dSUP$haul_duration <- dSUP$haul_duration/60
dSUP <- dSUP %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dSUP,file.path(resdir,"SUP_data.csv"),sep=";",row.names = FALSE)

# FR ------------------------------
tl_FR <- tl[tl$Fishing.related=="YES",]
tl_FR <- tl_FR %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dFR <- merge(ta,tl_FR,id="id",all.x=TRUE)
dFR[is.na(dFR$n),"n"] <- 0
dFR[is.na(dFR$kg),"kg"] <- 0

colnames(dFR)<- tolower(colnames(dFR))
dFR$category <- "FR"
dFR$haul_duration <- dFR$haul_duration/60
dFR <- dFR %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dFR,file.path(resdir,"FR_data.csv"),sep=";",row.names = FALSE)


# ENT ------------------------------
tl_ENT <- tl[tl$Entanglement=="YES",]
tl_ENT <- tl_ENT %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dENT <- merge(ta,tl_ENT,id="id",all.x=TRUE)
dENT[is.na(dENT$n),"n"] <- 0
dENT[is.na(dENT$kg),"kg"] <- 0

colnames(dENT)<- tolower(colnames(dENT))
dENT$category <- "ENT"
dENT$haul_duration <- dENT$haul_duration/60
dENT <- dENT %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dENT,file.path(resdir,"ENT_data.csv"),sep=";",row.names = FALSE)

# ING_FB ------------------------------
tl_ING_FB <- tl[tl$Ingestion.fish.and.birds=="YES",]
tl_ING_FB <- tl_ING_FB %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dING_FB <- merge(ta,tl_ING_FB,id="id",all.x=TRUE)
dING_FB[is.na(dING_FB$n),"n"] <- 0
dING_FB[is.na(dING_FB$kg),"kg"] <- 0

colnames(dING_FB)<- tolower(colnames(dING_FB))
dING_FB$category <- "INGFB"
dING_FB$haul_duration <- dING_FB$haul_duration/60
dING_FB <- dING_FB %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dING_FB,file.path(resdir,"INGFB_data.csv"),sep=";",row.names = FALSE)

# ING_SC ------------------------------
tl_ING_SC <- tl[tl$Ingestion.sharks.and.cetaceans=="YES",]
tl_ING_SC <- tl_ING_SC %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dING_SC <- merge(ta,tl_ING_SC,id="id",all.x=TRUE)
dING_SC[is.na(dING_SC$n),"n"] <- 0
dING_SC[is.na(dING_SC$kg),"kg"] <- 0

colnames(dING_SC)<- tolower(colnames(dING_SC))
dING_SC$category <- "INGSC"
dING_SC$haul_duration <- dING_SC$haul_duration/60
dING_SC <- dING_SC %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dING_SC,file.path(resdir,"INGSC_data.csv"),sep=";",row.names = FALSE)

# ADV ------------------------------
tl_ADV <- tl[tl$Advection=="YES",]
tl_ADV <- tl_ADV %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dADV <- merge(ta,tl_ADV,id="id",all.x=TRUE)
dADV[is.na(dADV$n),"n"] <- 0
dADV[is.na(dADV$kg),"kg"] <- 0

colnames(dADV)<- tolower(colnames(dADV))
dADV$category <- "ADV"
dADV$haul_duration <- dADV$haul_duration/60
dADV <- dADV %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dADV,file.path(resdir,"ADV_data.csv"),sep=";",row.names = FALSE)

# Plastic (L1) ------------------------------
tl_L1 <- tl[tl$LITTER_CATEGORY=="L1",]
tl_L1 <- tl_L1 %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dL1 <- merge(ta,tl_L1,id="id",all.x=TRUE)
dL1[is.na(dL1$n),"n"] <- 0
dL1[is.na(dL1$kg),"kg"] <- 0

colnames(dL1)<- tolower(colnames(dL1))
dL1$category <- "Plastic"
dL1$haul_duration <- dL1$haul_duration/60
dL1 <- dL1 %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dL1,file.path(resdir,"Plastic_data.csv"),sep=";",row.names = FALSE)

# Rubber (L2) ------------------------------
tl_L2 <- tl[tl$LITTER_CATEGORY=="L2",]
tl_L2 <- tl_L2 %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dL2 <- merge(ta,tl_L2,id="id",all.x=TRUE)
dL2[is.na(dL2$n),"n"] <- 0
dL2[is.na(dL2$kg),"kg"] <- 0

colnames(dL2)<- tolower(colnames(dL2))
dL2$category <- "Rubber"
dL2$haul_duration <- dL2$haul_duration/60
dL2 <- dL2 %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dL2,file.path(resdir,"Rubber_data.csv"),sep=";",row.names = FALSE)

# METAL (L3) ------------------------------
tl_L3 <- tl[tl$LITTER_CATEGORY=="L3",]
tl_L3 <- tl_L3 %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dL3 <- merge(ta,tl_L3,id="id",all.x=TRUE)
dL3[is.na(dL3$n),"n"] <- 0
dL3[is.na(dL3$kg),"kg"] <- 0

colnames(dL3)<- tolower(colnames(dL3))
dL3$category <- "Metal"
dL3$haul_duration <- dL3$haul_duration/60
dL3 <- dL3 %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dL3,file.path(resdir,"Metal_data.csv"),sep=";",row.names = FALSE)

# GLASS (L4) ------------------------------
tl_L4 <- tl[tl$LITTER_CATEGORY=="L4",]
tl_L4 <- tl_L4 %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dL4 <- merge(ta,tl_L4,id="id",all.x=TRUE)
dL4[is.na(dL4$n),"n"] <- 0
dL4[is.na(dL4$kg),"kg"] <- 0

colnames(dL4)<- tolower(colnames(dL4))
dL4$category <- "Glass"
dL4$haul_duration <- dL4$haul_duration/60
dL4 <- dL4 %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dL4,file.path(resdir,"Glass_data.csv"),sep=";",row.names = FALSE)

# NATURAL (L5) ------------------------------
tl_L5 <- tl[tl$LITTER_CATEGORY=="L5",]
tl_L5 <- tl_L5 %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dL5 <- merge(ta,tl_L5,id="id",all.x=TRUE)
dL5[is.na(dL5$n),"n"] <- 0
dL5[is.na(dL5$kg),"kg"] <- 0

colnames(dL5)<- tolower(colnames(dL5))
dL5$category <- "Natural"
dL5$haul_duration <- dL5$haul_duration/60
dL5 <- dL5 %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dL5,file.path(resdir,"Natural_data.csv"),sep=";",row.names = FALSE)

# OTHER (L8) ------------------------------
tl_L8 <- tl[tl$LITTER_CATEGORY=="L8",]
tl_L8 <- tl_L8 %>% group_by(id) %>% summarise(n = sum(TOTAL_NUMBER_IN_THE_HAUL_SUB.CATEGORY, na.rm=TRUE),kg=sum(TOTAL_WEIGHT_IN_THE_HAUL_SUB.CATEGORY,na.rm=TRUE)/1000)
dL8 <- merge(ta,tl_L8,id="id",all.x=TRUE)
dL8[is.na(dL8$n),"n"] <- 0
dL8[is.na(dL8$kg),"kg"] <- 0

colnames(dL8)<- tolower(colnames(dL8))
dL8$category <- "Other"
dL8$haul_duration <- dL8$haul_duration/60
dL8 <- dL8 %>% select(id, area, country, year, month, day, depth, x, y, haul_duration, swept, category, n, kg) %>% rename(duration=haul_duration)
write.table(dL8,file.path(resdir,"Other_data.csv"),sep=";",row.names = FALSE)

