# Ewan McHenry

# woodland cover gis data curation
##------ Thu Nov 25 12:59:32 2021 ------##

# aim is to combine NFI and NI woodland register and basemap data and curate where possible

# libraries ----
library(tidyverse)
library(sf) # for gis
library(raster)
library(rgdal)
library(units) # for set_units()
library(U.utilities) # devtools::install_github("EwanMcHenry/U.utilities")

nfi.gb.wd <- "\\Data\\woodland cover\\downloaded data\\National Forest Inventory GB"

# load data ----
nfigb2019 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2019\\National_Forest_Inventory_Woodland_GB_2019.shp"))
nfigb2020 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2020\\National_Forest_Inventory_Woodland_GB_2020.shp"))
nfigb2021 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2021\\National_Forest_Inventory_Woodland_GB_2021.shp"))
nfigb2022 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2022\\National_Forest_Inventory_GB_2022.shp"))
  
ni2021 =  st_read(paste0(gis.wd, "\\Data\\woodland cover\\downloaded data\\NI woodland basemap\\NIWoodlandBasemapV1_3\\NIWoodlandBasemapV1_3.shp"))

lcmgb2020 <- raster::raster(paste0(gis.wd,"\\Data\\LCM\\LCM2020\\25m land parcel\\gb2020lcm25m.tif"))

# curation ----

## Power line inconsistency ----
# pl20 <- nfigb2020[ nfigb2020$IFT_IOA == "Powerline", ]
# pl21 <- nfigb2021[ nfigb2021$IFT_IOA == "Powerline", ]
# pl22 <- nfigb2022[ nfigb2022$IFT_IOA == "Powerline", ]
# 
# power_21_22 <- st_intersection(nfigb2021, nfigb2022[ nfigb2022$IFT_IOA == "Powerline", ]) 
# power_21_20 <- st_intersection(nfigb2021, nfigb2020[ nfigb2020$IFT_IOA == "Powerline", ]) 
# power_21_22 <- st_intersection(nfigb2021[ nfigb2021$IFT_IOA == "Powerline", ], nfigb2022[ nfigb2022$IFT_IOA == "Powerline", ])
# st_write(power_21_22, "scratch//power_22.shp")
# st_write(power_21_20, "scratch//power_20.shp")
# st_write(power_21_22, "scratch//power_21_22.shp")
# st_write(pl20, "scratch//pl2020.shp")
# st_write(pl22, "scratch//pl2022.shp")

# convert all powerline to Urban for consistencey
nfigb2019$IFT_IOA[ nfigb2019$IFT_IOA == "Powerline" ] <- "Urban"
nfigb2020$IFT_IOA[ nfigb2020$IFT_IOA == "Powerline" ] <- "Urban"
nfigb2021$IFT_IOA[ nfigb2021$IFT_IOA == "Powerline" ] <- "Urban"
nfigb2022$IFT_IOA[ nfigb2022$IFT_IOA == "Powerline" ] <- "Urban"

# check all have same IFTs
# Extract unique IFT_IOA values for each year and compare them
ift_list <- lapply(list(nfigb2019, nfigb2020, nfigb2021, nfigb2022), function(x) unique(x$IFT_IOA))
# Check if all sets are identical
all(sapply(ift_list, function(x) identical(x, ift_list[[1]])))

## woodland / non and IFT ----
table(nfigb2019$IFT_IOA, nfigb2019$CATEGORY)




# NI woodland basemap curation ----
# 16 th February 2021 - derived from a basemap consisting of forests and woodlands throughout Northern Ireland <= 0.1ha
# lots of issues with this dataset, so parkign for now

ni2021 <- ni2021 %>%
  dplyr::select(c("TYPE", "MANAGED_BY", "geometry" )) %>% 
  rename(type = TYPE) %>% 
  mutate(CATEGORY  = "Woodland") %>% 
  mutate(CATEGORY[CATEGORY == "OPEN GROUND"]  = "Non woodland",
         type[type = "BROADLEAF"] = "Broadleaved" , 
         type[type = "CONIFER"] = "Conifer" , 
         type[type = "MIXED CONIFER/BROADLEAF"] = "mixed" , 
         type[type = "NOT KNOWN"] = "Uncertain" , 
         type[type = "OPEN GROUND"] = "Bare area" , 
         type[type = "REGENERATING"] = "Young trees" , 
         type[type = "SHORT ROTATION COPPICE (SRC)"] = "Coppice"
  )


# combining NFI GB and woodland basemap ---- 

nfi19_ni21 = rbind( nfigb2019 %>% 
                dplyr::select(c("CATEGORY", "IFT_IOA", "geometry" )) %>%
                rename(type = IFT_IOA) %>% 
                mutate(MANAGED_BY = NA),
              ni2021 
              ) 
  
  

