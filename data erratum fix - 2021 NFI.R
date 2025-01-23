# replacing 2021 NFI data with revised version revied 2024-11-26 from Vera Correia following flag of bug where power lines not included

# load libraries
library(sf)
library(tidyverse)
library(U.utilities) # devtools::install_github("EwanMcHenry/U.utilities")

nfi.gb.wd <- "\\Data\\woodland cover\\downloaded data\\National Forest Inventory GB"

nfigb2021 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\NFI_GB_IFT_Data_2021 -Nov2024_Revised_IFT\\NFI_GB_IFT_Data_Nov2024_Revised_IFT.shp")) 

st_write(nfigb2021, paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2021\\National_Forest_Inventory_Woodland_GB_2021.shp"))