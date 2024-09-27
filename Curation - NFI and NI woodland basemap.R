# Ewan McHenry

# woodland cover gis data curation
##------ Thu Nov 25 12:59:32 2021 ------##

# aim is to combine NFI and NI woodland register and basemap data and curate where possible

# libraries ----
library(tidyverse)
library(sf) # for gis
library(rgdal)
library(units) # for set_units()
library(U.utilities) # devtools::install_github("EwanMcHenry/U.utilities")
library(terra)
library(exactextractr)
library(compiler)

nfi.gb.wd <- "\\Data\\woodland cover\\downloaded data\\National Forest Inventory GB"
ni.wd <- "\\Data\\woodland cover\\downloaded data\\NI woodland basemap"

# load data ----
# nfigb2019 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2019\\National_Forest_Inventory_Woodland_GB_2019.shp"))
# nfigb2020 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2020\\National_Forest_Inventory_Woodland_GB_2020.shp"))
# nfigb2021 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2021\\National_Forest_Inventory_Woodland_GB_2021.shp"))
nfigb2022 <- st_read(paste0(gis.wd, nfi.gb.wd, "\\National_Forest_Inventory_Woodland_GB_2022\\National_Forest_Inventory_GB_2022.shp"))

lcmgb2020 <- rast(paste0(gis.wd, "\\Data\\LCM\\LCM2020\\25m land parcel\\gb2020lcm25m.tif"))

# configure ----
uncertain_woodland <- c("Assumed woodland", "Cloud \\ shadow", "Windblow", "Ground prep", "Felled", "Failed", "Uncertain", "Young trees", "OPEN GROUND", "NOT KNOWN") # types of "woodland" that neec checking

# explore ----
# # check all have same IFTs
# # Extract unique IFT_IOA values for each year and compare them
# ift_list <- lapply(list(nfigb2019, nfigb2020, nfigb2021, nfigb2022), function(x) unique(x$IFT_IOA))
# # Check if all sets are identical
# all(sapply(ift_list, function(x) identical(x, ift_list[[1]])))

## woodland / non and IFT ----
table(nfigb2022$IFT_IOA, nfigb2022$CATEGORY)
# all looks good and mutually exclusive

# curation nfi ----
# 
nfi_data <- ni2022
lcm_data <- lcmgb2020

do_ni_curation <- function(nfi_data, lcm_data) {
  ## nfi ha ----
nfi_data$nfi.ha <-  units::set_units(st_area(nfi_data), "ha")

nfi_data$og_IFT_IOA <- nfi_data$IFT_IOA
nfi_data$og_Categ <- nfi_data$CATEGORY
nfi_data$og_nfi.ha <- nfi_data$nfi.ha

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
nfi_data$IFT_IOA[ nfi_data$IFT_IOA == "Powerline" ] <- "Urban"


## Sort uncertain woodland, "assumed woodland" etc ----
uncertain_woodland_df <- nfi_data[ nfi_data$IFT_IOA %in% uncertain_woodland, ] %>% 
  arrange(desc(nfi.ha))

uncertain_checked_list <- vector("list", nrow(uncertain_woodland_df)) # List to collect results
non_woodland_list <- list()  # List to collect non-woodland polygons

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = nrow(uncertain_woodland_df), style = 3)

for (i in 1:nrow(uncertain_woodland_df)) {
  this.poly <- uncertain_woodland_df[i, ]
  #crop and mask lcm to this sf
  cropped_lcm <- terra::crop(lcm_data, this.poly)
  masked_lcm <- terra::mask(cropped_lcm, this.poly)

  # turn that cropped into polygons
  sf_polygons <- terra::as.polygons(masked_lcm, dissolve = TRUE) %>% 
    st_as_sf()
  colnames(sf_polygons)[1] = "IFT_IOA"
  sf_polygons$lcm <- sf_polygons$IFT_IOA
  
  # Classify polygons based on LCM values
  sf_polygons$IFT_IOA[!sf_polygons$IFT_IOA %in% 1:2] <- paste(this.poly$IFT_IOA, "- Not woodland")
  sf_polygons$IFT_IOA[sf_polygons$IFT_IOA==1] <- paste(this.poly$IFT_IOA, "- Confirmed Broadleaf")
  sf_polygons$IFT_IOA[sf_polygons$IFT_IOA==2] <- paste(this.poly$IFT_IOA, "- Confirmed Confier")
  
  sf_polygons$CATEGORY <- "Woodland" 
  sf_polygons$CATEGORY[sf_polygons$IFT_IOA == paste(this.poly$IFT_IOA, "- Not woodland")] <- "Non woodland" # overwrite non-woodland incorrectly assumed
  
  # Write original info from nfi
  sf_polygons$COUNTRY <- this.poly$COUNTRY
  sf_polygons$OBJECTID <- this.poly$OBJECTID
  # sf_polygons$nfi.ha <-   units::set_units(st_area(sf_polygons), "ha")
  
  # Separate non-woodland polygons for later union
  non_woodland_sf <- sf_polygons[sf_polygons$CATEGORY == "Non woodland",]
  
  if (nrow(non_woodland_sf) > 0) {
    non_woodland_list[[i]] <- non_woodland_sf
  }
  sf_polygons <- sf_polygons %>%
    filter(CATEGORY != "Non woodland")
  
  # Collect results in the list
  uncertain_checked_list[[i]] <- sf_polygons
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

close(pb)  # Close the progress bar

# Combine all results

output <- list(uncertain_checked = uncertain_checked_list, 
               non_woodland_list = non_woodland_list)

return(output)
}

compiled_do_nfi_curation <- cmpfun(do_nfi_curation)

# unlist the output components ----
# and rbind into a verison of the original nfi data with all the uncertain taken out
## function - batch rbind 
combine_sf_batches <- function(sf_list, batch_size = 1000) {
  # Calculate number of batches
  n_batches <- ceiling(length(sf_list) / batch_size)
  
  # Initialize list to store the combined results for each batch
  batch_list <- vector("list", n_batches)
  # Initialize the progress bar
  pb <- txtProgressBar(min = 0, max = n_batches, style = 3)
  
  for (i in 1:n_batches) {
    # Determine start and end indices for the current batch
    start_idx <- ((i - 1) * batch_size) + 1
    end_idx <- min(i * batch_size, length(sf_list))
    
    # Combine sf objects within the current batch and store the result
    batch_list[[i]] <- do.call(rbind, sf_list[start_idx:end_idx])
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
    
  }
  # Close the progress bar
  close(pb)
  
  return(batch_list)
}

# curate nfi -----
output <- compiled_do_nfi_curation(nfi_data = nfi_data, lcm_data)

nu.batch <- combine_sf_batches(output$uncertain_checked, batch_size = 100)
nu.batch2 <- combine_sf_batches(nu.batch, batch_size = 100)
nu.batch3 <- combine_sf_batches(nu.batch2, batch_size = 3)
uncertain_checked <- do.call(rbind, nu.batch3)

oth.batch <- combine_sf_batches(output$non_woodland_list, batch_size = 100)
oth.batch2 <- combine_sf_batches(oth.batch, batch_size = 100)
oth.batch3 <- combine_sf_batches(oth.batch2, batch_size = 3)
non_woodland_sf <- do.call(rbind, oth.batch3)

uncertain_reformed <- rbind(uncertain_checked, non_woodland_sf)

# add back into nfi
nfi_data <- nfigb2022 %>%
  filter(!OBJECTID %in% uncertain_reformed$OBJECTID) %>%
  bind_rows(uncertain_reformed)

# original IFT
nfi_data$og.IFT <- gsub(" - Confirmed Broadleaf| - Confirmed Confier| - Not woodland", "", nfi_data$IFT_IOA)

# add area
nfi_data$nfi.ha <-   units::set_units(st_area(nfi_data), "ha")

# to add to curation
# give a nu_ift for broadleaf and coniferous
nfi_data <- nfi_data %>% 
  mutate(nu_IFT = case_when(
    IFT_IOA %in% c( "Agriculture land", 
                    "Assumed woodland - Not woodland", 
                    "Bare area", 
                    "Cloud \\ shadow - Not woodland", 
                    "Failed - Not woodland", 
                    "Felled - Not woodland", "Grassland", "Ground prep - Not woodland", 
                    "Low density", "Open water", "Other vegetation", "Powerline", 
                    "Quarry", "River", "Road", "Urban", 
                    "Windblow - Not woodland", "Windfarm" ) ~ "Other",

    IFT_IOA %in% c("Young trees") ~ "Young trees",
    IFT_IOA %in% c("Uncertain") ~ "Uncertain",
    IFT_IOA %in% c("Coppice", "Coppice with standards") ~ "Coppice",
    IFT_IOA %in% c("Mixed", "Mixed mainly broadleaved", "Mixed mainly conifer") ~ "Mixed",
    
    IFT_IOA %in% c("Assumed woodland - Confirmed Broadleaf", 
                   "Broadleaved",
                   "Cloud \\ shadow - Confirmed Broadleaf", 
                   "Failed - Confirmed Broadleaf", 
                   "Felled - Confirmed Broadleaf", "Ground prep - Confirmed Broadleaf", 
                   "Low density", "Shrub", 
                   "Windblow - Confirmed Broadleaf") ~ "Broadleaved",
    
    IFT_IOA %in% c("Assumed woodland - Confirmed Confier", "Conifer", 
                   "Cloud \\ shadow - Confirmed Confier", "Failed - Confirmed Confier", 
                   "Felled - Confirmed Confier", "Ground prep - Confirmed Confier", 
                   "Windblow - Confirmed Confier") ~ "Coniferous",
    
    TRUE ~ "Other"
  ))

nfi_data$nu_IFT_2 <- nfi_data$nu_IFT
nfigb2022_curated <- nfi_data

save(nfigb2022_curated,uncertain_woodland,  file = "nfi_data_2022_curated.RData")




# curation NI woodland basemap  ----

ni2022 =  st_read(paste0(gis.wd, ni.wd,  "\\NIWoodlandBasemapV1_3\\NIWoodlandBasemapV1_3.shp")) %>% 
  st_transform(27700) # convert to OSGB

ni2022$COUNTRY <- "N. Ireland"
ni2022$og.IFT <- ni2022$TYPE
ni2022$og_nfi.ha <-   units::set_units(st_area(ni2022), "ha")
ni2022$CATEGORY <- "Woodland"
ni2022$CATEGORY[ni2022$TYPE %in% c("OPEN GROUND")] <- "Non woodland"

ni2022$nu_IFT <- case_when(
  ni2022$TYPE %in% c("BROADLEAF") ~ "Broadleaved",
  ni2022$TYPE %in% c("CONIFER") ~ "Conifer",
  ni2022$TYPE %in% c("MIXED CONIFER/BROADLEAF") ~ "Mixed",
  ni2022$TYPE %in% c("NOT KNOWN") ~ "Uncertain",
  ni2022$TYPE %in% c("OPEN GROUND") ~ "Other",
  ni2022$TYPE %in% c("REGENERATING") ~ "Young trees",
  ni2022$TYPE %in% c("SHORT ROTATION COPPICE (SRC)") ~ "Coppice",
  TRUE ~ "Other"
)
ni2022$nu_IFT_2 <- ni2022$nu_IFT
ni2022$nu_IFT_2[ni2022$nu_IFT %in% c("Broadleaved", "Coppice")] <- "Broadleaved"
ni2022$nu_IFT_2[ni2022$nu_IFT %in% c("Mixed")] <- "Mixed"
ni2022$nu_IFT_2[ni2022$nu_IFT %in% c("Conifer")] <- "Coniferous"

ni2022$nfi.ha <-   units::set_units(st_area(ni2022), "ha")

ni2022_curated <- ni2022
# save
save(ni2022_curated, file = "ni2022_curated.RData")


# combining NFI GB and woodland basemap ---- 

# Find columns in each dataset that the other is missing
missing_in_ni <- setdiff(names(nfigb2022_curated), names(ni2022_curated))
missing_in_nfigb <- setdiff(names(ni2022_curated), names(nfigb2022_curated))

# Add missing columns to ni2022_curated
ni2022_curated[missing_in_ni] <- NA

# Add missing columns to nfigb2022_curated
nfigb2022_curated[missing_in_nfigb] <- NA

# Combine the datasets
nfi_combined_2022 <- rbind(nfigb2022_curated, ni2022_curated)


# save
save(nfi_combined_2022, file = "nfi_combined_2022.RData")
