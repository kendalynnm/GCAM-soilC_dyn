
library(dplyr)
library(tidyr)
library(ggplot2)
library(gcamdata)

#input files from Weber et. al. in prep
#GCAM conservation agriculture values 
CA <- read.csv("inputs/soc.csv")
#GCAM conventional values
Conventional <- read.csv("inputs/soc_C_F.csv")
#Detailed land_allocation from GCAM output
allocation <- read.csv("inputs/detailed_land_allocation.csv")


#get conservation agriculture soil carbon density and soil time scale values
CA %>%
  select(LandLeaf, soilTimeScale, soil.carbon.density) %>%
  #separate Landleaf into individual identifying columns
  separate(LandLeaf,into = c("Crop", "Basin", "Water", "Fert", "Till", "Cove")) %>%
  group_by(Crop, Basin, Water, Fert, Till, Cove)-> wide_CA

#get conventional/fallow soil carbon density and soil time scale values
#first filter to USA agricultural land leafs used in study
Conventional %>%
  filter(region == "USA",
         grepl("CornC4", LandLeaf) |
           grepl("FiberCrop", LandLeaf) |
           grepl("OtherGrainC4", LandLeaf) |
           grepl("Soy", LandLeaf) |
           grepl("Wheat", LandLeaf)) %>%
  select(LandLeaf, soil.carbon.density) %>%
  separate(LandLeaf,into = c("Crop", "Basin", "Water", "Fert")) %>%
  mutate(soilTimeScale = 50, # in core GCAM all US landleaves had STS of 50
         Till = "C", #core GCAM does not have tillage or cover crops
         Cove = "F") -> wide_conv

#combine to create one dataframe with all soil C density and soil timescale data
soil_data <- bind_rows(wide_conv, wide_CA)

#same for allocation, filter to USA agricultural land leafs used in study
allocation %>%
  filter(region == "USA",
         grepl("CornC4", landleaf) |
         grepl("FiberCrop", landleaf) |
         grepl("OtherGrainC4", landleaf) |
         grepl("Soy", landleaf) |
         grepl("Wheat", landleaf)) %>%
  separate(landleaf,into = c("Crop", "Basin", "Water", "Fert", "Till", "Cove")) %>%
  mutate(Till = ifelse(is.na(Till), "C", Till),
         Cove = ifelse(is.na(Cove), "F", Cove)) -> us_ag_alloc

#find base year (1975) land allocation for all scenarios
us_ag_alloc %>%
  group_by(Basin, Crop) %>%
  filter(year == 1975) %>%
  select(-Units, -region) %>%
  relocate(Basin, Crop, Water, Fert, Till, Cove, value, year, scenario) %>%
  arrange(value)-> base_year_alloc


#find base year soil carbon density for all scenarios

#from current inputs it appears
#that every river basin has the same SCD
#for all crops and technologies
 wide_conv %>%
  select("Crop", "Basin", "soil.carbon.density") %>%
  distinct() %>%
  left_join(base_year_alloc, by = c("Crop", "Basin")) -> base_year_alloc_soilC
 
 us_ag_alloc %>%
   left_join(soil_data) %>%
   mutate(thous_sq_km = value) %>%
   select(-c(Units, value)) %>%
   group_by(scenario, Basin, Water, Fert, Till, Cove)-> combined_data
 
chunks <- split(combined_data,
                f = list(combined_data$scenario,
                         combined_data$Basin,
                         combined_data$Crop,
                         combined_data$Water,
                         combined_data$Fert,
                         combined_data$Till,
                         combined_data$Cove))

# TO DO
#figure out how to deal with decreasing allocation
 
#perhaps mapping the trends for each crop in each basin
#might be possible just to sum across all????

 
#select example
combined_data %>%
    filter(Basin == "TennR",
           Crop == "Soybean") -> examples

examples %>%
  filter(scenario == "Reference",
         Water == "RFD",
         Fert == "lo") -> TennR_RFD_lo

#give the function a dataframe
#with unique scenario, basin, crop, technology 


#lapply(list, conversion_dynamics)
#receive a list of dataframes generated split()
#split() a base R function that works like group_by()

#this function creates a dataframe
#with the yearly change in soil carbon density
#for land area converted at each GCAM time step

#beginning of function
conversion_dynamics <- function(soiltimescale, soilcarbondensity,
                                allocation, year) {
##inputs:
#land allocation (as a time series)
#soil carbon density and timescale, single numbers

#calculate area converted to that technology for each year
#pull out year for conversion
alloc_lag <- lag(allocation, n = 1L)
alloc_diff <- allocation- alloc_lag
year_lag = lag(year, n = 1L)

leaf_df <- bind_cols(alloc_lag = alloc_lag,
                     alloc_diff = alloc_diff,
                     year_lag = year_lag)
leaf_df <- na.omit(leaf_df)

#add groups for stacked plot
#convert_land groups are new increments of added thousands of square kilometers
#under that land management
leaf_df$convert_land <- "NA"
N <- length(leaf_df$convert_land[leaf_df$alloc_diff != 0])
leaf_df$convert_land[leaf_df$alloc_diff != 0] <- LETTERS[1:N]

#sigmoid dataframe with soil time scale of 66 years
#and carbon density of 10.06
#check that max soil carbon density is acheived in the appropriate soil time scale
sgmd_df <- data.frame(ggbump::sigmoid(x_from = 0, x_to = soiltimescale[1],
                                      y_from = 0, y_to = soilcarbondensity[1],
                                      smooth = soilcarbondensity[1]/2, #half of soil carbon density (10.06)
                                      n = soiltimescale[1],
                                      direction = "x"))
#create a common column to marge by
sgmd_df$mrg <- 1
leaf_df$mrg <- 1

#we want the entire ggbump::sigmoid output (the soil carbon accumulation curve)
#for each row (unique convert_land chunk) in select_example
accum_data <- left_join(leaf_df, sgmd_df, by = "mrg",
                        relationship = "many-to-many")
#compute absolute year from the start year of each convert_land chunk
accum_data$abs_year <- accum_data$year_lag + round(accum_data$x, 0)

#group by land chunks and fill in missing years out to 2100 with max C density
accum_data %>%
  filter(! is.na(abs_year),
         abs_year <= 2100) %>%
  group_by(convert_land) %>%
  complete(nesting(alloc_diff),
           abs_year = max(abs_year):2100, 
           fill = list(y = soilcarbondensity[1])) -> accum_dyn

# accum_dyn %>%
# mutate(soil_C = y*alloc_diff) %>%
#   select(abs_year, soil_C, convert_land) %>%
#   pivot_wider(values_from = soil_C, names_from = convert_land) -> soilC_dyn

return(data.frame(accum_dyn))
# 
# if (transmute == TRUE) {
#   
#   soilC_dyn %>%
#     transmute(abs_year,
#               change_c = rowSums(select(., -abs_year),
#                                  na.rm = TRUE)) -> transmuted_data
#   return(data.frame(transmuated_data))
#   #outputs:
#   #dataframe with abs_year and 
#   #sum of soil C (change_c) across all land conversion chunks
#   
# }
# 
# else {
# return(data.frame(soilC_dyn))
#   
#   #outputs:
#   #dataframe with abs_year (x+year_lag)
#   #soil carbon density by year (y)
#   #land allocation (alloc_diff) for grouping var
#   #grouping var(convert_land)
#   
# }


#end of function
}  

# 
# lapply(chunks, FUN = conversion_dynamics(soiltimescale = soilTimeScale,
#                                          soilcarbondensity = soil.carbon.density))


TennR_RFD_lo_out <- conversion_dynamics(soiltimescale = TennR_RFD_lo$soilTimeScale,
                                    soilcarbondensity = TennR_RFD_lo$soil.carbon.density,
                                    allocation = TennR_RFD_lo$thous_sq_km,
                                    year = TennR_RFD_lo$year)



#plot dynamics over time
ggplot(TennR_RFD_lo_out,
       aes(x=abs_year, y=y*alloc_diff,
           fill=forcats::fct_rev(convert_land))) +
    geom_area() + ggtitle("TennR Soybean Reference Conventional RFD-lo")

ggplot(test2,
       aes(x=abs_year, y=change_c)) +
  geom_area()
