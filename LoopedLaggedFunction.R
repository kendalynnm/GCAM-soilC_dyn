
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

#same for allocation, filter to USA agricultural land leafs used in study
allocation %>%
  filter(region == "USA",
         grepl("CornC4", landleaf) |
         grepl("FiberCrop", landleaf) |
         grepl("OtherGrainC4", landleaf) |
         grepl("Soy", landleaf) |
         grepl("Wheat", landleaf)) %>%
  separate(landleaf,into = c("Crop", "Basin", "Water", "Fert", "Till", "Cove")) %>%
  mutate(Till = ifelse(is.na(Till), "N", Till),
         Cove = ifelse(is.na(Cove), "F", Cove)) -> us_ag_alloc

us_ag_alloc %>%
  group_by(Basin, Crop) %>%
  filter(year == 1975) %>%
  select(-scenario, -Units, -region) -> base_year_alloc

#baseline soil carbon density
#from current inputs it appears
#that every river basin has the same SCD
#for all crops and technologies

 wide_conv %>%
  select("Crop", "Basin", "soil.carbon.density") %>%
  inner_join(base_year_alloc, by = c("Crop", "Basin"),
            relationship = "many-to-many") -> check
 #why does this have more rows than base_year_alloc????

###TO DO
#set baseline carbon density for each basin/crop
#figure out how to deal with decreasing allocation???
#perhaps mapping the trends for each crop in each basin
#might be possible just to sum across all????
#then combine into a big dataframe and split into many small
#feed this to the function




#select small example, one scenario, only No-Till
MissCornAlloc %>%
  group_by(scenario, Till) %>%
  left_join(soc1_MissCorn) %>%
  left_join(soc2_MissCorn) %>%
  filter(scenario == "Carbon_cropland",
         Till == "N") -> example

#give the function a dataframe
#with unique scenario, basin, crop, technology 


#lapply(list, conversion_dynamics)
#receive a list of dataframes generated split()
#split() a base R function that works like group_by()

# conversion_dynamics(soiltimescale = soiltimescale,
#                     soilcarbondensity = soilcarbondensity,
#                     allocation = value,
#                     year = year
#                    )

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
#convert_land groups are new increments of added acres under that land management
leaf_df$convert_land <- "NA"
N <- length(leaf_df$convert_land[leaf_df$alloc_diff > 0])
leaf_df$convert_land[leaf_df$alloc_diff > 0] <- LETTERS[1:N]

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
           fill = list(y = soilcarbondensity[1])) -> soilC_dyn

return(data.frame(soilC_dyn))

#outputs:
#dataframe with abs_year (x+year_lag)
#soil carbon density by year (y)
#land allocation (alloc_diff) for grouping var
#grouping var(convert_land)

#end of function
}  

soilC_dyn <- conversion_dynamics(soiltimescale = example$soilTimeScale,
                    soilcarbondensity = example$soil.carbon.density,
                    allocation = example$value,
                    year = example$year)

#plot dynamics over time
ggplot(soilC_dyn,
       aes(x=abs_year, y=y*alloc_diff,
           fill=forcats::fct_rev(convert_land))) +
  geom_area()

