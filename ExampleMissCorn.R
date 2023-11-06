
library(dplyr)
library(tidyr)
library(ggplot2)
library(gcamdata)

#GCAM SOC values, first csv
soc1 <- read.csv("C:/Users/morr497/OneDrive - PNNL/Desktop/GCAM Terrestrial C/beto_land_policy/beto_land_policy/R/soc.csv")
#GCAM SOC values, second csv
soc2 <- read.csv("C:/Users/morr497/OneDrive - PNNL/Desktop/GCAM Terrestrial C/beto_land_policy/beto_land_policy/R/soc_C_F.csv")
#need detailed_land_allocation from GCAM output
allocation <- read.csv("C:/Users/morr497/OneDrive - PNNL/Desktop/GCAM Terrestrial C/beto_land_policy/detailed_land_allocation/detailed_land_allocation.csv")



#get conservation agriculture values
soc1 %>%
  filter(grepl("MissppRS", LandLeaf)) %>%
  select(LandLeaf, soilTimeScale, soil.carbon.density) %>%
  separate(LandLeaf,into = c("Crop", "Basin", "Water", "Fert", "Till", "Cove")) %>%
  filter(grepl("CornC4", Crop),
         grepl("RFD", Water),
         grepl("lo", Fert),
         grepl("NonLgm", Cove)) %>%
  mutate(accum = soil.carbon.density/soilTimeScale) -> soc1_MissCorn

#get conventional/fallow values
soc2 %>%
  filter(grepl("MissppRS", LandLeaf)) %>%
  select(LandLeaf, soil.carbon.density) %>%
  separate(LandLeaf,into = c("Crop", "Basin", "Water", "Fert")) %>%
  filter(grepl("CornC4", Crop),
         grepl("RFD", Water),
         grepl("lo", Fert)) %>%
  mutate(soilTimeScale = 50, #previously, all US landleaves had STS of 50
         Till = "C",
         Cove = "F") -> soc2_MissCorn

#filter to USA agricultural land leafs used in study
allocation %>%
  filter(region == "USA",
         grepl("CornC4", landleaf) | grepl("FiberCrop", landleaf) |
           grepl("OtherGrainC4", landleaf) | grepl("Soy", landleaf) |
           grepl("Wheat", landleaf)) -> US_ag_alloc

#get land allocation for same leaves
US_ag_alloc %>%
  filter(grepl("MissppRS", landleaf)) %>%
  separate(landleaf,into = c("Crop", "Basin", "Water", "Fert", "Till", "Cove")) %>%
  filter(grepl("CornC4", Crop),
         grepl("RFD", Water),
         grepl("lo", Fert),
         grepl("NonLgm", Cove)) -> MissAlloc

#select small example, one scenario, only No-Till
MissAlloc %>%
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


#beginning of function


conversion_dynamics <- function(soiltimescale, soilcarbondensity,
                                allocation, year)
{

#this function creates a dataframe
#with the yearly change in soil carbon density
#for land area converted at each GCAM time step

##inputs:
#land allocation (as a time series)
#soil carbon density and timescale, single numbers

#outputs:
#dataframe with abs_year (x+year_lag)
#soil carbon density by year (y)
#land allocation (alloc_diff) for grouping var
#grouping var(convert_land)

#calculate area converted to that technology for each year
#pull out year for conversion
example %>%
  mutate(alloc_lag = lag(allocation, n = 1L),
         alloc_diff = allocation- alloc_lag,
         year_lag = lag(year, n = 1L)) -> example

#add groups for stacked plot
#convert_land groups are new increments of added acres under that land management
example$convert_land <- NA
N <- length(example$convert_land[example$value > 0])
example$convert_land[example$value > 0] <- LETTERS[1:N]

#sigmoid dataframe with soil time scale of 66 years
#and carbon density of 10.06
#check that max soil carbon density is acheived in the appropriate soil time scale
sgmd_df <- data.frame(ggbump::sigmoid(x_from = 0, x_to = soiltimescale,
                                      y_from = 0, y_to = soilcarbondensity,
                                      smooth = soilcarbondensity/2, #half of soil carbon density (10.06)
                                      n = soiltimescale,
                                      direction = "x"))
#create a common column to marge by
sgmd_df$mrg <- 1
example$mrg <- 1

#we want the entire ggbump::sigmoid output (the soil carbon accumulation curve)
#for each row (unique convert_land chunk) in select_example
sigmd_data <- left_join(example, sgmd_df, by = "mrg",
                        relationship = "many-to-many")
#compute absolute year from the start year of each convert_land chunk
sigmd_data$abs_year <- sigmd_data$year_lag + round(sigmd_data$x, 0)

#group by land chunks and fill in missing years out to 2100 with max C density
sigmd_data %>%
  filter(! is.na(abs_year),
         abs_year <= 2100) %>%
  group_by(convert_land) %>%
  complete(nesting(scenario, Crop, Basin, Fert, Till, Cove, alloc_diff),
           abs_year = max(abs_year):2100, 
           fill = list(y = soilcarbondensity)) -> SOC_accumulation

return(SOC_accumulation)
#end of function
}  

#y values will be year, x values will be x of sgmd_df x alloc from plot
ggplot(SOC_accumulation,
       aes(x=abs_year, y=y*alloc_diff,
           fill=forcats::fct_rev(convert_land))) +
  geom_area()

