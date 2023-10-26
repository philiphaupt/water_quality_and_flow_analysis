# read water quality
library(tidyverse)
library(data.table)

wq_dat <- read_csv("./data/water_quality/KSL-2022.csv")

# water types (includes inland - filter to keep only estuarine and marine)
wq_dat %>% filter(sample.sampledMaterialType.label)

# lets see where data is collected
wq_dat %>% 
  dplyr::distinct(sample.samplingPoint.label, sample.samplingPoint.easting, sample.samplingPoint.northing) %>% 
  dplyr::filter(sample.samplingPoint.easting > 600000, sample.samplingPoint.northing > 165000) %>% 
ggplot()+
  geom_point(aes(x = sample.samplingPoint.easting, y = sample.samplingPoint.northing))

wq_places <- wq_dat %>% dplyr::distinct(sample.samplingPoint.label)


# Sheppey
wq_sheppey <- wq_dat %>% filter(sample.samplingPoint.label == "SHEPPEY-SHELLFISH WATER")

# Make date time column plot curves for each chemical - find out which ones are dangerous

# LOOK AT SPILL DATA, LOOK AT FLOW DATA - SEE IF THERE ARE CORELATIONS BETWEEN TIMES OF POOR CATCHES AND OUTFLOED
wq_sheppey %>% 
  select(sample.sampleDateTime, determinand.label, result) %>% 
  filter(!determinand.label %in% c("NGR Easting", "NGR Northing")) %>% 
  filter(determinand.label %in% c("N Dis Inorg")) %>% #N Dis Inorg #Temp Water
  ggplot()+
  geom_point(aes(x = sample.sampleDateTime, y = result))+
  geom_smooth(aes(x = sample.sampleDateTime, y = result))+
  facet_wrap(~determinand.label, scales = "free")+
  theme(axis.text=element_text(size=15))
# highest values
wq_sheppey %>% 
  select(sample.sampleDateTime, determinand.label, result) %>% 
  filter(!determinand.label %in% c("NGR Easting", "NGR Northing")) %>% 
  filter(determinand.label %in% c("N Dis Inorg")) %>%
  arrange((`determinand.label`))


#NH3 = 0.0625 mg/l top value
# = 6.25e-05 g/l which is in solution
# NH3 = 17.03 g/mol
# which means , molar mass is:
# 17.03*6.25e-05
# = 0.001064375 moles/litre, which is
# = 1.064375 mmol /l, (*1000) which is below the 12 mmol/l category considered good by WFD

# N Dis Inorg e.g. NO3 (~62g/mol)
# = 1.0 mg/l in solution
# = 0.001 g/l
# = 62*0.001*1000 = 62, which is far above 40.5, whichis considered bad! in WFD 

#according to EA:
# use atomic mass 14g/mol - N.
# = 1.0 mg/l in solution
# = 0.001 g/l
# = 14*0.001*1000 = 14, so between high quality and good quality and which is far above 40.5, whichis considered bad! in WFD 


# Make spatial
wq_dat_sf <- wq_dat %>% sf::st_sf(coordinates)