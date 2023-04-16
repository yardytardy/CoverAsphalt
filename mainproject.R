library(tidyverse)
library(tidycensus)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tigris)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(units)

census_api_key("511be8d24286614f61800bdda6520c88c5e27594")

# get variable names
v10 <- load_variables(2010, "pl", cache = TRUE)
view(v10)
v20 <- load_variables(2020, "pl", cache = TRUE)
view(v20)

# find data for only treatment blocks
tracts <- tracts(state = "NY", county = "Onondaga", cb = TRUE)
treat_shape <- tracts[tracts$TRACTCE == "003200", ]

# find housing units for entire county
onon10 <- get_decennial(geography = "block", 
                                  variables = "H001001",
                                state = "NY", 
                                county = "Onondaga", 
                                year = 2010,
                                geometry = TRUE)
# isolate housing units only for treatment area
treatarea10 <- st_intersection(onon10, treat_shape)

# visualizing data for this area
treatplot10 <- treatarea10 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2010 Housing Units in Treatment Area")


# doing the same for 2020:
onon20 <- get_decennial(geography = "block", 
                        variables = "H1_001N",
                        state = "NY", 
                        county = "Onondaga", 
                        year = 2020,
                        geometry = TRUE)
treatarea20 <- st_intersection(onon20, treat_shape)
treatplot20 <- treatarea20 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2020 Housing Units in Treatment Area")

# putting both plots side by side
grid.arrange(treatplot10, treatplot20, ncol = 2)

# fix any geometric areas in the polygons
treatarea10 <- st_make_valid(treatarea10)
treatarea20 <- st_make_valid(treatarea20)

# calculate the area of each block
treatarea10 <- treatarea10 %>% 
  mutate(area = units::set_units(st_area(.), "km^2"))
treatarea20 <- treatarea20 %>% 
  mutate(area = units::set_units(st_area(.), "km^2"))

# calculate the density of each block and drop the units (the actual unit is housing unit per km^2)
treatarea10 <- treatarea10 %>%
  mutate(density = value / area)
treatarea10$density <- drop_units(treatarea10$density)
treatarea20 <- treatarea20 %>%
  mutate(density = value / area)
treatarea20$density <- drop_units(treatarea20$density)

# plotting density comparing 2010 to 2020
treatdensplot10 <- treatarea10 |>
  filter(!is.nan(density)) |>
  ggplot(aes(fill = density)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Housing Units per Square KM", 
                       limits = set_units(c(1, 60100))) +
  ggtitle("2010 Housing Density in Treatment Area")
treatdensplot20 <- treatarea20 |>
  filter(!is.nan(density)) |>
  ggplot(aes(fill = density)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Housing Units per Square KM", 
                       limits = set_units(c(1, 60100))) +
  ggtitle("2020 Housing Density in Treatment Area")
grid.arrange(treatdensplot10, treatdensplot20, ncol = 2)

# creating an ln variable for density
treatarea10 <- treatarea10 |>
  mutate(logdens = log(density))
treatarea20 <- treatarea20 |> 
  mutate(logdens = log(density))

# check values 
summary(treatarea10$logdens) 
summary(treatarea20$logdens)
# max is 16.35, so I will set upper limit as 17

# creating new plots
treatlnplot10 <- treatarea10 |>
  filter(!is.nan(logdens)) |>
  ggplot(aes(fill = logdens)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Log of Housing Units per Square KM", 
                       limits = set_units(c(1, 17))) +
  ggtitle("2010 Log of Housing Density in Treatment Area")
treatlnplot20 <- treatarea20 |>
  filter(!is.nan(logdens)) |>
  ggplot(aes(fill = logdens)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Log of Housing Units per Square KM", 
                       limits = set_units(c(1, 17))) +
  ggtitle("2020 Log of Housing Density in Treatment Area")
grid.arrange(treatlnplot10, treatlnplot20, ncol = 2)

# plot them all
grid.arrange(treatdensplot10, treatdensplot20, treatlnplot10, treatlnplot20, ncol = 2, nrow = 2)


# CONTROL UNITS

# find data for control units; trim out the treatment units and non-syracuse tracts
control_shape <- tracts[tracts$TRACTCE != "003200" &  tracts$TRACTCE != "016502" & tracts$TRACTCE != "013800" &
                              tracts$TRACTCE != "012900" & tracts$TRACTCE != "010301" & tracts$TRACTCE != "012600" &
                              tracts$TRACTCE != "016001" & tracts$TRACTCE != "010321" & tracts$TRACTCE != "015203" &
                              tracts$TRACTCE != "011021" & tracts$TRACTCE != "010322" & tracts$TRACTCE != "016002" &
                              tracts$TRACTCE != "011102" & tracts$TRACTCE != "016100" & tracts$TRACTCE != "016501" &
                              tracts$TRACTCE != "013500" & tracts$TRACTCE != "011231" & tracts$TRACTCE != "014200" &
                              tracts$TRACTCE != "011202" & tracts$TRACTCE != "011022" & tracts$TRACTCE != "016700" &
                              tracts$TRACTCE != "015201" & tracts$TRACTCE != "011201" & tracts$TRACTCE != "010600" &
                              tracts$TRACTCE != "016400" & tracts$TRACTCE != "012000" & tracts$TRACTCE != "013200" &
                              tracts$TRACTCE != "011012" & tracts$TRACTCE != "010400" & tracts$TRACTCE != "016802" &
                              tracts$TRACTCE != "010200" & tracts$TRACTCE != "013600" & tracts$TRACTCE != "011101" &
                              tracts$TRACTCE != "016300" & tracts$TRACTCE != "015100" & tracts$TRACTCE != "015602" &
                              tracts$TRACTCE != "012400" & tracts$TRACTCE != "014500" & tracts$TRACTCE != "011232" &
                              tracts$TRACTCE != "011800" & tracts$TRACTCE != "014800" & tracts$TRACTCE != "013400" &
                              tracts$TRACTCE != "013100" & tracts$TRACTCE != "010900" & tracts$TRACTCE != "010100" &
                              tracts$TRACTCE != "012700" & tracts$TRACTCE != "016901" & tracts$TRACTCE != "012300" &
                              tracts$TRACTCE != "016600" & tracts$TRACTCE != "011242" & tracts$TRACTCE != "011401" &
                              tracts$TRACTCE != "015702" & tracts$TRACTCE != "013000" & tracts$TRACTCE != "011241" &
                              tracts$TRACTCE != "016200" & tracts$TRACTCE != "010800" & tracts$TRACTCE != "014000" &
                              tracts$TRACTCE != "014300" & tracts$TRACTCE != "013701" & tracts$TRACTCE != "011500" &
                              tracts$TRACTCE != "015601" & tracts$TRACTCE != "012500" & tracts$TRACTCE != "013900" &
                              tracts$TRACTCE != "011900" & tracts$TRACTCE != "015400" & tracts$TRACTCE != "012100" &
                              tracts$TRACTCE != "013300" & tracts$TRACTCE != "015000" & tracts$TRACTCE != "011700" &
                              tracts$TRACTCE != "015202" & tracts$TRACTCE != "014400" & tracts$TRACTCE != "014900" &
                              tracts$TRACTCE != "014600" & tracts$TRACTCE != "940000" & tracts$TRACTCE != "010500" &
                              tracts$TRACTCE != "011600" & tracts$TRACTCE != "011011" & tracts$TRACTCE != "011401" &
                              tracts$TRACTCE != "015500" & tracts$TRACTCE != "012200" & tracts$TRACTCE != "016801" &
                              tracts$TRACTCE != "014700" & tracts$TRACTCE != "016902" & tracts$TRACTCE != "015800" &
                              tracts$TRACTCE != "011300" & tracts$TRACTCE != "012800" & tracts$TRACTCE != "015701" &
                              tracts$TRACTCE != "010700" & tracts$TRACTCE != "011402", ]

# simple plots of housing
controlarea10 <- st_intersection(onon10, control_shape)
controlplot10 <- controlarea10 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2010 Housing Units in Control Area")
controlarea20 <- st_intersection(onon20, control_shape)
controlplot20 <- controlarea20 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2020 Housing Units in Control Area")
grid.arrange(controlplot10, controlplot20, ncol = 2)

controlarea10 <- st_make_valid(controlarea10)
controlarea20 <- st_make_valid(controlarea20)

# doing density
controlarea10 <- controlarea10 %>% 
  mutate(area = units::set_units(st_area(.), "km^2"))
controlarea20 <- controlarea20 %>% 
  mutate(area = units::set_units(st_area(.), "km^2"))

controlarea10 <- controlarea10 %>%
  mutate(density = value / area)
controlarea10$density <- drop_units(controlarea10$density)
controlarea20 <- controlarea20 %>%
  mutate(density = value / area)
controlarea20$density <- drop_units(controlarea20$density)

# basic density plots 
controldensplot10 <- controlarea10 |>
  filter(!is.nan(density)) |>
  ggplot(aes(fill = density)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Housing Units per Square KM", 
                       limits = set_units(c(1, 10000))) +
  ggtitle("2010 Housing Density in Control Area")
controldensplot20 <- controlarea20 |>
  filter(!is.nan(density)) |>
  ggplot(aes(fill = density)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Housing Units per Square KM", 
                       limits = set_units(c(1, 10000))) +
  ggtitle("2020 Housing Density in Control Area")
grid.arrange(controldensplot10, controldensplot20, ncol = 2)

# doing log density
controlarea10 <- controlarea10 |>
  mutate(logdens = log(density))
controlarea10 <- controlarea10 |>
  filter(!is.nan(logdens))
controlarea20 <- controlarea20 |> 
  mutate(logdens = log(density))
controlarea20 <- controlarea20 |>
  filter(!is.nan(logdens))

controllnplot10 <- controlarea10 |>
  ggplot(aes(fill = logdens)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Log of Housing Units per Square KM", 
                       limits = set_units(c(1, 17))) +
  ggtitle("2010 Log of Housing Density in Control Area")
controllnplot20 <- controlarea20 |>
  ggplot(aes(fill = logdens)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Log of Housing Units per Square KM", 
                       limits = set_units(c(1, 17))) +
  ggtitle("2020 Log of Housing Density in Control Area")
grid.arrange(controllnplot10, controllnplot20, ncol = 2)

grid.arrange(controldensplot10, controldensplot20, controllnplot10, controllnplot20, ncol = 2, nrow = 2)

# trimming infinite values
controlarea10 <- controlarea10 |>
  mutate(validdensity = case_when(is.na(density) ~ NA_real_,
                                  is.finite(density) ~ density,
                                  TRUE ~ NA_real_))
controlarea20 <- controlarea20 |>
  mutate(validdensity = case_when(is.na(density) ~ NA_real_,
                                  is.finite(density) ~ density,
                                  TRUE ~ NA_real_))
treatarea10 <- treatarea10 |>
  mutate(validdensity = case_when(is.na(density) ~ NA_real_,
                                  is.finite(density) ~ density,
                                  TRUE ~ NA_real_))
treatarea20 <- treatarea20 |>
  mutate(validdensity = case_when(is.na(density) ~ NA_real_,
                                  is.finite(density) ~ density,
                                  TRUE ~ NA_real_))




# time to do Albany (the second control group)

