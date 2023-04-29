# install.packages(c("tidyverse", "tidycensus", "sf", "terra", "spData", "spDataLarge", "tigris", "ggplot2", "gridExtra", "dplyr", "units", "ggdag"))
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
library(ggdag)
library(MatchIt)
library(lfe)
library(estimatr)
library(optmatch)
library(modelsummary)
library(flextable)


census_api_key("511be8d24286614f61800bdda6520c88c5e27594")


# let's make a DAG
coord_dag <- list(
  x = c(PM = 0, BC = 1, HD = 2, CP = 1),
  y = c(PM = 1, BC = 1.75, HD = 1, CP = 0.25)
)

maindag <- ggdag::dagify(BC ~ PM,
                         HD ~ BC, 
                         PM ~ CP,
                         HD ~ PM,
                         HD ~ CP,
                         labels = c("PM" = "Parking\n Minimums",
                                    "BC" = "Building\n Costs",
                                    "HD" = "Housing\n Density",
                                    "CP" = "Community\n Politics"),
                         coords = coord_dag) 

ggdag::ggdag(maindag,
             text = FALSE,
             use_labels = "label",
             label_col = "maroon",
             node_size = 18) + 
  theme_void() + 
  ggtitle("Directed Acyclic Graph of Variables") 


# get variable names
v10 <- load_variables(2010, "pl", cache = TRUE)
v20 <- load_variables(2020, "pl", cache = TRUE)

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


# doing the same for 2020:
onon20 <- get_decennial(geography = "block", 
                        variables = "H1_001N",
                        state = "NY", 
                        county = "Onondaga", 
                        year = 2020,
                        geometry = TRUE)
treatarea20 <- st_intersection(onon20, treat_shape)

# fix any geometric areas in the polygons
treatarea10 <- st_make_valid(treatarea10)
treatarea20 <- st_make_valid(treatarea20)

# make the name for the variable name the same 
treatarea10 <- treatarea10 |>
  mutate(varname = case_when(variable == "H001001" ~ "housing units"))
treatarea20 <- treatarea20 |>
  mutate(varname = case_when(variable == "H1_001N" ~ "housing units"))

# add a year component 
treatarea10 <- treatarea10 |>
  mutate(year = case_when(value > -1 ~ 2010))
treatarea20 <- treatarea20 |>
  mutate(year = case_when(value > -1 ~ 2020))

# make the geoid variable better
treatarea10 <- treatarea10 |>
  mutate(geonum = as.numeric(GEOID))
treatarea10 <- treatarea10 |>
  mutate(bettergeo = geonum - 360670000000000)
treatarea20 <- treatarea20 |>
  mutate(geonum = as.numeric(GEOID))
treatarea20 <- treatarea20 |>
  mutate(bettergeo = geonum - 360670000000000)

# remove blocks from outside the census tract
treatarea10 <- treatarea10 |>
  filter(bettergeo > 32000000 & bettergeo < 33000000)
treatarea20 <- treatarea20 |>
  filter(bettergeo > 32000000 & bettergeo < 33000000)

#make the geoid variable even better 
treatarea10 <- treatarea10 |>
  mutate(bestgeo = bettergeo - 32000000)
treatarea20 <- treatarea20 |>
  mutate(bestgeo = bettergeo - 32000000)

# visualizing data for this area
treatplot10 <- treatarea10 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2010 Housing Units in Treatment Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

# and for 2020
treatplot20 <- treatarea20 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2020 Housing Units in Treatment Area") +
  theme_minimal()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

# putting both plots side by side
grid.arrange(treatplot10, treatplot20, ncol = 2)

# changing boundaries
treatplotoverlap <- ggplot() +
  geom_sf(data = treatarea10, color = "black", alpha = .2) + 
  geom_sf(data = treatarea20, color = "red", alpha = .2)+
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("Block Borders Treatment Area") +
  theme_minimal()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
treatplotoverlap

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
  ggtitle("2010 Housing Density in Treatment Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
treatdensplot20 <- treatarea20 |>
  filter(!is.nan(density)) |>
  ggplot(aes(fill = density)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Housing Units per Square KM", 
                       limits = set_units(c(1, 60100))) +
  ggtitle("2020 Housing Density in Treatment Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
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
  ggtitle("2010 Log of Housing Density in Treatment Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
treatlnplot20 <- treatarea20 |>
  filter(!is.nan(logdens)) |>
  ggplot(aes(fill = logdens)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Log of Housing Units per Square KM", 
                       limits = set_units(c(1, 17))) +
  ggtitle("2020 Log of Housing Density in Treatment Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
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
  ggtitle("2010 Housing Units in Control Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
controlarea20 <- st_intersection(onon20, control_shape)
controlplot20 <- controlarea20 %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2020 Housing Units in Control Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
grid.arrange(controlplot10, controlplot20, ncol = 2)

controlplotoverlap <- ggplot() +
  geom_sf(data = controlarea10, color = "black", alpha = .2) + 
  geom_sf(data = controlarea20, color = "red", alpha = .2)+
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("Block Borders in Control Area") +
  theme_minimal()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
controlplotoverlap

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
  ggtitle("2010 Housing Density in Control Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
controldensplot20 <- controlarea20 |>
  filter(!is.nan(density)) |>
  ggplot(aes(fill = density)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Housing Units per Square KM", 
                       limits = set_units(c(1, 10000))) +
  ggtitle("2020 Housing Density in Control Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
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
  ggtitle("2010 Log of Housing Density in Control Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
controllnplot20 <- controlarea20 |>
  ggplot(aes(fill = logdens)) + 
  geom_sf(color = "black", size = 1)  +
  scale_fill_viridis_c(option = "C", 
                       name = "Log of Housing Units per Square KM", 
                       limits = set_units(c(1, 17))) +
  ggtitle("2020 Log of Housing Density in Control Area") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
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

# find the means
mean(controlarea10$validdensity, na.rm = TRUE) 
mean(treatarea10$validdensity, na.rm = TRUE)
mean(controlarea20$validdensity, na.rm = TRUE) 
mean(treatarea20$validdensity, na.rm = TRUE)

#
# let's try to create merged blocks!
#

# try to overlay GEOID names onto the blocks
treat10labels <- treatarea10 |>
  ggplot() +
  geom_sf(color = "black") +
  theme_minimal()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_sf_text(aes(label = bestgeo), size = 3, color = "black")

treat20labels <- treatarea20 |>
  ggplot() +
  geom_sf(color = "black") + 
  theme_minimal()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_sf_text(aes(label = bestgeo), size = 3, color = "black")



# combining the block borders for both censuses

# making the data for the treatment area 
treat1020area <- st_intersection(treatarea10, treatarea20)
# turning this into a plot for 2010 housing units
treat1020plot1 <- treat1020area %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2010 Housing Units in Treatment Area, New Borders") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
# doing the same for 2020 housing units
treat1020plot2 <- treat1020area %>%
  ggplot(aes(fill = value.1)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("2020 Housing Units in Treatment Area, New Borders") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

# creating a variable for change between 2010 and 2020
treat1020area <- treat1020area |>
  mutate(unitchange = value.1 - value)
# let's see it plotted!
treatchangeplot <- treat1020area %>%
  ggplot(aes(fill = unitchange)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("Treatment Area Change in Housing Units") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
treatchangeplot

# okay, let's do this for the control area

control1020area <- st_intersection(controlarea10, controlarea20)
control1020area <- control1020area |>
  mutate(unitchange = value.1 - value)
controlchangeplot <- control1020area |>
  ggplot(aes(fill = unitchange)) + 
  geom_sf(color = "black", size = 1) + 
  scale_fill_viridis_c(option = "C", limits = c(1, 800), name = "Housing Units") +
  ggtitle("Control Area Change in Housing Units") +
  theme_minimal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
grid.arrange(treatchangeplot, controlchangeplot, ncol= 2)


# now let's make a dummy variable for treatment!

treat1020area <- treat1020area |>
  mutate(treatment = 1)

control1020area <- control1020area |>
  mutate(treatment = 0)

# time to give these shapefiles a haircut

trimtreatarea <- treat1020area |>
  filter(value != 0 | value.1 != 0)

trimcontrolarea <- control1020area |>
  filter(value != 0 | value.1 != 0) 


# can we bind these two together?

comparecontrol <- control1020area |>
  select(GEOID, value, STATEFP, COUNTYFP, TRACTCE, NAME.1, value.1, geometry, unitchange, area, density, density.1, treatment)
comparetreat <- treat1020area |>
  select(GEOID, value, STATEFP, COUNTYFP, TRACTCE, NAME.1, value.1, geometry, unitchange, area, density, density.1, treatment)
bindedarea <- rbind(comparecontrol, comparetreat)


# great. let's (FINALLY) do some matching!

bindedarea <- bindedarea |>
  filter(value > 0 | value.1 > 0)
bindedarea <- bindedarea |>
  filter(!is.infinite(density) & !is.infinite(density.1)) 
bindedarea <- bindedarea |>
  mutate(densitydiff = density.1 - density)

#first with no matching
nomatch <- bindedarea |>
  matchit(formula = treatment ~ value + area,
          method = NULL, distance = "glm")
nomatcheddata <- match.data(nomatch)
mnull <- lm(unitchange ~ treatment, data = nomatcheddata, weights = weights)

# now some matching models
match0 <- bindedarea |>
  matchit(formula = treatment ~ value + area,
          method = "nearest", distance = "glm")
matched0data <- match.data(match0)
m0 <- lm(unitchange ~ treatment, data = matched0data, weights = weights)

plot(match0, type = "jitter", interactive = FALSE)

match1 <- matchit(formula = treatment ~ value + area,
                  data = bindedarea |> filter(unitchange > 0),
                  method = "nearest", distance = "glm")
matched1data <- match.data(match1)
m1 <- lm(unitchange ~ treatment, data = matched1data, weights = weights)

match2 <- bindedarea |>
  matchit(formula = treatment ~ value + area,
          method = "optimal", distance = "glm")
matched2data <- match.data(match2)
m2 <- lm(unitchange ~ treatment, data = matched2data, weights = weights)

match3 <- matchit(formula = treatment ~ value + area,
                  data = bindedarea |> filter(unitchange > 0),
                  method = "optimal", distance = "glm")
matched3data <- match.data(match3)
m3 <- lm(unitchange ~ treatment, data = matched3data, weights = weights)

modelsummary(list("Model 1" = m0, "Model 2" = m1, "Model 3" = m2, "Model 4" = m3))



# time to do Albany (the second control group)

