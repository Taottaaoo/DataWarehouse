####################################
## Author: Matthew Walker
## AAEA Data Viz Competition
###################################

# install.packages('tidyverse')
# install.packages('magrittr')
# install.packages('tigris')
# install.packages('sf')
# install.packages('ggspatial')

library(tidyverse) # Basic Data tools (includes ggplot2)
library(magrittr)  # "Piping" tool, pipes are %>% or %<>% syntax
library(tigris)    # API for State and County boundaries
library(sf)        # GIS in R package
library(ggspatial) # allows for ggplot syntax for spatial data visualization
library(cowplot)

##################################
## Rough Draft Map Skeleton
##################################
StateLyr <- states()
StateLyr2 <- states()
CountyLyr <- counties()


StateLyr %<>% filter(STUSPS == 'WV' | STUSPS == 'OH' | STUSPS == 'KY' | STUSPS == 'TN') # filtering down to our study area

StateLyr2 %<>% filter(NAME == 'West Virginia' | NAME == 'Virginia' | NAME == 'Kentucky' |
                         NAME == 'Pennsylvania' | NAME == 'Maryland' | NAME == 'Ohio' | NAME == 'New York' |
                         NAME == 'North Carolina'| NAME == 'South Carolina' | NAME == 'Tennessee' |
                         NAME == 'Georgia' | NAME == 'Mississippi' | NAME == 'Alabama' |
                         NAME == 'Florida' | NAME == 'Louisiana' | 
                         NAME == 'Arkansas' | NAME == 'Missouri' | NAME == 'Michigan' | NAME == 'Minnesota' |
                         NAME == 'Illinois' | NAME == 'Indiana' | NAME == 'Iowa' | NAME == 'Wisconsin' |
                         NAME == 'Delaware' | NAME == 'New Jersey' | NAME == 'Connecticut' |
                         NAME == 'Rhode Island' | NAME == 'Massachusetts' | NAME == 'Vermont' |
                         NAME == 'New Hampshire' | NAME == 'Maine') # states that are used as background in our map

CountyLyr %<>% filter(STATEFP == '54' | STATEFP == '39' | STATEFP == '47' | STATEFP == '21' | 
                      STATEFP == '42' & NAME %in% c('Allegheny', 'Armstrong', 'Beaver', 'Bedford', 'Butler',
                                                    'Cambria', 'Clarion', 'Crawford', 'Erie', 'Fayette', 'Franklin',
                                                    'Fulton', 'Greene', 'Indiana', 'Jefferson', 'Lawrence', 'Mercer',
                                                    'Somerset', 'Venango', 'Washington', 'Westmoreland') |
                      STATEFP == '51' & NAME %in% c('Alleghany', 'Bath', 'Bland', 'Botetourt', 'Buchanan', 'Carroll',
                                                    'Craig', 'Dickenson', 'Floyd', 'Giles', 'Grayson', 'Henry', 'Highland',
                                                    'Lee', 'Montgomery', 'Patrick', 'Pulaski', 'Rockbridge', 'Russell',
                                                    'Scott', 'Smyth', 'Tazewell', 'Washington', 'Wise', 'Wythe') |
                      STATEFP == '24' & NAME %in% c('Allegany', 'Garrett', 'Washington')) # counties in our study area

mid_point <- CountyLyr$ALAND %>%  mean(na.rm = T)  # Calculation of the mean Land Area of a county in our study Area, just for Data Viz Rough Draft

ggplot() +
  geom_sf(data = StateLyr2) + # Background States
  geom_sf(data = CountyLyr, aes(fill = ALAND)) + # Counties displayed as choropleth of Land Area
  geom_sf(data = StateLyr, fill = NA, linewidth = 0.8) + # Overlay of Study Area states with thicker borders 
  coord_sf(xlim = c(-90, -77.5), ylim = c(34, 43)) + # Setting the "zoom" based on Lat/Lon
  xlab('Longitude') + ylab('Latitude') + # Label axes as Lat/Lon
  labs(fill = 'Land Area', title = 'Rough Draft Map Idea') + # Title and Legend Labels
  annotation_scale(location = "br", width_hint = 0.4, unit_category = 'imperial',
                   pad_x = unit(0.15, 'in'), pad_y = unit(0.1, 'in')) +  # scale bar
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.35, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) + # North Arrow
  theme(panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', linewidth = 0.5),
        panel.background = element_rect(fill = 'slategray1')) + # Background Oceans/Lakes
  scale_fill_gradient2(low = 'navy', mid = 'white', high = 'coral4', midpoint = mid_point) # Diverging color scheme for above/below Average Land Area  

setwd("C:/Users/mwalk/Downloads")
counties <- st_drop_geometry(CountyLyr)
write.csv(counties, "ARFBC_counties.csv")

############################
## Mapping Data Prep
############################


df <- read.csv('ARFBC_counties.csv', header = T, colClasses = 'character')

explore <- read.csv('df_business_dev_infra.csv', header = T, colClasses = 'character')

our_counties <- unique(df$GEOID)

explore %<>% filter(fips %in% our_counties)
explore2 <- filter(explore, variable_name == 'number_meat_processing')

explore2 %<>% rename(GEOID = fips)
df <- left_join(df, explore2, by = 'GEOID')

rm(explore2)

df1 <- read.csv('ARFBC_counties.csv', header = T, colClasses = 'character')

explore <- read.csv('df_foodaccess.csv', header = T, colClasses = 'character')
explore %<>% filter(fips %in% our_counties)
explore2 <- filter(explore, variable_name == 'pct_laccess_pop')

explore2 %<>% rename(GEOID = fips)
df1 <- left_join(df1, explore2, by = 'GEOID')

df2 <- rbind(df, df1) 

write.csv(df2, 'ARFBC_counties_v2.csv')


#########
## Bivariate Map
#########

#df2 %<>% pivot_wider(names_from = 'variable_name', values_from = 'value')

# df <- df2
# rm(df1, df2, explore, explore2)

setwd("C:/Users/Mwalke18/Downloads")
#setwd("C:/Users/mwalk/Downloads")
df <- read.csv('ARFBC_counties_v2.csv', header = T, colClasses = 'character')

#install.packages('biscale', dependencies = T)
library(biscale)
library(scales)
#library(cowplot)
#library(tigris)

states <- states()
states %<>% filter(NAME == "West Virginia" | NAME == "Virginia" | NAME == "Ohio" | NAME == "Pennsylvania" |
                     NAME == "Maryland" | NAME == "Tennessee" | NAME == "Kentucky")

counties <- counties()

our_counties <- unique(df$GEOID)

counties %<>% filter(GEOID %in% our_counties)

dfX <- filter(df, year == '2023' | year == '2015')

dfX %<>% select(-X.1, -year, -category, -topic_area, -value_codes) %>% 
  pivot_wider(names_from = 'variable_name', values_from = 'value') %>%
  mutate(number_meat_processing = as.numeric(number_meat_processing),
         pct_laccess_pop = as.numeric(pct_laccess_pop),
         ALAND = as.numeric(ALAND),
         AWATER = as.numeric(AWATER))

dfX <- left_join(counties, dfX)

dat <- bi_class(dfX, x=number_meat_processing, y=pct_laccess_pop, style = 'jenks', dim = 3)

our_breaks <- bi_class_breaks(dfX, x=number_meat_processing, y=pct_laccess_pop, style = 'jenks', dim = 3)

map <- ggplot() +
  geom_sf(data = dat, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_sf(data = states, fill = NA) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Low Access Population and Processing Facilities:",
    subtitle = "A bivariate map of Appalachia"
  ) +
  bi_theme(base_size = 18) +
  coord_sf(xlim = c(-90, -78), ylim = c(35, 43)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

#map

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Processing Facilities",
                    ylab = "% of Pop. with Low Food Access",
                    size = 10, breaks = our_breaks)



# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, .45, 0.35, 0.35) #+
  theme(plot.margin = unit(c(5,5,5,5), "cm"))

finalPlot

ggsave('Bivariate_Map_v4.png', height = 8, width = 8, units = 'in') ### New v4

###############
## Interp Surface Map
###############

#df <- read.csv('ARFBC_counties.csv', header = T, colClasses = 'character')

#counties <- counties()
#our_counties <- unique(df$GEOID)
#counties %<>% filter(GEOID %in% our_counties)

#rm(df)

df2 <- read.csv('df_business_dev_infra_point.csv', header = T, colClasses = 'character')

df2 %<>% filter(fips %in% our_counties) %>%
  filter(variable_name == 'location_meat_processing')

df2_sf = st_as_sf(df2, coords = c("long", "lat"), 
                 crs = 4326, agr = "constant")





#install.packages('elevatr')
#install.packages('tidyterra')

library(elevatr)
library(terra)
library(tidyterra)
#library(cowplot)

elev <- get_elev_raster(counties, z = 7)
elev_mask <- mask(elev, counties)
elev_mask <- rast(elev_mask)

WGS84 <- "+init=EPSG:4326"
NAD83 <- "+init=EPSG:4269"

elev_mask <- project(elev_mask, WGS84)

rm(elev, df2)

dist <- distance(elev_mask, df2_sf)

dist <- project(dist, NAD83)
dist <- mask(dist, counties)
dist <- dist / 1000

states <- states()
states %<>% filter(NAME == "West Virginia" | NAME == "Virginia" | NAME == "Ohio" | NAME == "Pennsylvania" |
                     NAME == "Maryland" | NAME == "Tennessee" | NAME == "Kentucky")


distPlot<-ggplot()+
  geom_spatraster(data = dist)+
  geom_sf(data = counties, fill = NA, color = 'white')+
  geom_sf(data = states, fill = NA)+
  #geom_sf(data = df2_sf)+
  scale_fill_gradient2(low = 'red', mid = 'antiquewhite', high = 'darkblue',
                       midpoint = 25, na.value = 'transparent')+
  labs(title = 'Distance to Meat Processor', subtitle = '1 km resolution',
       fill = 'Distance (km)')+
  coord_sf(xlim = c(-90, -78), ylim = c(35, 43))+
  theme_map(font_size = 18)+
  theme(legend.position = 'inside', legend.position.inside = c(0.2, 0.65),
        legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.75)),
        plot.title = element_text(size = rel(1.25), hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold",
                                     margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm")),
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.margin = unit(c(0.05, 0, 0.05, 0), "cm"))

distPlot

ggsave('Distance_Map_v3.png', height = 8, width = 8, units = 'in') ### No update

# install.packages('magick')
# library(magick)
# 
# bi_var_map <- ggdraw() + draw_image('Bivariate_Map_v2.png')
# 
# dist_map <- ggdraw() + draw_image('Distance_Map_v1.png')

########################
## ARC outline
########################

ARC_df <- read.csv("Appalachian-Counties-Served-by-ARC_2021.csv", header = T, colClasses = "character")

ARC <- counties()

ARC %<>% filter(GEOID %in% ARC_df$FIPS)

ggplot()+
  geom_sf(data = ARC, fill = NA)

ARC %<>% st_union(ARC)

ggplot()+
  geom_sf(data = ARC, fill = NA)

#####################
## Bar Graph of State
#####################

df_bars <- dfX %>% group_by(state_name) %>%
  summarise(Num_Meat_Processors = sum(number_meat_processing))



bars <- ggplot(df_bars, aes(x=state_name, y=Num_Meat_Processors, fill = state_name))+
  scale_x_discrete(label = c('Kentucky' = 'KY', 'Maryland' = 'MD', 'Ohio'='OH', 'Pennsylvania'='PA',
                             'Tennessee'='TN', 'Virginia'='VA', 'West Virginia'='WV')) +
  scale_fill_viridis_d(option = 'turbo')+
  geom_col() +
  geom_text(aes(label = Num_Meat_Processors), vjust = -0.5)+
  labs(title = 'Total Number of Meat\n Processors in \n ARFBC by State', x = '', y = '')+
  scale_y_continuous(limits = c(0,200), expand = c(0, 0)) +
  guides(fill='none') +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5, vjust = -6, face = "bold"),  #
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.margin = unit(c(-1.2, 0.0, 0.0, 0.0), "cm"))

bars

ggsave('state_bars_v2.png', height = 12, width = 4, units = 'in') ### Updated to v2

element_text
############
## Correlation between # meat proc and pct low access pop
#############

ggplot(dfX, aes(x = number_meat_processing, y = pct_laccess_pop, color = state_name))+
  geom_point()


############
## Line Graph of Cold Storage
############

explore <- read.csv('df_business_dev_infra.csv', header = T, colClasses = 'character')

#our_counties <- unique(df$GEOID)

#explore %<>% filter(fips %in% our_counties)

explore3 <- filter(explore, variable_name == 'public_refrigerated_warehouses' |
                     variable_name == 'private_semi_private_refrigerated_warehouses')

explore3$value <- as.numeric(explore3$value)

explore3 %<>% filter(state_name != 'US')


#explore4 <- explore3 %>% filter(fips %in% our_counties)
explore4 <- explore3 %>% filter(state_name == "West Virginia" | state_name == "Virginia" | state_name == "Ohio" | state_name == "Pennsylvania" |
                                  state_name == "Maryland" | state_name == "Tennessee" | state_name == "Kentucky")

ggplot(explore4, aes(x = state_name, y = value, fill = variable_name))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0.9))

explore4$year <- as.integer(explore4$year)

levels(explore4$variable_name) <- c('Private/Semi-Private', 'Public')

ColdStor <- ggplot(explore4, aes(x = year, y = value, color = state_name))+
  geom_line(linewidth = 1.5)+
  facet_wrap(~levels(variable_name))+
  scale_x_continuous(breaks=seq(2010, 2022, 2))+
  labs(color = '', y = 'Number of\n Cold Storage Facilities',
       x = 'Year', caption = "*The cold storage data is state-wide and includes facilities that may be outside the ARFBC region due to data availability")+#, linetype = 'Ownership')+
  scale_color_viridis_d(option = 'turbo') +
  #scale_linetype_discrete(labels = c('Private/Semi-private', 'Public'))+
  theme_classic(base_size = 18) +
  theme(plot.title = element_text(size = rel(1.25), hjust = 0.5, face = "bold"),
        #axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        legend.position = 'bottom', legend.direction = 'horizontal',
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  guides(colour = guide_legend(nrow = 1))

ColdStor

ggsave('ColdStorage_TS_v0.png', height = 8, width = 16, units = 'in') ##No update


###########
## Building Final Plot
###########


plot_row1 <- plot_grid(finalPlot, distPlot, nrow = 1, scale = 1.05)

#ggsave('check.png', height = 8, width = 16, units = 'in')

mainBlock <- plot_grid(plot_row1, ColdStor, ncol = 1, rel_heights = c(1,0.5))#+

#ggsave('check.png', height = 12, width = 16, units = 'in')


mainBlock <- plot_grid(plot_row1, ColdStor, ncol = 1, rel_heights = c(1,0.6))
mainBlock <- plot_grid(mainBlock, bars, nrow = 1, rel_widths = c(1, 0.3))

#ggsave('check.png', height = 12, width = 20, units = 'in')

### testing ground for final

plot_header <- ggdraw() + 
  draw_label("Farmer Access to Meat Processing in the Context of Household Food Access in Appalachia",
             size = 32, fontface = 'bold', x = 0, vjust = 0, hjust = -0.03)

complete <- plot_grid(plot_header, mainBlock, ncol = 1, rel_heights = c(0.1, 1))+
  theme(plot.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0,1,0,0.5), "cm"))

ggsave('check.png', height = 12.9, width = 20, units = 'in')



### Header with space for text

plot_header <- ggdraw() + 
  draw_label("Farmer Access to Meat Processing in the Context of Household Food Access in Appalachia",
             size = 34, fontface = 'bold', x = 0, vjust = -1.5, hjust = 0) +
  draw_label(" Areas with a greater number of meat processing facilities tended to have a larger percentage of residents struggling with low food access. The distance\n to meat processor map helps identify areas in which the establishment of a new meat processing facilities could improve the food distribution network.\n Cold Storage facilities generally outnumber meat processors in ARFBC states.",
             size = 20, fontface = 'bold', x = 0, vjust = 1.0, hjust = 0)

complete <- plot_grid(plot_header, mainBlock, ncol = 1, rel_heights = c(0.15, 1))+
  theme(plot.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0,1,0,0.5), "cm"))


ggsave('AAEA_panel_v10.png', height = 16, width = 21, units = 'in')




### Header without space for text

plot_header <- ggdraw() + 
  draw_label("Farmer Access to Meat Processing in the Context of Household Food Access in Appalachia",
             size = 32, fontface = 'bold', x = 0, vjust = 0, hjust = -0.01)

complete <- plot_grid(plot_header, mainBlock, ncol = 1, rel_heights = c(0.1, 1))+
  theme(plot.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0,1,0,0.5), "cm"))


ggsave('AAEA_panel_v9-2.png', height = 12.9, width = 21, units = 'in')
