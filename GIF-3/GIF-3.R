## Libraries

library(ggplot2)
library(sf)
library(ggtext)
library(showtext)

## Add fonts

font_add_google("Righteous", family = "clim")
font_add_google("Montserrat", family = "mont")

showtext_auto()


## Read shapefile

greece_region_map = st_read("GIF-3/Greece_Region_Map/d7f50467-e5ef-49ac-a7ce-15df3e2ed738.shp") 

## Rename regions

greece_region_map$PER = c("Eastern Macedonia and Thrace","Central Macedonia","Western Macedonia",
                          "Epirus", "Thessaly", "North Aegean", "South Aegean", "Central Greece",
                          "Western Greece", "Peloponnese", "Ionian Islands", "Crete", "Attica")

## Add observations

greece_region_map$area = c(14157, 18811, 9451, 
                                 9203, 14037, 3836, 
                                 5286, 15549, 11350, 
                                 15490, 2307, 8336, 
                                 3808)

greece_region_map$pop2021 = c(562201, 1795669, 254595,
                              319991, 688255, 194943,
                              327820, 508254, 648220,
                              539535, 204532, 624408,
                              3828434)

greece_region_map$density = greece_region_map$pop2021/greece_region_map$area

# Visualization

map = ggplot2::ggplot(data = greece_region_map) +
  geom_sf(aes(fill = density)) +
  geom_sf_text(aes(label = round(density, digits = 0)),
               colour = "black", 
               size = 15,
               family="mont") +
  labs(
    title = "Population Density per Region",
    subtitle = "Average concentration of population per squared kms. (regional data, 2021)",
    caption = "**stesiam** | Source: Hellenic Statistical Authority"
  ) +
  scale_fill_gradient(low='white', high='#ff8864') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-3/GIF-3.png",
  plot = map,
  device = "png",
  bg = "white")

