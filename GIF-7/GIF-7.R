## Libraries

library(ggplot2)
library(sf)
library(ggtext)
library(showtext)
library(readxl)
library(dplyr)


## Add fonts

font_add_google("Righteous", family = "clim")
font_add_google("Montserrat", family = "mont")

showtext_auto()


## Read shapefile

greece_region_map = st_read("GIF-7/Greece_Region_Map/d7f50467-e5ef-49ac-a7ce-15df3e2ed738.shp") 

## Rename regions

greece_region_map$PER = c("Eastern Macedonia and Thrace","Central Macedonia","Western Macedonia",
                          "Epirus", "Thessaly", "North Aegean", "South Aegean", "Central Greece",
                          "Western Greece", "Peloponnese", "Ionian Islands", "Crete", "Attica")


vineyards <- read_excel("GIF-7/vineyards.xlsx", skip = 4) %>%
  select(1:2) %>%
  setNames(c("PER", "Area"))


greece_region_map = inner_join(greece_region_map, vineyards, by = "PER")


# Visualization

map = ggplot2::ggplot(data = greece_region_map) +
  geom_sf(aes(fill = Area)) +
  geom_sf_text(aes(label = round(Area/1000, digits = 0)),
               colour = "black", 
               size = 15,
               family="mont") +
  labs(
    title = "Areas with Vineyards",
    subtitle = "Total vineyard area in thousands of acres (regional data, 2020)",
    caption = "**stesiam** | Source: Hellenic Statistical Authority"
  ) +
  scale_fill_gradient(low='white', high='#8f3875') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-7/GIF-7.png",
  plot = map,
  device = "png",
  bg = "white")
