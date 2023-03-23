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

greece_region_map = st_read("GIF-1/Greece_Region_Map/d7f50467-e5ef-49ac-a7ce-15df3e2ed738.shp") 

## Rename regions

greece_region_map$PER = c("Eastern Macedonia and Thrace","Central Macedonia","Western Macedonia",
                          "Epirus", "Thessaly", "North Aegean", "South Aegean", "Central Greece",
                          "Western Greece", "Peloponnese", "Ionian Islands", "Crete", "Attica")

## Add observations

greece_region_map$dentists = c(568, 2369, 252, 
                                 331, 740, 165, 
                                 321, 426, 632, 
                                 546, 183, 664, 
                                 6562)

greece_region_map$pop2021 = c(562201, 1795669, 254595,
                              319991, 688255, 194943,
                              327820, 508254, 648220,
                              539535, 204532, 624408,
                              3828434)

greece_region_map$per1000 = greece_region_map$dentists/greece_region_map$pop2021 *1000

# Visualization

map = ggplot2::ggplot(data = greece_region_map) +
  geom_sf(aes(fill = per1000)) +
  geom_sf_text(aes(label = round(per1000, digits = 2)),
               colour = "black", 
               size = 13,
               family="mont") +
  labs(
    title = "Dentists per Region",
    subtitle = "Total dentists in Greece are 13500 of which half of them are only on **Attica** (regional data, 2021)",
    caption = "**stesiam** | Source: Panhellenic Medical Association (PMA) and Hellenic Dental Association (HAD) "
  ) +
  scale_fill_gradient(low='white', high='grey80') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-2/GIF-2.png",
  plot = map,
  device = "png",
  bg = "white")

