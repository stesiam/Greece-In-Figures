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

greece_region_map = st_read("GIF-5/Greece_Region_Map/d7f50467-e5ef-49ac-a7ce-15df3e2ed738.shp") 

## Rename regions

greece_region_map$PER = c("Eastern Macedonia and Thrace","Central Macedonia","Western Macedonia",
                          "Epirus", "Thessaly", "North Aegean", "South Aegean", "Central Greece",
                          "Western Greece", "Peloponnese", "Ionian Islands", "Crete", "Attica")

## Add observations

greece_region_map$cons_diesel_heat = c(88827.3334, 237736.297, 43483.0709, 
                                       47700.1682, 74639.1059, 17541.408, 
                                       15278.4606, 73956.5803, 45726.2929, 
                                       50750.9388, 18069.8432, 34796.1136, 
                                       290475.5617)

greece_region_map$pop2021 = c(562201, 1795669, 254595,
                              319991, 688255, 194943,
                              327820, 508254, 648220,
                              539535, 204532, 624408,
                              3828434)

greece_region_map$per1000 = greece_region_map$cons_diesel_heat/greece_region_map$pop2021*1000

# Visualization

map = ggplot2::ggplot(data = greece_region_map) +
  geom_sf(aes(fill = per1000)) +
  geom_sf_text(aes(label = round(per1000, digits = 0)),
               colour = "black", 
               size = 15,
               family="mont") +
  labs(
    title = "Heating Oil Consumption",
    subtitle = "Consumption in metric tones per thousand inhabitants (regional data, 2020)",
    caption = "**stesiam** | Source: Ministry of Environment and Energy"
  ) +
  scale_fill_gradient(low='white', high='#ff0000') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-5/GIF-5.png",
  plot = map,
  device = "png",
  bg = "white")
