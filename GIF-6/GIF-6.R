## Libraries

library(ggplot2)
library(sf)
library(ggtext)
library(showtext)
library(readxl)

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


greece_region_map$elec2012 = c(843946.91, 3019191.59, 412555.30,
                               454254.29, 1050738.12, 334497.85,
                               555784.55, 895781.60, 1036741.80,
                               1070718.78, 371099.24, 932433.50,
                               7476845.69)


greece_region_map$pop2011 = c(608182, 1882108, 283689,
                              336856, 732762, 199231,
                              309015, 547390, 679796,
                              577903, 207855, 623065,
                              3828434)

greece_region_map$per1000 = greece_region_map$elec2012/greece_region_map$pop2011*1000

# Visualization

map = ggplot2::ggplot(data = greece_region_map) +
  geom_sf(aes(fill = per1000)) +
  geom_sf_text(aes(label = round(per1000, digits = 0)),
               colour = "black", 
               size = 15,
               family="mont") +
  labs(
    title = "Electricity Consumption",
    subtitle = "Consumption in thousands kwh per thousand inhabitants (regional data, 2012)",
    caption = "**stesiam** | Source: Hellenic Statistical Authority"
  ) +
  scale_fill_gradient(low='white', high='#e5de00') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-6/GIF-6.png",
  plot = map,
  device = "png",
  bg = "white")
