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

greece_region_map = st_read("Shapefiles/Greece_Region_Map/d7f50467-e5ef-49ac-a7ce-15df3e2ed738.shp") 

## Rename regions

greece_region_map$PER = c("Eastern Macedonia and Thrace","Central Macedonia","Western Macedonia",
                          "Epirus", "Thessaly", "North Aegean", "South Aegean", "Central Greece",
                          "Western Greece", "Peloponnese", "Ionian Islands", "Crete", "Attica")

## Add observations

greece_region_map$gdp_capita = c(11643, 13343, 14656, 
                                 12098, 13064, 11355, 
                                 18332, 15497, 12424, 
                                 14553, 16041, 14490, 
                                 23430)


# Visualization

map = ggplot2::ggplot(data = greece_region_map) +
  geom_sf(aes(fill = gdp_capita)) +
  geom_sf_text(aes(label = round((gdp_capita)/1000, digits = 1)),
               colour = "black", 
               size = 13,
               family="mont") +
  labs(
    title = "GDP per capita",
    subtitle = "Greece's GDP/capita (in thousands Euros)<br>
    **Attica**, **South Aegean** and **Ionian Islands** are the regions with the highest GDP/Capita.<br>
    On the other side, **North Aegean** has the lowest value.",
    caption = "Base year : 2015 <br> **stesiam** | Data : Hellenic Statistical Authority "
  ) +
  scale_fill_gradient(low='white', high='yellow3') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-1/GIF-1.png",
  plot = map,
  device = "png",
  bg = "white")
