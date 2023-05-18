## Libraries

library(ggplot2)
library(sf)
library(ggtext)
library(showtext)
library(readxl)
library(dplyr)

library(stringr)


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

greece_region_map$greekname = c("Ανατολική Μακεδονία και Θράκη", "Κεντρική Μακεδονία",
                                "Δυτική Μακεδονία", "Ήπειρος", "Θεσσαλία", "Βόρειο Αιγαίο",
                                "Νότιο Αιγαίο", "Στερεά Ελλάδα", "Δυτική Ελλάδα",
                                "Πελοπόννησος", "Ιόνια Νησιά", "Κρήτη", "Αττική")

greece_region_map$RegionGen = c("Ανατολικής Μακεδονίας και Θράκης", "Κεντρικής Μακεδονίας",
                               "Δυτικής Μακεδονίας", "Ηπείρου", "Θεσσαλίας", "Βόρειου Αιγαίου", 
                               "Νότιου Αιγαίου","Στερεάς Ελλάδας", "Δυτικής Ελλάδας", "Πελοποννήσου", 
                               "Ιόνιων Νήσων", "Κρήτης", "Αττικής")

v = c("ά" = "α", "έ" = "ε", "ή" = "η", "ί" = "ι", "ό"="ο", "ύ"="υ")

greece_region_map$RegionGen = str_replace_all(string = greece_region_map$RegionGen, v) %>%
  toupper


## Import dataset

weddings = read_excel("GIF-8/weddings.xlsx", sheet = "2015-2021", range = "A182:B259") %>%
  na.omit() %>%
  setNames(c("greekname", "Marriages"))
  

## Merge to keep only specified values (Regions)

greece_region_map = merge(greece_region_map, weddings, by = "greekname")

greece_region_map$Observation = as.numeric(greece_region_map$Observation)

## Population per Region

apografi2021 <- read_excel("GIF-8/apografi2021.xlsx", 
                           sheet = "ΠΕΡΙΦΕΡΕΙΕΣ") %>%
  setNames(c("RegionGen","Population")) %>%
  select(RegionGen, Population) %>%
  filter(.[[1]] != "Σύνολο Χώρας")



## Clean dataset 

apografi2021$RegionGen = str_replace(apografi2021$RegionGen, "ΠΕΡΙΦΕΡΕΙΑ ", "")


## Combine datasets

new_data = left_join(greece_region_map, apografi2021, by = "RegionGen")

new_data$Marriages = as.numeric(new_data$Marriages)
new_data$perc = new_data$Marriages/new_data$Population*1000

## Visualize

map = ggplot2::ggplot(data = new_data) +
  geom_sf(aes(fill = perc)) +
  geom_sf_text(aes(label = round(perc, digits = 1)),
               colour = "black", 
               size = 15,
               family="mont") +
  labs(
    title = "Weddings",
    subtitle = "Weddings per 1000 regional residents",
    caption = "**stesiam** | Source: Hellenic Statistical Authority"
  ) +
  scale_fill_gradient(low='white', high='#fda2c6') +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "clim", size = 60, hjust = 0.5),
    plot.subtitle = element_markdown(family = "mont", size = 25, hjust = 0.5, lineheight = 0.5),
    plot.caption = element_markdown(family = "mont", size = 20, hjust = 0.5),
    legend.position = "none"
  )



## Export visualization

ggsave(
  filename = "GIF-8/GIF-8.png",
  plot = map,
  device = "png",
  bg = "white"
)


