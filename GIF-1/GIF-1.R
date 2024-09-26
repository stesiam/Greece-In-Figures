library(readr)
library(readxl)
library(dplyr)
library(fuzzyjoin)

library(ggplot2)
library(sf)
library(glue)
library(ggtext)
library(showtext)
library(sysfonts)


font_add_google("Righteous", family = "clim")
font_add_google("Montserrat", family = "mont")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

here::here("GIF-1")

shapefile <- st_read("Shapefiles/GR_bsm_admn_adm2_py_EuroGeoGraphics_2015_pp.shp")


## Download and load dataset

download.file(url = "https://www.statistics.gr/en/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=3&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_documentID=115366&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_locale=en",
              destfile = "gdp_capita.xlsx")

gdp_capita <- read_excel("GIF-1/gdp_capita.xlsx", 
                         skip = 8)



## Tidy data

gdp_capita = gdp_capita |>
  rename(NAMA2 = `Nuts II and  Nuts III`) |>
  dplyr::select(-...24) |>
  head(75)

# Perform a fuzzy join based on approximate matching
matched_data <- stringdist_inner_join(shapefile, gdp_capita, 
                                      by = "NAMA2", 
                                      max_dist = 7, 
                                      method = "lv",
                                      distance_col = "distance")

matched_data = matched_data |>
  relocate(NAMA2.x, NAMA2.y, distance, .before = 1)

matched_data = matched_data |>
  group_by(NAMA2.x) |>
  dplyr::filter(distance == min(distance),
                NAMA2.x != "Agion Oros")

matched_data = matched_data |>
  rename(
    gdp_2021 =  `2021*`
  ) |>
  select(NAMA2.y, geometry, gdp_2021)

matched_data = matched_data |>
  arrange(NAMA2.y)

matched_data <- st_as_sf(matched_data)


# Viz texts

title_text = glue("<b>GDP per Capita</b>")
subtitle_text = glue("Regional GDP/capita (2021) indicates the average welath of the citizens of the specified region
                        **Attica**, **Central Greece** and **South Aegean islands** are the regions with the highest GDP/Capita.
                        On the other side, **North Aegean**, **Epirus** and **Eastern Macedonia, Thrace** have the least wealthy citizens.")
caption_text = glue("*Data of:* 2021 - *Base year:* &nbsp; 2015<br><b> Data:</b> Hellenic Statistical Authority<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")


# Visualization

map = ggplot2::ggplot(data = matched_data) +
  geom_sf(aes(fill = gdp_2021)) +
  
  # Eastern Macedonia and Thrace
  
  geom_sf_text(
    x = 25.2, y = 41.15, 
    label = round(matched_data$gdp_2021[1]/1000, 2), 
    size = 7.5, 
    family = "mont",
    fontface = "bold"
  ) +
  
  # Attica (Athens)
  geom_sf_text(
    x = 23.8, y = 37.96, 
    label = round(matched_data$gdp_2021[2]/1000, 2), 
    size = 3.4, 
    family = "mont",
    fontface = "bold",
    angle = -50
  ) +
  # Western Greece
  geom_sf_text(
    x = 21.2, y = 38.65, 
    label = round(matched_data$gdp_2021[3]/1000, 2), 
    size = 4.5, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Western Macedonia
  
  geom_sf_text(
    x = 21.5, y = 40.4, 
    label = round(matched_data$gdp_2021[4]/1000, 2), 
    size = 5, 
    family = "mont",
    fontface = "bold"
  ) +
  
  # Ionian Islands
  
  geom_sf_text(
    x = 20.2, y = 39,
    label = round(matched_data$gdp_2021[5]/1000, 2), 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Epirus
  
  geom_sf_text(
    x = 20.75, y = 39.6,
    label = round(matched_data$gdp_2021[6]/1000, 2), 
    size = 5, 
    family = "mont",
    fontface = "bold"
  ) +
  
  # Central Macedonia
  
  geom_sf_text(
    x = 23, y = 40.8, 
    label = round(matched_data$gdp_2021[7]/1000, 2), 
    size = 8.5, 
    family = "mont",
    fontface = "bold"
  ) +
  
  # Crete
  
  geom_sf_text(
    x = 25.1, y = 35.2, 
    label = round(matched_data$gdp_2021[8]/1000, 2), 
    size = 5, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # South Aegean
  
  geom_sf_text(
    x = 26, y = 36.8, 
    label = round(matched_data$gdp_2021[9]/1000, 2), 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Peloponnese
  
  geom_sf_text(
    x = 22.4, y = 37.45, 
    label = round(matched_data$gdp_2021[10]/1000, 2), 
    size = 5.4, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Central Greece
  
  geom_sf_text(
    x = 22.65, y = 38.6, 
    label = round(matched_data$gdp_2021[11]/1000, 2), 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -25
  ) +
  
  # Thessaly
  
  geom_sf_text(
    x = 22.15, y = 39.5, 
    label = round(matched_data$gdp_2021[12]/1000, 2), 
    size = 7, 
    family = "mont",
    fontface = "bold",
    angle = 0
  ) +
  
  # North Aegean
  
  geom_sf_text(
    x = 25.9, y = 39.2, 
    label = round(matched_data$gdp_2021[13]/1000, 2), 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  scale_fill_gradient(low='white', high='yellow3') +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_markdown(family = "clim", hjust = 0.5, size = 20,
                                  margin = margin(t = 20, b = 10)),
    plot.subtitle = element_textbox_simple(size = 10, family = "mont", hjust = 0.5, 
                                           lineheight = 1.2),
    plot.caption = element_markdown(family = "mont", hjust = 0.5,
                                    margin = margin(t = 5, b = 5),
                                    lineheight = 1.2),
    legend.position = "none"
  )

ggsave(
  filename = "GIF-1/GIF-1.png",
  plot = map,
  width = 7,
  height = 7,
  device = "png",
  bg = "white")
