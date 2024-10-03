library(readr)
library(readxl)
library(dplyr)
library(fuzzyjoin)
library(tabulizer)

library(ggplot2)
library(MetBrewer)
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

download.file(
  url = "https://www.statistics.gr/en/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=3&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_documentID=115793&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_locale=en",
  destfile = "GIF-0/pharmacies.xlsx"
)



statistics_tables <- extract_tables(
  file   = "GIF-2/subset.pdf", 
  method = "decide", 
  output = "data.frame")

regional_population = statistics_tables[[1]] |>
  select(-1) |>
  select(1,3) |>
  setNames(c("Region", "Population")) |>
  mutate(
    Population = stringr::str_replace_all(Population, ",", "")
  ) |>
  tidyr::drop_na() |>
  mutate(
    Region = stringr::str_to_title(Region)
  ) |>
  slice(-nrow(statistics_tables[[1]]))

pharmacies_raw = read_excel("GIF-0/pharmacies.xlsx", 
                                          sheet = "Φαρμακεία 2010-2022") |>
  select(last_col(), last_col(1)) |>
  tidyr::drop_na() |>
  setNames(c("Region", "Pharmacies"))

data_join = fuzzyjoin::stringdist_left_join(regional_population, pharmacies_raw,
                                            by = "Region",distance_col = "distance") |>
  rename(Region = Region.x) |>
  mutate(Pharmacies10000Capita = round(Pharmacies * 10000 / as.numeric(Population), digits = 2 )) |>
  rename(NAMA2 = Region) |>
  select(NAMA2, Pharmacies10000Capita)


data_shp_join = fuzzyjoin::stringdist_left_join(data_join, shapefile,
                                                by = "NAMA2",distance_col = "distance",
                                                max_dist = 7) |>
  group_by(NAMA2.x) |>
  dplyr::filter(distance == min(distance)) |>
  rename(Region = NAMA2.x) |>
  select(Region, Pharmacies10000Capita, geometry) |>
  arrange(Region) |>
  st_as_sf()

## Viz texts

title_text = glue("<b>Φαρμακεία στην Ελλάδα</b>")
subtitle_text = glue("Αριθμός φαρμακείων ανά 10,000 κατοίκους")
caption_text = glue("*Δεδομένα του:* 2022 <br><b> Πηγή:</b> Πανελλήνιος Φρμακευτικός Σύλλογος<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")


## Viz background

bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Demuth")[8:10]))

## Map viz

map = ggplot2::ggplot(data = data_shp_join) +
  scale_x_continuous(limits = c(19.7, 28.3)) +
  geom_sf(aes(fill = Pharmacies10000Capita)) +
  
  # Eastern Macedonia and Thrace
  
  geom_sf_text(
    x = 25.2, y = 41.15, 
    label = data_shp_join$Pharmacies10000Capita[1], 
    size = 7.5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Attica (Athens)
  geom_sf_text(
    x = 23.8, y = 37.96, 
    label = data_shp_join$Pharmacies10000Capita[2], 
    size = 3.4, 
    family = "mont",
    fontface = "bold",
    angle = -50,
    color = "black"
  ) +
  # Western Greece
  geom_sf_text(
    x = 21.2, y = 38.65, 
    label = data_shp_join$Pharmacies10000Capita[3], 
    size = 4.5, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Western Macedonia
  
  geom_sf_text(
    x = 21.5, y = 40.4, 
    label = data_shp_join$Pharmacies10000Capita[4], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Ionian Islands
  
  geom_sf_text(
    x = 20.2, y = 39,
    label = data_shp_join$Pharmacies10000Capita[5], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Epirus
  
  geom_sf_text(
    x = 20.75, y = 39.6,
    label = data_shp_join$Pharmacies10000Capita[6], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Central Macedonia
  
  geom_sf_text(
    x = 23, y = 40.8, 
    label = data_shp_join$Pharmacies10000Capita[7], 
    size = 8.5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Crete
  
  geom_sf_text(
    x = 25.1, y = 35.2, 
    label = data_shp_join$Pharmacies10000Capita[8], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # South Aegean
  
  geom_sf_text(
    x = 26, y = 36.8, 
    label = data_shp_join$Pharmacies10000Capita[9], 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -10,
    color = "black"
  ) +
  
  # Peloponnese
  
  geom_sf_text(
    x = 22.4, y = 37.45, 
    label = data_shp_join$Pharmacies10000Capita[10], 
    size = 5.4, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Central Greece
  
  geom_sf_text(
    x = 22.65, y = 38.6, 
    label = data_shp_join$Pharmacies10000Capita[11], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -25
  ) +
  
  # Thessaly
  
  geom_sf_text(
    x = 22.15, y = 39.5, 
    label = data_shp_join$Pharmacies10000Capita[12], 
    size = 7, 
    family = "mont",
    fontface = "bold",
    angle = 0,
    color = "white"
  ) +
  
  # North Aegean
  
  geom_sf_text(
    x = 25.9, y = 39.2, 
    label = data_shp_join$Pharmacies10000Capita[13], 
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
  scale_fill_gradientn(colors = met.brewer("VanGogh3")) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_markdown(family = "serif", hjust = 0.5, size = 20,
                                  margin = margin(t = 20, b = 10),
                                  color = "white"),
    plot.subtitle = element_markdown(size = 9, family = "serif", hjust = 0.5, 
                                           lineheight = 1.2, color = "white",
                                           margin = margin(l = 10, r = 10)),
    plot.caption = element_markdown(family = "serif", hjust = 0.5,
                                    margin = margin(t = 5, b = 5),
                                    lineheight = 1.2,
                                    color = "white",
                                    size = 7),
    legend.position = "none",
    plot.background = element_rect(fill = bg_gradient, color = "transparent")
  )

ggsave(
  filename = "GIF-0/GIF-0-el.png",
  height = 7,
  width = 5.6,
  plot = map,
  device = "png",
  dpi = 300)



