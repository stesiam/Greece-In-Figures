library(readr)
library(readxl)
library(dplyr)
library(fuzzyjoin)
library(qpdf)

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

here::here("GIF-2")

shapefile <- st_read("Shapefiles/GR_bsm_admn_adm2_py_EuroGeoGraphics_2015_pp.shp")


## Download and load dataset

download.file(url = "https://elstat-outsourcers.statistics.gr/census_results_2022_en.pdf",
              destfile = "GIF-2/census.pdf")

greek_doctors_raw <- read_excel("GIF-2/greek_doctors_regional.xlsx", 
                                skip = 10) |>
  select(last_col(1), last_col()) |>
  setNames(c("Dentists", "Region")) |>
  tidyr::drop_na()

# Extract only the pages I am interested at

pdf_subset("GIF-2/census.pdf",
           pages = 6,  output = "GIF-2/subset.pdf")


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

population_doctors_joined = stringdist_inner_join(regional_population, 
                                                  greek_doctors_raw,
                                                  by = "Region", max_dist = 7, distance_col = "distance",
                                                  method = "lv") |>
  relocate(Region.x, Region.y, distance, .before = 1) |>
  group_by(Region.x) |>
  dplyr::filter(distance == min(distance),
                Region.x != "Agion Oros") |>
  rename(
    NAMA2 = Region.x
  ) |>
  select(NAMA2, Population, Dentists)


## Join data to shapefile

shp_data_join = stringdist_inner_join(shapefile, 
                                      population_doctors_joined,
                                      by = "NAMA2", max_dist = 7, 
                                      distance_col = "distance",
                                      method = "lv") |>
  relocate(NAMA2.x, NAMA2.y, distance, .before = 1) |>
  group_by(NAMA2.x) |>
  dplyr::filter(distance == min(distance),
                NAMA2.x != "Agion Oros") |>
  rename(
    Region = NAMA2.x
  ) |>
  select(Region, Population, Dentists, geometry) |>
  arrange(Region) |>
  mutate(DentPer1000 = round(Dentists/as.numeric(Population)*1000, digits =2))


shp_data_join = st_as_sf(shp_data_join)

## Viz texts

title_text = glue("<b>Dentists by Region</b>")
subtitle_text = glue("Regional GDP/capita (2021) indicates the average welath of the citizens of the specified region
                        **Attica**, **Central Greece** and **South Aegean islands** are the regions with the highest GDP/Capita.
                        On the other side, **North Aegean**, **Epirus** and **Eastern Macedonia, Thrace** have the least wealthy citizens.")
caption_text = glue("*Data of:* 2022 - Regional Population is based on 2021 census<br><b> Source:</b> Panhellenic Medical Association (PMA) and Hellenic Dental Association (HAD) <br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")


bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Pillement")[3:4]))

map = ggplot2::ggplot(data = shp_data_join) +
  scale_x_continuous(limits = c(19.7, 28.3)) +
  geom_sf(aes(fill = DentPer1000)) +
  
  # Eastern Macedonia and Thrace
  
  geom_sf_text(
    x = 25.2, y = 41.15, 
    label = shp_data_join$DentPer1000[1], 
    size = 7.5, 
    family = "mont",
    fontface = "bold"
  ) +
  
  # Attica (Athens)
  geom_sf_text(
    x = 23.8, y = 37.96, 
    label = shp_data_join$DentPer1000[2], 
    size = 3.4, 
    family = "mont",
    fontface = "bold",
    angle = -50,
    color = "white"
  ) +
  # Western Greece
  geom_sf_text(
    x = 21.2, y = 38.65, 
    label = shp_data_join$DentPer1000[3], 
    size = 4.5, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Western Macedonia
  
  geom_sf_text(
    x = 21.5, y = 40.4, 
    label = shp_data_join$DentPer1000[4], 
    size = 5, 
    family = "mont",
    fontface = "bold"
  ) +
  
  # Ionian Islands
  
  geom_sf_text(
    x = 20.2, y = 39,
    label = shp_data_join$DentPer1000[5], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Epirus
  
  geom_sf_text(
    x = 20.75, y = 39.6,
    label = shp_data_join$DentPer1000[6], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Central Macedonia
  
  geom_sf_text(
    x = 23, y = 40.8, 
    label = shp_data_join$DentPer1000[7], 
    size = 8.5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Crete
  
  geom_sf_text(
    x = 25.1, y = 35.2, 
    label = shp_data_join$DentPer1000[8], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # South Aegean
  
  geom_sf_text(
    x = 26, y = 36.8, 
    label = shp_data_join$DentPer1000[9], 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Peloponnese
  
  geom_sf_text(
    x = 22.4, y = 37.45, 
    label = shp_data_join$DentPer1000[10], 
    size = 5.4, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Central Greece
  
  geom_sf_text(
    x = 22.65, y = 38.6, 
    label = shp_data_join$DentPer1000[11], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -25
  ) +
  
  # Thessaly
  
  geom_sf_text(
    x = 22.15, y = 39.5, 
    label = shp_data_join$DentPer1000[12], 
    size = 7, 
    family = "mont",
    fontface = "bold",
    angle = 0
  ) +
  
  # North Aegean
  
  geom_sf_text(
    x = 25.9, y = 39.2, 
    label = shp_data_join$DentPer1000[13], 
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
  scale_fill_gradientn(colors = met.brewer("OKeeffe2")) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_markdown(family = "clim", hjust = 0.5, size = 20,
                                  margin = margin(t = 20, b = 10),
                                  color = "white"),
    plot.subtitle = element_textbox_simple(size = 9, family = "mont", hjust = 0.5, 
                                           lineheight = 1.2, color = "white",
                                           margin = margin(l = 10, r = 10)),
    plot.caption = element_markdown(family = "mont", hjust = 0.5,
                                    margin = margin(t = 5, b = 5),
                                    lineheight = 1.2,
                                    color = "white",
                                    size = 7),
    legend.position = "none",
    plot.background = element_rect(fill = bg_gradient, color = "transparent"),
    panel.background = element_rect(fill = bg_gradient, color = "transparent")
  )

ggsave(
  filename = "GIF-2/GIF-2.png",
  height = 7,
  width = 6,
  plot = map,
  device = "png")

