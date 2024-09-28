library(readr)
library(readxl)
library(dplyr)
library(fuzzyjoin)
library(qpdf)
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

here::here("GIF-5")

shapefile <- st_read("Shapefiles/GR_bsm_admn_adm2_py_EuroGeoGraphics_2015_pp.shp")


## Download and load dataset

download.file(url = "https://elstat-outsourcers.statistics.gr/census_results_2022_en.pdf",
              destfile = "GIF-5/census.pdf")

download.file(url = "https://www.statistics.gr/en/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=2&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_documentID=513865&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_locale=en",
              destfile = "GIF-5/heating_oil.xlsx")

# Extract only the pages I am interested at

pdf_subset("GIF-5/census.pdf",
           pages = 6,  output = "GIF-5/subset.pdf")



statistics_tables <- extract_tables(
  file   = "GIF-5/subset.pdf", 
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


# Load data

heating_oil <- read_excel("GIF-5/heating_oil.xlsx", 
                          skip = 2) |>
  select(1,5) |>
  setNames(c("Region", "HeatOil")) |>
  tidyr::drop_na()

nuts1 = c("Voreia Ellada", "Kentriki Ellada", "Nisia Aigaiou, Kriti")

is_similar <- function(x, exclude_strings, threshold = 0.2) {
  # Calculate the Jaro-Winkler distance for each string in exclude_strings
  distances <- stringdist::stringdistmatrix(x, exclude_strings, method = "dl")
  # Find the minimum distance for the closest match
  min_distance <- min(distances)
  # Return TRUE if the minimum distance is below the threshold, meaning it's "similar"
  return(min_distance < threshold)
}

filter_out = c("Not Declared", 
               "Total Of Greece", "Nisia Agaioy, Kriti"
               )

# Apply the fuzzy matching function and filter out similar rows
df_filtered <- heating_oil %>%
  rowwise() %>%
  filter(!is_similar(Region, nuts1, threshold = 0.1)) |>
  dplyr::filter(! Region %in% filter_out)


heatingOIl_pop_joined = stringdist_left_join(df_filtered, regional_population,
                      by = "Region", distance = "distance",
                      method = "lv", max_dist = 13) |>
  dplyr::rename(NAMA2 = Region.y) |>
  group_by(NAMA2) |>
  dplyr::filter(distance == min(distance)) |>
  mutate(HeatOilLitres = HeatOil * 1000 / 0.85) |>
  mutate(HeatOilCapita = HeatOilLitres/as.numeric(Population)) |>
  arrange(NAMA2) |>
  select(NAMA2, HeatOilCapita)




data_shp_joined = stringdist_left_join(heatingOIl_pop_joined, shapefile,
                                       by = "NAMA2", distance = "distance",
                                       method = "lv", max_dist = 8) |>
  group_by(NAMA2.x) |>
  dplyr::filter(distance == min(distance)) |>
  ungroup(NAMA2.x) |>
  rename(Region = NAMA2.x) |>
  select(Region, HeatOilCapita, geometry) |>
  arrange(Region) |>
  mutate(HeatOilCapita = round(HeatOilCapita, 2)) |>
  st_as_sf()


## Viz texts

title_text = glue("<b>Heating Oil Consumption</b>")
subtitle_text = glue("Northern profectures of Greece have significantly higher cosumption of heating oil.
                     An expected fact as Kastoria (northern Greece town) averages at -10C and ")
caption_text = glue("*Data of:* 2022 - Regional Population is based on 2021 census<br><b> Source:</b> Ministry of Environment and Energy <br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")

## Viz background

bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Cassatt2")[4:2]))

map = ggplot2::ggplot(data = data_shp_joined) +
  scale_x_continuous(limits = c(19.7, 28.3)) +
  geom_sf(aes(fill = HeatOilCapita)) +
  
  # Eastern Macedonia and Thrace
  
  geom_sf_text(
    x = 25.2, y = 41.15, 
    label = data_shp_joined$HeatOilCapita[1], 
    size = 7.5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Attica (Athens)
  geom_sf_text(
    x = 23.8, y = 37.96, 
    label = data_shp_joined$HeatOilCapita[2], 
    size = 3.4, 
    family = "mont",
    fontface = "bold",
    angle = -50,
    color = "black"
  ) +
  # Western Greece
  geom_sf_text(
    x = 21.2, y = 38.65, 
    label = data_shp_joined$HeatOilCapita[3], 
    size = 4.5, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Western Macedonia
  
  geom_sf_text(
    x = 21.5, y = 40.4, 
    label = data_shp_joined$HeatOilCapita[4], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Ionian Islands
  
  geom_sf_text(
    x = 20.2, y = 39,
    label = data_shp_joined$HeatOilCapita[5], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Epirus
  
  geom_sf_text(
    x = 20.75, y = 39.6,
    label = data_shp_joined$HeatOilCapita[6], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Central Macedonia
  
  geom_sf_text(
    x = 23, y = 40.8, 
    label = data_shp_joined$HeatOilCapita[7], 
    size = 8.5, 
    family = "mont",
    fontface = "bold",
    color = "white"
  ) +
  
  # Crete
  
  geom_sf_text(
    x = 25.1, y = 35.2, 
    label = data_shp_joined$HeatOilCapita[8], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # South Aegean
  
  geom_sf_text(
    x = 26, y = 36.8, 
    label = data_shp_joined$HeatOilCapita[9], 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Peloponnese
  
  geom_sf_text(
    x = 22.4, y = 37.45, 
    label = data_shp_joined$HeatOilCapita[10], 
    size = 5.4, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Central Greece
  
  geom_sf_text(
    x = 22.65, y = 38.6, 
    label = data_shp_joined$HeatOilCapita[11], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -25
  ) +
  
  # Thessaly
  
  geom_sf_text(
    x = 22.15, y = 39.5, 
    label = data_shp_joined$HeatOilCapita[12], 
    size = 7, 
    family = "mont",
    fontface = "bold",
    angle = 0
  ) +
  
  # North Aegean
  
  geom_sf_text(
    x = 25.9, y = 39.2, 
    label = data_shp_joined$HeatOilCapita[13], 
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
    plot.background = element_rect(fill = bg_gradient, color = "transparent")
)

ggsave(
  filename = "GIF-5/GIF-5.png",
  height = 7,
  width = 5.165,
  plot = map,
  device = "png",
  dpi = 300)
