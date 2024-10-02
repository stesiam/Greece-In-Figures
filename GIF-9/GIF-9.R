library(readxl)
library(dplyr)
library(tidyr)

library(sf)

url = "https://www.statistics.gr/el/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=2&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_documentID=532489&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_locale=el"


download.file(url = url,
              destfile = "GIF-9/households.xlsx")

households = read_excel("GIF-9/households.xlsx", 
                                      skip = 4) |>
  select(-c(1,2)) |>
  setNames(c("Region","Households", paste("Members_",1:10), "totalpop")) |>
  pivot_longer(cols = starts_with("Members"),
             names_to = "Members",
             values_to = "Observations") |>
  rename(NAMA2 = Region) %>%
  dplyr::filter(str_detect(NAMA2 , "ΠΕΡΙΦΕΡΕΙΑ")) |>
  dplyr::filter(!str_detect(NAMA2, "ΕΝΟΤΗΤΑ")) |>
  mutate(
    NAMA2 = stringr::str_remove_all(NAMA2, "ΠΕΡΙΦΕΡΕΙΑ")
  ) |>
  mutate(NAMA2 = tolower(NAMA2)) %>%
  mutate(NAMA2 = transliterate_greek_to_english(NAMA2)) |>
  group_by(NAMA2) |>
  summarise(
    mean_household = round(totalpop/Households, digits = 2)
  ) |>
  select(NAMA2, mean_household) |>
  distinct(NAMA2, .keep_all = TRUE)
  


  
shapefile <- st_read("Shapefiles/GR_bsm_admn_adm2_py_EuroGeoGraphics_2015_pp.shp") |>
  dplyr::filter(NAMA2 != "Agion Oros")
  
d = fuzzyjoin::stringdist_left_join(shapefile, households , by = "NAMA2", 
                                    max_dist = 7, distance_col = "distance")|>
  group_by(NAMA2.x) |>
  dplyr::filter(distance == min(distance)) |>
  select(NAMA2.x, mean_household, geometry) |>
  arrange(NAMA2.x) |>
  st_as_sf()

## Viz texts

title_text = glue("<b>Household Members</b>")
subtitle_text = glue("Electric energy consumption by great geographic area, region and department and by category of use:2012")
caption_text = glue("*Data of:* 2021<br><b> Source:</b> Hellenic Statistical Authority - Census 2021<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")

## Viz background

bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Cross")[7:8]))

## Make map

map = ggplot2::ggplot(data = d) +
  scale_x_continuous(limits = c(19.7, 28.3)) +
  geom_sf(aes(fill = mean_household)) +
  
  # Eastern Macedonia and Thrace
  
  geom_sf_text(
    x = 25.2, y = 41.15, 
    label = d$mean_household[1], 
    size = 7.5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Attica (Athens)
  geom_sf_text(
    x = 23.8, y = 37.96, 
    label = d$mean_household[2], 
    size = 3.4, 
    family = "mont",
    fontface = "bold",
    angle = -50,
  ) +
  # Western Greece
  geom_sf_text(
    x = 21.2, y = 38.65, 
    label = d$mean_household[3], 
    size = 4.5, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Western Macedonia
  
  geom_sf_text(
    x = 21.5, y = 40.4, 
    label = d$mean_household[4], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Ionian Islands
  
  geom_sf_text(
    x = 20.2, y = 39,
    label = d$mean_household[5], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Epirus
  
  geom_sf_text(
    x = 20.75, y = 39.6,
    label = d$mean_household[6], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Central Macedonia
  
  geom_sf_text(
    x = 23, y = 40.8, 
    label = d$mean_household[7], 
    size = 8.5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Crete
  
  geom_sf_text(
    x = 25.1, y = 35.2, 
    label = d$mean_household[8], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # South Aegean
  
  geom_sf_text(
    x = 26, y = 36.8, 
    label = d$mean_household[9], 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Peloponnese
  
  geom_sf_text(
    x = 22.4, y = 37.45, 
    label = d$mean_household[10], 
    size = 5.4, 
    family = "mont",
    fontface = "bold",
    angle = -10,
    color = "black"
  ) +
  
  # Central Greece
  
  geom_sf_text(
    x = 22.65, y = 38.6, 
    label = d$mean_household[11], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -25
  ) +
  
  # Thessaly
  
  geom_sf_text(
    x = 22.15, y = 39.5, 
    label = d$mean_household[12], 
    size = 7, 
    family = "mont",
    fontface = "bold",
    angle = 0
  ) +
  
  # North Aegean
  
  geom_sf_text(
    x = 25.9, y = 39.2, 
    label = d$mean_household[13], 
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
  scale_fill_gradientn(colors = c("grey90", "#CC5500")) +
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
  filename = "GIF-9/GIF-9.png",
  height = 7,
  width = 5.44,
  plot = map,
  device = "png",
  dpi = 300)

