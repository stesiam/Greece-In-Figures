library(readr)
library(tidyr)
library(stringi)
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

transliterate_greek_to_english <- function(greek_string) {
  
  # Step 1: Create a named vector for removing accents from Greek letters
  accent_removal <- c(
    "Ά" = "Α", "ά" = "α",
    "Έ" = "Ε", "έ" = "ε",
    "Ή" = "Η", "ή" = "η",
    "Ί" = "Ι", "ί" = "ι", "ΐ" = "ι", "ϊ" = "ι",
    "Ό" = "Ο", "ό" = "ο",
    "Ύ" = "Υ", "ύ" = "υ", "ΰ" = "υ", "ϋ" = "υ",
    "Ώ" = "Ω", "ώ" = "ω"
  )
  
  # Replace accented Greek letters with their non-accented versions
  for (accented_letter in names(accent_removal)) {
    greek_string <- gsub(accented_letter, accent_removal[accented_letter], greek_string)
  }
  
  # Step 2: Create a named vector for exact Greek to English transliteration
  greek_to_english <- c(
    "Α" = "A", "α" = "a",
    "Β" = "B", "β" = "b",
    "Γ" = "G", "γ" = "g",
    "Δ" = "D", "δ" = "d",
    "Ε" = "E", "ε" = "e",
    "Ζ" = "Z", "ζ" = "z",
    "Η" = "I", "η" = "i",
    "Θ" = "Th", "θ" = "th",
    "Ι" = "I", "ι" = "i",
    "Κ" = "K", "κ" = "k",
    "Λ" = "L", "λ" = "l",
    "Μ" = "M", "μ" = "m",
    "Ν" = "N", "ν" = "n",
    "Ξ" = "X", "ξ" = "x",
    "Ο" = "O", "ο" = "o",
    "Π" = "P", "π" = "p",
    "Ρ" = "R", "ρ" = "r",
    "Σ" = "S", "σ" = "s", "ς" = "s",
    "Τ" = "T", "τ" = "t",
    "Υ" = "Y", "υ" = "y",
    "Φ" = "F", "φ" = "f",
    "Χ" = "Ch", "χ" = "ch",
    "Ψ" = "Ps", "ψ" = "ps",
    "Ω" = "O", "ω" = "o"
  )
  
  # Step 3: Replace each Greek letter with its English equivalent
  for (greek_letter in names(greek_to_english)) {
    greek_string <- gsub(greek_letter, greek_to_english[greek_letter], greek_string)
  }
  
  return(greek_string)
}

here::here("GIF-6")

shapefile <- st_read("Shapefiles/GR_bsm_admn_adm2_py_EuroGeoGraphics_2015_pp.shp")


## Download and load dataset

download.file("https://www.statistics.gr/en/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=2&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_documentID=106329&_documents_WAR_publicationsportlet_INSTANCE_VBZOni0vs5VJ_locale=en",
              destfile = "GIF-6/electricity_2012.xls")

download.file(url = "https://elstat-outsourcers.statistics.gr/census_results_2022_en.pdf",
              destfile = "GIF-6/census.pdf")

pdf_subset("GIF-6/census.pdf",
           pages = 6,  output = "GIF-6/subset.pdf")


statistics_tables <- extract_tables(
  file   = "GIF-6/subset.pdf", 
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



electricity_consumption_raw = read_excel("GIF-6/electricity_2012.xls", 
                                         skip = 4) |>
  select(c(1,3, last_col())) |>
  setNames(c("RegionGR", "Electricity", "RegionEN")) |>
  drop_na() %>%
  mutate(
    Region = transliterate_greek_to_english(RegionGR)
  ) |>
  select(Region, Electricity)



# Join electricity and regional population datasets

joined_data = stringdist_left_join(regional_population, electricity_consumption_raw, 
                                   by = "Region", 
                                   distance_col = "distance",
                                   max_dist = 10) |>
  group_by(Region.x) |>
  dplyr::filter(distance == min(distance)) |>
  select(Region.x, Electricity, Population) |>
  rename(NAMA2 = Region.x) |>
  mutate(ElectricityCapita = round(1000*Electricity/as.numeric(Population), digits = 0)) |>
  distinct(NAMA2, .keep_all = TRUE)


#

joined_data_shp = stringdist_left_join(shapefile, joined_data, 
                                       by = "NAMA2", 
                                       distance_col = "distance",
                                       max_dist = 10) |>
  group_by(NAMA2.x) |>
  dplyr::filter(distance == min(distance)) |>
  distinct(NAMA2.x,.keep_all = TRUE) |>
  dplyr::filter(NAMA2.x != "Agion Oros") |>
  select(NAMA2.x, ElectricityCapita, geometry) |>
  rename(Region = NAMA2.x) |>
  arrange(Region) |>
  st_as_sf()


## Viz texts

title_text = glue("<b>Κατανάλωση Ρεύματος</b>")
subtitle_text = glue("Η περιφέρεια με τη μεγαλύτερη κατανάλωση ρεύματος ανά κάτοικο είναι της Πελοπονήσου και ακολουθείται από την Αττική.
                     Οι υπόλοιπες έχουν σημαντικά χαμηλότερη κατανάλωση. Αξίζει να σημειωθεί ότι τα δεδομένα αφορούν αποκλειστικά την οικιακή χρήση.")
caption_text = glue("*Μονάδα μέτρησης:* Κιλοβατώρες/κάτοικο <br>*Δεδομένα χρονιάς:* 2012 - Περιφερειακός πληθυσμός βασίζεται στην απογραφή του 2021<br><b> Source:</b> Ελληνική Στατιστική Υπηρεσία<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2023")

## Viz background

bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Morgenstern")[7:8]))

## Make map

map = ggplot2::ggplot(data = joined_data_shp) +
  scale_x_continuous(limits = c(19.7, 28.3)) +
  geom_sf(aes(fill = ElectricityCapita)) +
  
  # Eastern Macedonia and Thrace
  
  geom_sf_text(
    x = 25.2, y = 41.15, 
    label = joined_data_shp$ElectricityCapita[1], 
    size = 7.5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Attica (Athens)
  geom_sf_text(
    x = 23.8, y = 37.96, 
    label = joined_data_shp$ElectricityCapita[2], 
    size = 3.4, 
    family = "mont",
    fontface = "bold",
    angle = -50,
  ) +
  # Western Greece
  geom_sf_text(
    x = 21.2, y = 38.65, 
    label = joined_data_shp$ElectricityCapita[3], 
    size = 4.5, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Western Macedonia
  
  geom_sf_text(
    x = 21.5, y = 40.4, 
    label = joined_data_shp$ElectricityCapita[4], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Ionian Islands
  
  geom_sf_text(
    x = 20.2, y = 39,
    label = joined_data_shp$ElectricityCapita[5], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -60
  ) +
  
  # Epirus
  
  geom_sf_text(
    x = 20.75, y = 39.6,
    label = joined_data_shp$ElectricityCapita[6], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Central Macedonia
  
  geom_sf_text(
    x = 23, y = 40.8, 
    label = joined_data_shp$ElectricityCapita[7], 
    size = 8.5, 
    family = "mont",
    fontface = "bold",
    color = "black"
  ) +
  
  # Crete
  
  geom_sf_text(
    x = 25.1, y = 35.2, 
    label = joined_data_shp$ElectricityCapita[8], 
    size = 5, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # South Aegean
  
  geom_sf_text(
    x = 26, y = 36.8, 
    label = joined_data_shp$ElectricityCapita[9], 
    size = 10, 
    family = "mont",
    fontface = "bold",
    angle = -10
  ) +
  
  # Peloponnese
  
  geom_sf_text(
    x = 22.4, y = 37.45, 
    label = joined_data_shp$ElectricityCapita[10], 
    size = 5.4, 
    family = "mont",
    fontface = "bold",
    angle = -10,
    color = "black"
  ) +
  
  # Central Greece
  
  geom_sf_text(
    x = 22.65, y = 38.6, 
    label = joined_data_shp$ElectricityCapita[11], 
    size = 6, 
    family = "mont",
    fontface = "bold",
    angle = -25
  ) +
  
  # Thessaly
  
  geom_sf_text(
    x = 22.15, y = 39.5, 
    label = joined_data_shp$ElectricityCapita[12], 
    size = 7, 
    family = "mont",
    fontface = "bold",
    angle = 0
  ) +
  
  # North Aegean
  
  geom_sf_text(
    x = 25.9, y = 39.2, 
    label = joined_data_shp$ElectricityCapita[13], 
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
  scale_fill_gradient(low = "grey90", high = "yellow2") +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_markdown(family = "serif", hjust = 0.5, size = 20,
                                  margin = margin(t = 20, b = 10),
                                  color = "white"),
    plot.subtitle = element_textbox_simple(size = 9, family = "serif", hjust = 0.5, 
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
  filename = "GIF-6/GIF-6-el.png",
  height = 7,
  width = 5.46,
  plot = map,
  device = "png",
  dpi = 300)
