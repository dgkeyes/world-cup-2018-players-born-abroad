library(tidyverse)
library(janitor)
library(leaflet)


rosters <- read_csv("data/rosters_final.csv")    


# Add born in/outside country ---------------------------------------------

rosters <- rosters %>%
     mutate(born_in_country = if_else(str_detect(birthplace, team), 
                                      "Y",
                                      "N")) %>%
     # Deal with former Soviet countries 
     mutate(born_in_country = case_when(
          str_detect(birthplace, "Yugoslavia") & team == "Croatia" ~ "Y",
          str_detect(birthplace, "Soviet Union") & team == "Russia" ~ "Y",
          TRUE ~ born_in_country
     ))

born_in_country_df <- rosters %>%
     group_by(team, born_in_country) %>%
     summarize(n = n()) %>%
     filter(born_in_country == "Y")


# Make maps ---------------------------------------------------------------


dk.blue <- "#004c97"
dk.orange <- "#DD8233"

world_map <- map_data("world") %>%
     filter(region != "Antarctica")


temp %>%
     mutate(lon = jitter(lon, factor = 2)) %>%
     mutate(lat = jitter(lat, factor = 2)) %>%
     leaflet() %>%
     addProviderTiles(providers$CartoDB.Positron) %>%
     addCircleMarkers(
          color = case_when(
               temp$born_in_country == "Y" ~ dk.blue,
               temp$born_in_country == "N" ~ dk.orange
          ),
          popup = paste("Player: ", 
                        temp$player, 
                        "<br>",
                        "Birthplace: ",
                         temp$birthplace,
                        sep = ""),
          fillOpacity = .75)

ggplot() +
     geom_map(data = world_map, 
              map = world_map,
              aes(x = long, 
                  y = lat, 
                  map_id = region),
              color="white", 
              fill="#7f7f7f", 
              # size=0.05, 
              alpha=1/4) +
     geom_map(data = filter(world_map, region == "Egypt"), 
              map = filter(world_map, region == "Egypt"),
              aes(x = long, 
                  y = lat, 
                  map_id = region),
              color="white", 
              fill=dk.blue,
              alpha = 0.5) +
     geom_jitter(data = temp,
                aes(x = lon, 
                    y = lat),
                color = dk.orange) +
     coord_quickmap() +
     theme_void() +
     labs(title = "Egypt")
