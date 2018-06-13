library(tidyverse)
library(rvest)
library(janitor)
library(ggmap)



# Get links to each player's Wikipedia page --------------------------------


wiki_rosters_url <- read_html("https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads")

player_links <- wiki_rosters_url %>%
     html_nodes("th > a") %>%
     html_attr("href") 

# Remove links that aren't to players' pages

player_links <- player_links[1:736]




# Get rosters data  -------------------------------------------------------

# See https://www.kaggle.com/cclayford/2018-fifa-world-cup-squads

rosters <- read_csv("data/rosters.csv") %>%
     clean_names() %>%
     mutate(player = str_replace(player, "\\(captain\\)", "")) %>%
     mutate(wiki_link = paste("https://en.wikipedia.org", player_links, sep = ""))


# Add birthplaces ---------------------------------------------------------

# I scraped them the first time, then saved to CSV to avoid having to do it again.

birthplace <- ""

for (i in 1:736) {

     player_birthplace <- read_html(rosters$wiki_link[i]) %>%
          html_node(".birthplace") %>%
          html_text()

     birthplace <- c(birthplace, player_birthplace)

}

birthplace <- birthplace[2:737]

birthplace <- as.data.frame(birthplace) %>%
     mutate(birthplace = str_replace_all(birthplace, "\\[1\\]", "")) %>%
     mutate(birthplace = str_replace_all(birthplace, "\\[2\\]", "")) %>%
     mutate(birthplace = str_replace_all(birthplace, "\\[3\\]", ""))

write_csv(birthplace, "data/birthplaces.csv")


# Read birthplace file back in --------------------------------------------

birthplaces <- read_csv("data/birthplaces.csv")

# Add birthplace back to rosters and geocode them

# register_google(key = "APIKEY")

rosters <- rosters %>%
     mutate(birthplace = birthplaces$birthplace) %>%
     # Manually add in birthplaces for players without them or those who need minor adjustments
     mutate(birthplace = case_when (
          player == "Mahmoud Hamdy" ~ "Belbais, Egypt",
          player == "Ahmed Khalil" ~ "Kairouan, Tunisia",
          player == "Jérôme Boateng" ~ "West Berlin, Germany",
          TRUE ~ birthplace
     )) %>%
     mutate_geocode(birthplace) %>%
     mutate(born_in_country = if_else(str_detect(birthplace, team), 
                                      "Y",
                                      "N")) %>%
     mutate(born_in_country = if_else(str_detect(birthplace, "Guadeloupe"), 
                                      "Y",
                                      born_in_country)) %>%
     # Deal with former Soviet countries 
     mutate(born_in_country = case_when(
          str_detect(birthplace, "Yugoslavia") & team == "Croatia" ~ "Y",
          str_detect(birthplace, "Yugoslavia") & team == "Serbia" ~ "Y",
          str_detect(birthplace, "Soviet Union") & team == "Russia" ~ "Y",
          TRUE ~ born_in_country
     )) %>%
     arrange(team)


# Write final data file ---------------------------------------------------

write_csv(rosters, "data/rosters_final.csv")

