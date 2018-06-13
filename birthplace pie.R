
# Make summary data frame for players born abroad -------------------------

players_born_abroad_summary <- rosters %>%
     mutate(born_in_country = str_replace(born_in_country, "N", "Born outside country")) %>%
     mutate(born_in_country = str_replace(born_in_country, "Y", "Born in country")) %>%
     group_by(born_in_country, team) %>%
     summarize(n = n()) 


ggplot(players_born_abroad_summary,
       aes(x = "", 
           y = n,
           fill = factor(born_in_country))) +
     geom_bar(stat = "identity",
              width = 1) +
     geom_text(data = filter(players_born_abroad_summary, 
                             born_in_country == "Born outside country"),
               aes(label = n),
               position = position_stack(vjust = 0.5),
               color = "white",
               fontface = "bold") +
     coord_polar("y") +
     theme_void() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5,
                                     face = "bold",
                                     color = dk.orange),
           plot.margin=unit(c(.5, .5, .5, .5),"cm"),
           plot.caption = element_text(color = "#505050"),
           strip.text = element_text(face = "bold")) +
     scale_fill_manual(values = c(dk.gray, dk.orange)) +
     facet_wrap(~team, ncol = 8) +
     labs(title = "Number of players born abroad on each World Cup 2018 team",
          caption = "dgkeyes.com")

ggsave("players-born-abroad.png",
       height = 8, width = 12, scale = .75)
