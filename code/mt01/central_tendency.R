

library(tidyverse)

dta <- read.csv("data/parlgov_elections.csv",
                fileEncoding = "utf-8")

dta_ireland <- dta %>% 
  filter(country_name == "Ireland") %>% 
  filter(party_name %in% c("Fine Gael", "Fianna Fáil", "Labour Party")) %>% 
  filter(election_type == "parliament") %>% 
  select(country_name:party_name_short) %>% 
  mutate(country_name = as.factor(country_name),
         election_type = as.factor(election_type),
         party_name_short = as.factor(party_name_short))

ggplot(dta_ireland, aes(x = as.Date(election_date), 
                        y = vote_share, 
                        colour = party_name_short)) +
  geom_line(size = 1.4, alpha = 0.8) +
  scale_colour_manual(values = c("darkgreen", "blue", "darkred")) +
  labs(x = "Year", y = "Percentage vote share") +
  theme(legend.title = element_blank()) +
  theme_bw()

rio::export(characterize(dta_ireland), "data/data_ire_elections.sav")
