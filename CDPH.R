# looking at CPDH budget stuff

library(tidyverse)
library(RSocrata)
library(scales)
library(ggthemes)

rm(list = ls())

city_budgets <- read_csv("data/budget_ordinances.csv")

salaries <- read_csv("data/titles_salaries.csv")

# isolate CDPH for 2021

CDPH_2021 <- city_budgets %>% 
  filter(department_number == "41" &
           year == 2021 &
           ordinance_amount > 5000000) %>% 
  select(fund_type,
         fund_description,
         appropriation_authority_description,
         appropriation_account_description,
         ordinance_amount
  )

CDPH_2021 %>% 
  group_by(fund_description) %>%
  summarize(total_app = sum(ordinance_amount)) %>%
  ggplot() +
  geom_col(aes(fct_reorder(fund_description, total_app), total_app)) +
  coord_flip()

# ~ 200 million for 'building epidemiology and heath IT capa

# isolate 2022 budget recommendations. for some reason the loop is not working for this year

CDPH_2022 <- read.socrata("https://data.cityofchicago.org/resource/ncj3-k47t.json")

CDPH_2022 %>% 
  mutate(appropriation = as.numeric(appropriation),
         revised_appropriation = as.numeric(revised_appropriation),
         recommendation = as.numeric(recommendation)) %>% 
  filter(department_number == "41" & 
           recommendation > 5000000) %>% 
  mutate(recommendation = recommendation / 1000000) %>% 
  group_by(fund_description) %>%
  summarize(total_app = sum(recommendation)) %>%
  ggplot() +
  geom_col(aes(fct_reorder(fund_description, total_app), 
               total_app,
               fill = total_app)
           ) +
  geom_text(aes(fund_description, 
                 total_app, 
                 label = paste0("$", round(total_app, 0))
                 ),
             nudge_y = 8
             ) +
  labs(title = "CDPH 2022 Recommended Budget by Fund",
       subtitle = "Millions of $ (greater than $5M)",
       caption = "source: https://data.cityofchicago.org/Administration-Finance/Budget-2022-Budget-Recommendations-Appropriations/ncj3-k47t",
       x = NULL,
       y = NULL
       ) +
  scale_fill_continuous_tableau() +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.title.position = "plot",
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# looking at just 2022, building capacity

IT_capacity <- CDPH_2022 %>% 
  filter(fund_description == "Building Epidemiology and Health It Capacity") %>% 
  view()

# ok, let's isolate 2021 budget and titles

budget_2021 <- read.socrata("https://data.cityofchicago.org/resource/6tbx-h7y2.json")

titles_2021 <- read.socrata("https://data.cityofchicago.org/resource/gcwx-xm5a.json")

CDPH_2021 <- budget_2021 %>% 
  filter(department_number == "41") %>% 
  view()

CDPH_titles_2021 <- titles_2021 %>% 
  filter(department_code == "41") %>% 
  view()

# ok, let's look again at 2022

budget_2022_rec <- read.socrata("https://data.cityofchicago.org/resource/ncj3-k47t.json")

titles_2022_rec <- read.socrata("https://data.cityofchicago.org/resource/kwap-s85k.json")

CDPH_2022 <- budget_2022_rec %>% 
  filter(department_number == "41") %>% 
  view()

CDPH_titles_2022 <- titles_2022_rec %>% 
  filter(department_code == "41") %>% 
  view()












