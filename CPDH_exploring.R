# categorize funds to collapse data and such

# 
temp <- c("Hepatitis", 
          "HIV",
          "Lead",
          "Air",
          "Tuberculosis",
          "")

CDPH_22_funds <- budget_ordinances_clean %>% 
  filter(department_number == "41" & year == 2022) %>% 
  group_by(fund_description,
           appropriation_account_description) %>% 
  summarize(total_app = sum(ordinance_amount)) %>% 
  # mutate(
  #   fund_cat = case_when(
  #     str_detect(fund_description, "Hepatitis") ~ "Hepatitis",
  #     str_detect(fund_description, "HIV") ~ "HIV",
  #     str_detect(fund_description, "Lead|Air") ~ "Environment",
  #     str_detect(fund_description, "Tuberculosis") ~ "Tuberculosis",
  #     TRUE ~ fund_description
  #     )
  #       ) %>% 
  view()

# looking at non-grants

CDPH_22_NG <- budget_ordinances_clean %>% 
  filter(department_number == "41" & 
           year == 2022 & 
           fund_type != "GRANTS" &
           ordinance_amount > 0)
         
CDPH_22_NG %>%
  # group_by(appropriation_account_description) %>% 
  # summarize(total_app = sum(ordinance_amount)) %>% 
  ggplot() +
  geom_col(
    aes(
      fct_reorder(appropriation_account_description, ordinance_amount), 
      ordinance_amount)
    ) +
  # facet_wrap(~ fund_description) +
  coord_flip()
