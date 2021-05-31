###### Script to compare individual country forecasts from WEO and Consensus


# Combine Consensus and WEO intermediate data -----

names_df=c("consensus_2020","weo_2020")


comparison_df <- names_df %>% 
  map(~ readRDS(paste0("../Forecasts_Time_Covid_material/intermediate_data/",.x,".RDS"))) %>% 
  reduce(merge, by=c("country","horizon"), all.x = T) %>% #keep only data availability for Consensus
  as_tibble() %>% 
  setNames(c("country","horizon","consensus","country_code","year","actual","imf")) %>% 
  split(.$country) %>% 
  map(~ .x %>% mutate(actual = case_when(is.na(actual) ~ unique(actual)[1],
                                         T ~ actual))) %>%
  map(~ .x %>% mutate(country_code = case_when(is.na(country_code) ~ unique(country_code)[1],
                                         T ~ country_code))) %>%
  bind_rows() %>% 
  select(country_code, country, horizon, actual, consensus,imf)


export(comparison_df,"../Forecasts_Time_Covid_material/intermediate_data/replication_figures/comparison_individual_countries.xlsx")


# Run comparison ------

# List of individual countries:
  
individual_countries=c("Brazil","India","United States")  


# Plot:

individual_plots <- individual_countries %>% 
  map(~ comparison_df %>% filter(country == .x)) %>% 
  map(~ .x %>% gather("institution","value",consensus:imf)) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                                           "Sep","Oct","Nov","Dec")))) %>% 
  map(~ .x %>% mutate(institution = case_when(institution == "consensus" ~ "Consensus",
                                              institution == "imf" ~ "WEO"))) %>% 
  map(~ .x %>% 
        ggplot(aes(horizon,value, col = institution)) +
        geom_hline(aes(yintercept = actual, linetype = "Actual"), size = 1.5, col = "gray") +
        geom_point(size = 3, alpha = 0.8) +
        ylab("Real GDP Growth Forecast (%)") +
        xlab("") +
        labs(col = "",
             linetype = "") +
        scale_color_manual(values = c("#4472C4","#ED7D31")) +
        scale_linetype_manual(values = "dotted") +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 18),
              axis.title = element_text(size = 21))
      )


# Export:

names(individual_plots) = individual_countries

individual_plots %>% 
  iwalk(~ ggsave(paste0("../Forecasts_Time_Covid_material/output/figures/individual_comparison/",.y,".pdf"),
                 .x,
                 height = 5.7,
                 width = 11))




