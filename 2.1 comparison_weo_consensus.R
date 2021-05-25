# Combine Consensus and WEO

names_df=c("consensus_2020","weo_2020")


comparison_df <- names_df %>% 
  map(~ readRDS(paste0("../Forecasts_Time_Covid_material/intermediate_data/",.x,".RDS"))) %>% 
  reduce(merge, by=c("country","horizon"), all.x = T) %>%
  as_tibble() 

  
  

individual_countries=c("Brazil","India","United States")  

individual_countries %>% 
  map(~ comparison_df %>% filter(country == .x)) %>% 
  map(~ .x %>% setNames(c("country","horizon","consensus","country_code","year","imf"))) %>% 
  map(~ .x %>% select(country,horizon, consensus,imf)) %>% 
  map(~ .x %>% gather("institution","value",consensus:imf)) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                                           "Sep","Oct","Nov","Dec")))) %>% 
  map(~ .x %>% mutate(institution = case_when(institution == "consensus" ~ "Consensus",
                                              institution == "imf" ~ "WEO"))) %>% 
  map(~ .x %>% 
        ggplot(aes(horizon,value, col = institution)) +
        geom_vline(xintercept = c("Jan","Apr","Jun","Oct"), size = 15, col = "gray", alpha = 0.7) + 
        geom_point(size = 3, alpha = 0.8) +
        ylab("Real GDP Growth Forecast (%)") +
        xlab("") +
        labs(col = "") +
        scale_color_manual(values = c("#4472C4","#ED7D31")) +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 18),
              axis.title = element_text(size = 21))
      )

