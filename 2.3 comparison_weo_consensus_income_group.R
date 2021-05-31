########## Script to compare income groups forecasts from Consensus and WEO

list_weights=c("adv","em","lidc") %>% 
  map_chr(~ paste0("../Forecasts_Time_Covid_material/intermediate_data/weights_",.x,".RDS")) %>% 
  map(~ read_rds(.x))

# Prepare weights dataframe for merging with comparison dataframe (need to have all horizons) -----
# Note: apply same weights for following months after WEO issue before the next issue


extrapolate_horizons <- function(df){
  df %>% 
    split(.$horizon) %>% 
    rep(c(2,3,4,3)) %>% 
    map2(c("Apr","May","Jan","Feb","Mar","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
         ~ .x %>% mutate(horizon = .y)) %>% 
    bind_rows()
}


comparison_list <- list_weights %>% 
  map(~ extrapolate_horizons(.x)) %>% 
  map(~ .x %>% merge(read_xlsx("../Forecasts_Time_Covid_material/intermediate_data/replication_figures/comparison_individual_countries.xlsx") %>% 
        rename(ifscode = country_code), by=c("ifscode","horizon"))) %>% 
  map(~ .x %>% as_tibble()) %>% 
  map(~ .x %>% mutate(imf_weighted = imf*weight,
         consensus_weighted = consensus*weight )) %>% 
  map(~ .x %>% group_by(horizon)) %>% 
  map(~ .x %>% summarise(group_consensus = sum(consensus_weighted,na.rm = T),
            group_imf = sum(imf_weighted, na.rm = T))) %>% 
  map(~ .x %>% mutate(group_imf = case_when(group_imf == 0 ~ NA_real_,
                                T ~ group_imf))) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec")))) %>% 
  map(~ .x %>% gather("institution","value",group_consensus:group_imf)) %>% 
  map(~ .x %>% mutate(institution = case_when(institution == "group_consensus" ~ "Consensus",
                                 institution == "group_imf" ~ "WEO"))) 


  


# Plot and export (both plot and df for construction): -----

comparison_list %>% 
  map(~ .x %>% ggplot(aes(horizon,value, col = institution)) +
  geom_vline(xintercept = c("Jan","Apr","Jun","Oct"), size = 15, col = "gray", alpha = 0.7) + 
  geom_point(size = 3, alpha = 0.8) +
  ylab("Real GDP Growth Forecast (%)") +
  xlab("") +
  labs(col = "",
       linetype = "") +
  scale_color_manual(values = c("#4472C4","#ED7D31")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21)))



ggsave("../Forecasts_Time_Covid_material/output/figures/aggregate_comparison/Global.pdf",
       height = 5.8,
       width = 12.3)

export(global_comparison_df, "../Forecasts_Time_Covid_material/intermediate_data/replication_figures/comparison_global.xlsx")

