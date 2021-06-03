########## Script to compare income groups forecasts from Consensus and WEO

income_groups=c("adv","em","lidc")

list_weights_group=income_groups %>% 
  map_chr(~ paste0("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_",.x,".RDS")) %>% 
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

# List of dataframe with actuals: -----

list_df_actuals <- income_groups %>% 
  map_chr(~ paste0("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_",.x,"_actual.RDS")) %>% 
  map(~ readRDS(.x)) %>% 
  map(~ as_tibble(.x))




# Combine comparison dataframes with weights dataframes: -----


comparison_list_group <- list_weights_group %>% 
  map(~ extrapolate_horizons(.x)) %>% 
  map(~ .x %>% merge(read_xlsx("../Forecasts_Time_Covid_material/intermediate_data/replication_figures/comparison_individual_countries.xlsx") %>% 
        rename(ifscode = country_code), by=c("ifscode","horizon"))) %>%
  map2(list_df_actuals, ~ .x %>% merge(.y, by=c("ifscode"))) %>% 
  map(~ .x %>% as_tibble()) %>% 
  map(~ .x %>% mutate(imf_weighted = imf*weight,
         consensus_weighted = consensus*weight,
         actual_weighted = actual*actual_weight)) %>% 
  map(~ .x %>% group_by(horizon)) %>% 
  map(~ .x %>% summarise(group_consensus = sum(consensus_weighted,na.rm = T),
            group_imf = sum(imf_weighted, na.rm = T),
            group_actual = sum(actual_weighted, na.rm = T))) %>% 
  map(~ .x %>% mutate(group_imf = case_when(group_imf == 0 ~ NA_real_,
                                T ~ group_imf))) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec")))) %>% 
  map(~ .x %>% gather("institution","value",group_consensus:group_imf)) %>% 
  map(~ .x %>% mutate(institution = case_when(institution == "group_consensus" ~ "Consensus",
                                 institution == "group_imf" ~ "WEO"))) 


  
names(comparison_list_group)=income_groups


# Plot and export (both plot and df for construction): -----

list_plots_income <- comparison_list_group %>% 
  map(~ .x %>% 
  ggplot(aes(horizon,value, col = institution)) +
  geom_hline(aes(yintercept = group_actual, linetype = "Actual"), size = 1.5, col = "gray") +
  geom_point(size = 3, alpha = 0.8) +
  ylab("Real GDP Growth Forecast (%)") +
  xlab("") +
  labs(col = "",
       linetype = "") +
  scale_color_manual(values = c("#4472C4","#ED7D31")) +
  scale_linetype_manual(values="dotted") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21)))



# Export:



# Plots:

list_plots_income %>% 
  iwalk(~ ggsave(paste0("../Forecasts_Time_Covid_material/output/figures/aggregate_comparison/consensus/",.y,".pdf"),
                 .x,
                 height = 5.7,
                 width = 11))

# Data:

comparison_list_group %>% 
  iwalk(~ export(.x,paste0("../Forecasts_Time_Covid_material/intermediate_data/replication_figures/aggregate_comparison/consensus/",.y,".xlsx")))



 

