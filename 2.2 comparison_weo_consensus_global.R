########## Script to compare global growth forecasts from Consensus and WEO


df_weights_global=readRDS("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global.RDS")


# Prepare weights dataframe for merging with comparison dataframe (need to have all horizons) -----
# Note: apply same weights for following months after WEO issue before the next issue

weights_global_comparison <- df_weights_global %>% 
  split(.$horizon) %>% 
  rep(c(2,3,4,3)) %>% 
  map2(c("Apr","May","Jan","Feb","Mar","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
       ~ .x %>% mutate(horizon = .y)) %>% 
  bind_rows()


  
# Combine comparison dataframe with weight dataframes: -----


global_comparison_df <- read_xlsx("../Forecasts_Time_Covid_material/intermediate_data/replication_figures/individual_comparison/consensus.xlsx") %>% 
  merge(weights_global_comparison, by=c("country_code","horizon")) %>% 
  merge(readRDS("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global_actual.RDS"), by=c("country_code")) %>% 
  as_tibble() %>%
  mutate(imf_weighted = imf*weight,
         consensus_weighted = consensus*weight,
         actual_weighted = actual*actual_weight) %>% 
  group_by(horizon) %>% 
  summarise(global_consensus = sum(consensus_weighted,na.rm = T),
            global_imf = sum(imf_weighted, na.rm = T),
            global_actual = sum(actual_weighted, na.rm = T)) %>% 
  mutate(global_imf = case_when(global_imf == 0 ~ NA_real_,
                                T ~ global_imf)) %>% 
  mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                                           "Sep","Oct","Nov","Dec"))) %>% 
  gather("institution","value",global_consensus:global_imf) %>% 
  mutate(institution = case_when(institution == "global_consensus" ~ "Consensus",
                                              institution == "global_imf" ~ "WEO")) 



# Plot and export (both plot and df for construction): -----

global_comparison_df %>% 
  ggplot(aes(horizon,value, col = institution)) +
  geom_hline(aes(yintercept = global_actual, linetype = "Actual"), size = 1.5, col = "gray") +
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
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))



ggsave("../Forecasts_Time_Covid_material/output/figures/aggregate_comparison/consensus/Global.pdf",
       height = 5.7,
       width = 11)

export(global_comparison_df, "../Forecasts_Time_Covid_material/intermediate_data/replication_figures/aggregate_comparison/consensus/global.xlsx")



