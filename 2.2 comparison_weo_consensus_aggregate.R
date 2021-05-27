# Comparison between aggregates:
# have to build 2020 weights from January, April, June and October 2020 WEO
# will extrapolate for the months not covered in Consensus


weights_global_comparison <- df_weights_global %>% 
  split(.$horizon) %>% 
  rep(c(2,3,4,3)) %>% 
  map2(c("Apr","May","Jan","Feb","Mar","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
       ~ .x %>% mutate(horizon = .y)) %>% 
  bind_rows()


  

read_xlsx("../Forecasts_Time_Covid_material/intermediate_data/replication_figures/comparison_individual_countries.xlsx") %>% 
  merge(weights_global_comparison, by=c("country_code","horizon")) %>% 
  merge(df_weights_global_actual, by=c("country_code")) %>% 
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
                                              institution == "global_imf" ~ "WEO")) %>% 
  ggplot(aes(horizon,value, col = institution)) +
  geom_vline(xintercept = c("Jan","Apr","Jun","Oct"), size = 15, col = "gray", alpha = 0.7) + 
  geom_hline(aes(yintercept = global_actual, linetype = "Actual"), size = 1.5) +
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
        axis.title = element_text(size = 21))



ggsave("../Forecasts_Time_Covid_material/output/figures/aggregate_comparison/Global.pdf",
                 height = 5.8,
                 width = 12.3)




