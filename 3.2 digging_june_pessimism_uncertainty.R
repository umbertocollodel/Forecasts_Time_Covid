# Script to show evolution of Consensus weighted standard deviation over time


# Read Consensus sd dataframe ----

consesus_sd=readRDS("../Forecasts_Time_Covid_material/intermediate_data/consensus_2020_sd.RDS") %>% 
  mutate(country_code = countrycode(country,"country.name","imf")) %>% 
  filter(complete.cases(country_code))

# Figure: individual countries uncertainty evolution ------

selected_countries=c("Brazil","United States","China")


consesus_sd %>% 
  filter(country %in% selected_countries) %>% 
  mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec"))) %>% 
  ggplot(aes(horizon,value, group = country,col = country)) +
  geom_line(size = 2,alpha = 0.8) +
  xlab("") +
  ylab("Sd (%)") +
  labs(col= "") +
  scale_color_manual(values = c("#4472C4","#ED7D31","#92D050")) +
  theme_minimal() +
  theme(legend.position = "bottom",legend.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))
  

ggsave("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/evolution_consensus_sd_individual.pdf",
       height = 5.7,
       width = 11)
  
  


# Prepare weights dataframe for merging with Consensus sd -----
# Note: apply same weights for following months after WEO issue before the next issue

df_weights_global=readRDS("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global.RDS")

weights_global_comparison <- df_weights_global %>% 
  split(.$horizon) %>% 
  rep(c(2,3,4,3)) %>% 
  map2(c("Apr","May","Jan","Feb","Mar","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
       ~ .x %>% mutate(horizon = .y)) %>% 
  bind_rows()

# Combine and plot weighted sd evolution ----

consesus_sd %>% 
  merge(weights_global_comparison, by=c("country_code","horizon")) %>% 
  mutate(weighted_sd = value*weight) %>% 
  group_by(horizon) %>% 
  summarise(global_sd = sum(weighted_sd)) %>% 
  mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec"))) %>% 
  ggplot(aes(horizon, global_sd)) +
  geom_point(size = 3, col = "#4472C4",alpha = 0.8) +
  xlab("") +
  ylab("Weighted Sd (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21))


# Export: 

ggsave("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/evolution_Consensus_sd.pdf",
       height = 5.7,
       width = 11)
