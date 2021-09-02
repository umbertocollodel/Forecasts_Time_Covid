######### Script to show evolution of global Consensus weighted standard deviation and
######### individual case studies for different variables over 2020 horizon


# Read Consensus sd dataframes: ----

consesus_sd=list(growth = readRDS("../Forecasts_Time_Covid_material/intermediate_data/consensus_2020_sd.RDS"),
                 inflation = readRDS("../Forecasts_Time_Covid_material/intermediate_data/consensus_2020_inflation_sd.RDS")) %>% 
  map(~ .x %>% mutate(country_code = countrycode(country,"country.name","imf"))) %>% 
  map(~ .x %>% filter(complete.cases(country_code))) %>% 
  suppressWarnings()

# Figure: individual countries standard deviation ------

selected_countries=c("Brazil","United States","China")

# Plot:

individual_sd <- consesus_sd %>% 
  map(~ .x %>% filter(country %in% selected_countries)) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec")))) %>% 
  map(~ .x %>% ggplot(aes(horizon,value, group = country,col = country)) +
  geom_line(size = 2,alpha = 0.8) +
  xlab("") +
  ylab("Sd (%)") +
  labs(col= "") +
  ylim(0,2) +
  scale_color_manual(values = c("#4472C4","#ED7D31","#92D050")) +
  theme_minimal() +
  theme(legend.position = "bottom",legend.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21)))

# Export: 

individual_sd %>% 
  iwalk(~ ggsave(paste0("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/consensus_sd_individual_",.y,".pdf"),
                .x,
                height = 5.7,
                width = 11))

  
  


# Prepare weights dataframe for merging with Consensus sd -----
# Note: apply same weights for following months after WEO issue before the next issue

df_weights_global=readRDS("../Forecasts_Time_Covid_material/intermediate_data/weights_aggregates/weights_global.RDS")

weights_global_comparison <- df_weights_global %>% 
  split(.$horizon) %>% 
  rep(c(2,3,4,3)) %>% 
  map2(c("Apr","May","Jan","Feb","Mar","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
       ~ .x %>% mutate(horizon = .y)) %>% 
  bind_rows()

# Figure: global standard deviation (calculated with weights and simple median) ----
# Note: with weights inflation results highly influenced by outliers, better to use median value for
# comparison

global_sd_df <- consesus_sd %>% 
  map(~ .x %>% merge(weights_global_comparison, by=c("country_code","horizon"))) %>% 
  map(~ .x %>% mutate(weighted_sd = value*weight)) %>% 
  map(~ .x %>% group_by(horizon)) %>% 
  map(~ .x %>% summarise(global_sd = sum(weighted_sd),
                         median_sd = median(value, na.rm = T))) %>% 
  map(~ .x %>% mutate(horizon = factor(horizon, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec")))) 


# Plot: global using weights


global_sd_weighted <- global_sd_df %>% 
  map(~ .x %>% ggplot(aes(horizon, global_sd)) +
  geom_point(size = 3, col = "#4472C4",alpha = 0.8) +
  xlab("") +
  ylab("Weighted Sd (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 21)))

# Export: 

global_sd_weighted %>% 
  iwalk(~ ggsave(paste0("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/consensus_sd_global_",.y,"_weighted.pdf"),
                 .x,
                 height = 5.7,
                 width = 11))


# Plot: global using median value

global_sd_median <- global_sd_df %>% 
  map(~ .x %>% ggplot(aes(horizon, median_sd)) +
        geom_point(size = 3, col = "#4472C4",alpha = 0.8) +
        xlab("") +
        ylab("Weighted Sd (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        theme(axis.text = element_text(size = 18),
              axis.title = element_text(size = 21)))

# Export: 

global_sd_median %>% 
  iwalk(~ ggsave(paste0("../Forecasts_Time_Covid_material/output/figures/digging_june_pessimism/consensus_sd_global_",.y,"_median.pdf"),
                 .x,
                 height = 5.7,
                 width = 11))



