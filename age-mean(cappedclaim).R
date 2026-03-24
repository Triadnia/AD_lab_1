library(dplyr)
library(ggplot2)

df_freq <- read.csv("freMTPL2freq.csv", sep = ";")
df_sev <- read.csv("freMTPL2sev (1).csv", sep = ",")

df_sev_grouped <- df_sev %>% group_by(IDpol) %>% summarise(TotalClaim = sum(ClaimAmount))
df_merged <- df_freq %>% inner_join(df_sev_grouped, by = "IDpol")

cap_value <- quantile(df_merged$TotalClaim, 0.95)

df_age <- df_merged %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_value, cap_value, TotalClaim)) %>%
  filter(VehAge <= 30) %>%
  group_by(VehAge) %>%
  summarise(AvgClaim = mean(CappedClaim), Count = n()) %>%
  filter(Count >= 30)

ggplot(df_age, aes(x = VehAge, y = AvgClaim)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Вплив віку автомобіля на середній розмір збитку",
    x = "Вік автомобіля (роки)",
    y = "Середній збиток (Євро)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 26, by = 2)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))