library(dplyr)
library(ggplot2)

df_freq <- read.csv("freMTPL2freq.csv", sep = ";")
df_sev <- read.csv("freMTPL2sev (1).csv", sep = ",")

df_sev_grouped <- df_sev %>% group_by(IDpol) %>% summarise(TotalClaim = sum(ClaimAmount))
df_merged <- df_freq %>% inner_join(df_sev_grouped, by = "IDpol")

cap_value <- quantile(df_merged$TotalClaim, 0.95)

df_power <- df_merged %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_value, cap_value, TotalClaim)) %>%
  group_by(VehPower) %>%
  summarise(AvgClaim = mean(CappedClaim), Count = n()) %>%
  filter(Count >= 30)

ggplot(df_power, aes(x = as.factor(VehPower), y = AvgClaim)) +
  geom_col(fill = "blue", width = 0.7) +
  geom_text(aes(label = round(AvgClaim, 0)), vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title = "Вплив потужності двигуна на середній розмір збитку",
    x = "Категорія потужності автомобіля",
    y = "Середній збиток (Євро)"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))