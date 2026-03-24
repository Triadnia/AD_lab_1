library(dplyr)
library(ggplot2)

df_freq <- read.csv("freMTPL2freq.csv", sep = ";")
df_sev <- read.csv("freMTPL2sev (1).csv", sep = ",")

df_sev_grouped <- df_sev %>% group_by(IDpol) %>% summarise(TotalClaim = sum(ClaimAmount))
df_merged <- df_freq %>% inner_join(df_sev_grouped, by = "IDpol")

cap_value <- quantile(df_merged$TotalClaim, 0.95)

df_gas <- df_merged %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_value, cap_value, TotalClaim)) %>%
  group_by(VehGas) %>%
  summarise(
    AvgClaim = mean(CappedClaim),
    Count = n() 
  ) %>%
  mutate(GasLabel = paste0(VehGas, "\n(Всього авто: ", Count, ")"))

ggplot(df_gas, aes(x = GasLabel, y = AvgClaim, fill = VehGas)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = round(AvgClaim, 0)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Diesel" = "blue", "Regular" = "red")) +
  labs(
    title = "Вплив типу палива на середній розмір збитку",
    x = "Тип палива та кількість автомобілів",
    y = "Середній збиток (Євро)"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(color = c("black", "black"), face = "bold", size = 12)
  )