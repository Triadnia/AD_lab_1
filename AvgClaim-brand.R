library(dplyr)
library(ggplot2)

# 1. Завантаження та підготовка даних
df_freq <- read.csv("freMTPL2freq.csv", sep = ";")
df_sev <- read.csv("freMTPL2sev (1).csv", sep = ",")

df_sev_grouped <- df_sev %>% group_by(IDpol) %>% summarise(TotalClaim = sum(ClaimAmount))
df_merged <- df_freq %>% inner_join(df_sev_grouped, by = "IDpol")

cap_value <- quantile(df_merged$TotalClaim, 0.95)

# 2. Обробка для марки авто
df_brand <- df_merged %>%
  mutate(CappedClaim = ifelse(TotalClaim > cap_value, cap_value, TotalClaim)) %>%
  group_by(VehBrand) %>%
  summarise(AvgClaim = mean(CappedClaim), Count = n()) %>%
  filter(Count >= 30)

# 3. Побудова графіка (Вертикальні стовпчики)
ggplot(df_brand, aes(x = reorder(VehBrand, AvgClaim), y = AvgClaim)) +
  geom_col(fill = "blue", width = 0.6) +
  # Значення тепер пишуться НАД стовпчиками (vjust = -0.5)
  geom_text(aes(label = round(AvgClaim, 0)), vjust = -0.5, size = 4, fontface = "bold") +
  
  labs(
    title = "Середній розмір збитку залежно від марки автомобіля",
    x = "Марка автомобіля",
    y = "Середній збиток (Євро)"
  ) +
  theme_minimal() +
  # Трохи підіймаємо "стелю" графіка, щоб цифри не зрізалися
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))