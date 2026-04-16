library(tidyverse)

freq_data <- read_csv("D:/my_projects/AD/freMTPL2freq.csv", show_col_types = FALSE)
sev_data <- read_csv("D:/my_projects/AD/freMTPL2sev.csv", show_col_types = FALSE)

sev_data_clean <- sev_data %>% distinct()

sev_aggregated <- sev_data_clean %>%
  group_by(IDpol) %>%
  summarise(Total_Claim_Amount = sum(ClaimAmount), .groups = 'drop')

full_data <- freq_data %>%
  left_join(sev_aggregated, by = "IDpol") %>%
  mutate(
    Total_Claim_Amount = replace_na(Total_Claim_Amount, 0),
    Exposure = if_else(Exposure > 1, 1, Exposure),
    VehBrand = as.factor(VehBrand),
    VehGas = as.factor(VehGas),
    Region = as.factor(Region),
    Area = as.factor(Area)
  ) %>%
  filter(!(ClaimNb == 0 & Total_Claim_Amount > 0)) %>%
  filter(!(ClaimNb > 0 & Total_Claim_Amount == 0))

claims_only <- full_data %>% filter(Total_Claim_Amount > 0)

claims_classified <- claims_only %>%
  mutate(
    Claim_Type = if_else(round(Total_Claim_Amount) %in% c(1204, 1128, 1172), 
                         "Фіксовані суми (Аномалія)", 
                         "Індивідуальні аварії (Норма)")
  )

age_sev <- claims_only %>%
  group_by(DrivAge) %>%
  summarise(
    Avg_Claim = mean(Total_Claim_Amount),
    Count = n(),
    .groups = 'drop'
  ) %>%
  filter(Count > 5)

individual_claims <- claims_classified %>% 
  filter(Claim_Type == "Індивідуальні аварії (Норма)")

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

cat("Межа 99.5% процентилю для виплат становить:", round(cap_995, 2), "євро\n")

risk_analysis_capped <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount)
  ) %>%
  mutate(
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    PowerGroup = case_when(
      VehPower <= 5 ~ "Низька (<=5)",
      VehPower <= 8 ~ "Середня (6-8)",
      TRUE ~ "Висока (9+)"
    )
  ) %>%
  group_by(Area, AgeGroup, PowerGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount), 
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Risk_Premium_Capped))

head(risk_analysis_capped, 10)


library(dplyr)
library(ggplot2)

top_10_risk <- head(risk_analysis_capped, 10) %>%
  mutate(
    Group_Label = paste("Район:", Area, "| Потужність:", PowerGroup),
    Group_Label = reorder(Group_Label, Risk_Premium_Capped)
  )

ggplot(top_10_risk, aes(x = Risk_Premium_Capped, y = Group_Label)) +
  geom_col(aes(fill = Risk_Premium_Capped), color = "black") +
  scale_fill_gradient(low = "orange", high = "darkred") +
  
  geom_text(aes(label = paste0("€", round(Risk_Premium_Capped, 0))), 
            hjust = -0.2, color = "black", fontface = "bold", size = 5) +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "ТОП-10 ГРУП НАЙВИЩОГО ФІНАНСОВОГО РИЗИКУ",
    x = "Очікувані збитки на 1 поліс за рік (Risk Premium), євро",
    y = ""
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

cat("Межа 99.5% процентилю для виплат становить:", round(cap_995, 2), "євро\n")

risk_analysis_vehage <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount)
  ) %>%
  mutate(
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    VehAgeGroup = case_when(
      VehAge <= 3 ~ "Нові (0-3 р.)",
      VehAge <= 10 ~ "Б/В (4-10 р.)",
      TRUE ~ "Старі (11+ р.)"
    )
  ) %>%
  group_by(Area, AgeGroup, VehAgeGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount), 
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Risk_Premium_Capped))

head(risk_analysis_vehage, 10)

top_10_vehage <- head(risk_analysis_vehage, 10) %>%
  mutate(
    Group_Label = paste("Район:", Area, "| Вік авто:", VehAgeGroup),
    Group_Label = reorder(Group_Label, Risk_Premium_Capped)
  )

p_vehage <- ggplot(top_10_vehage, aes(x = Risk_Premium_Capped, y = Group_Label)) +
  geom_col(aes(fill = Risk_Premium_Capped), color = "black") +

  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  
  geom_text(aes(label = paste0("€", round(Risk_Premium_Capped, 0))), 
            hjust = -0.2, color = "black", fontface = "bold", size = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "ТОП-10 ГРУП РИЗИКУ (За Віком Авто)",
    x = "Очікувані збитки на 1 поліс за рік (Risk Premium), євро",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_vehage)

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

risk_analysis_combined <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount),
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    PowerGroup = case_when(
      VehPower <= 5 ~ "Низька (<=5)",
      VehPower <= 8 ~ "Середня (6-8)",
      TRUE ~ "Висока (9+)"
    ),
    VehAgeGroup = case_when(
      VehAge <= 3 ~ "Нові (0-3 р.)",
      VehAge <= 10 ~ "Б/В (4-10 р.)",
      TRUE ~ "Старі (11+ р.)"
    )
  ) %>%
  group_by(Area, AgeGroup, PowerGroup, VehAgeGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount),
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Risk_Premium_Capped))

head(risk_analysis_combined, 10)

top_10_combined <- head(risk_analysis_combined, 10) %>%
  mutate(
    Group_Label = paste("Район:", Area, "| Потужність:", PowerGroup, "\nВік авто:", VehAgeGroup),
    Group_Label = reorder(Group_Label, Risk_Premium_Capped)
  )

p_combined <- ggplot(top_10_combined, aes(x = Risk_Premium_Capped, y = Group_Label)) +
  geom_col(aes(fill = Risk_Premium_Capped), color = "black") +
  scale_fill_gradient(low = "#DDA0DD", high = "#4B0082") + 
  
  geom_text(aes(label = paste0("€", round(Risk_Premium_Capped, 0))), 
            hjust = -0.2, color = "black", fontface = "bold", size = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "ТОП-10 ГРУП АБСОЛЮТНОГО РИЗИКУ (Комбінований)",
    x = "Очікувані збитки на 1 поліс за рік (Risk Premium), євро",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold", lineheight = 0.8), 
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_combined)

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

frequency_analysis <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount)
  ) %>%
  mutate(
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    PowerGroup = case_when(
      VehPower <= 5 ~ "Низька (<=5)",
      VehPower <= 8 ~ "Середня (6-8)",
      TRUE ~ "Висока (9+)"
    )
  ) %>%
  group_by(Area, AgeGroup, PowerGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount), 
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Frequency))

head(frequency_analysis, 10)

df_freq_all <- head(frequency_analysis, 10) %>%
  mutate(
    Label = paste("Район:", Area, "|", AgeGroup, "\nПотужн.:", PowerGroup),
    Label = reorder(Label, Frequency)
  )

ggplot(df_freq_all, aes(x = Frequency, y = Label)) +
  geom_col(fill = "#2E8B57", color = "black") + 
  geom_text(aes(label = round(Frequency, 3)), hjust = -0.2, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "ТОП-10 ЗА ЧАСТОТОЮ ДТП (Всі вікові групи)",
       subtitle = "Ймовірність потрапити в аварію протягом 1 року",
       x = "Частота (Frequency)", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

risk_analysis_adults <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount),
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    PowerGroup = case_when(
      VehPower <= 5 ~ "Низька (<=5)",
      VehPower <= 8 ~ "Середня (6-8)",
      TRUE ~ "Висока (9+)"
    ),
    VehAgeGroup = case_when(
      VehAge <= 3 ~ "Нові (0-3 р.)",
      VehAge <= 10 ~ "Б/В (4-10 р.)",
      TRUE ~ "Старі (11+ р.)"
    )
  ) %>%
  filter(AgeGroup != "18-25 (Молоді)") %>%
  
  group_by(Area, AgeGroup, PowerGroup, VehAgeGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount),
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Risk_Premium_Capped))

head(risk_analysis_adults, 10)

df_risk_adults <- head(risk_analysis_adults, 10) %>%
  mutate(
    Label = paste("Район:", Area, "|", AgeGroup, "\nПотужн.:", PowerGroup, "| Авто:", VehAgeGroup),
    Label = reorder(Label, Risk_Premium_Capped)
  )

ggplot(df_risk_adults, aes(x = Risk_Premium_Capped, y = Label)) +
  geom_col(fill = "#4682B4", color = "black") + 
  geom_text(aes(label = paste0("€", round(Risk_Premium_Capped, 0))), hjust = -0.2, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "ТОП-10 НАЙЗБИТКОВІШИХ (Без Молоді)",
       subtitle = "Ризик-премія: фінансовий ризик дорослих клієнтів",
       x = "Ризик-премія (євро)", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

severity_analysis <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount)
  ) %>%
  mutate(
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    PowerGroup = case_when(
      VehPower <= 5 ~ "Низька (<=5)",
      VehPower <= 8 ~ "Середня (6-8)",
      TRUE ~ "Висока (9+)"
    )
  ) %>%
  group_by(Area, AgeGroup, PowerGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount), 
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Severity_Capped))

head(severity_analysis, 10)

df_sev_all <- head(severity_analysis, 10) %>%
  mutate(
    Label = paste("Район:", Area, "|", AgeGroup, "\nПотужн.:", PowerGroup),
    Label = reorder(Label, Severity_Capped)
  )

ggplot(df_sev_all, aes(x = Severity_Capped, y = Label)) +
  geom_col(fill = "#B22222", color = "black") + 
  geom_text(aes(label = paste0("€", round(Severity_Capped, 0))), hjust = -0.2, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "ТОП-10 ЗА ВАРТІСТЮ ОДНІЄЇ ДТП (Severity)",
       subtitle = "Середній чек виплати у разі аварії",
       x = "Тяжкість ДТП (євро)", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

cap_995 <- quantile(full_data$Total_Claim_Amount[full_data$Total_Claim_Amount > 0], probs = 0.995)

frequency_adults <- full_data %>%
  mutate(
    Capped_Claim_Amount = if_else(Total_Claim_Amount > cap_995, cap_995, Total_Claim_Amount),
    AgeGroup = case_when(
      DrivAge <= 25 ~ "18-25 (Молоді)",
      DrivAge <= 60 ~ "26-60 (Дорослі)",
      TRUE ~ "60+ (Пенсіонери)"
    ),
    PowerGroup = case_when(
      VehPower <= 5 ~ "Низька (<=5)",
      VehPower <= 8 ~ "Середня (6-8)",
      TRUE ~ "Висока (9+)"
    ),
    VehAgeGroup = case_when(
      VehAge <= 3 ~ "Нові (0-3 р.)",
      VehAge <= 10 ~ "Б/В (4-10 р.)",
      TRUE ~ "Старі (11+ р.)"
    )
  ) %>%
  filter(AgeGroup != "18-25 (Молоді)") %>%
  
  group_by(Area, AgeGroup, PowerGroup, VehAgeGroup) %>%
  summarise(
    Total_Clients = n(),
    Total_Exposure = sum(Exposure),
    Total_Claims = sum(ClaimNb),
    Total_Loss_Capped = sum(Capped_Claim_Amount),
    .groups = "drop"
  ) %>%
  filter(Total_Exposure >= 100) %>%
  mutate(
    Frequency = Total_Claims / Total_Exposure,
    Severity_Capped = if_else(Total_Claims > 0, Total_Loss_Capped / Total_Claims, 0),
    Risk_Premium_Capped = Total_Loss_Capped / Total_Exposure
  ) %>%
  arrange(desc(Frequency))
head(frequency_adults, 10)

df_freq_adults <- head(frequency_adults, 10) %>%
  mutate(
    Label = paste("Район:", Area, "|", AgeGroup, "\nПотужн.:", PowerGroup, "| Авто:", VehAgeGroup),
    Label = reorder(Label, Frequency)
  )

ggplot(df_freq_adults, aes(x = Frequency, y = Label)) +
  geom_col(fill = "#20B2AA", color = "black") + 
  geom_text(aes(label = round(Frequency, 3)), hjust = -0.2, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "ТОП-10 ЗА ЧАСТОТОЮ (Без Молоді)",
       subtitle = "Групи дорослих, які найчастіше потрапляють у ДТП",
       x = "Частота (Frequency)", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 14))
