library(dplyr)

setwd("D:/R")
freq <- read.csv("freMTPL2freq.csv")
sev <- read.csv("freMTPL2sev.csv")

sev_sum <- sev |> group_by(IDpol) |> summarise(TotalClaim = sum(ClaimAmount), .groups = "drop")

df_all <- freq |> 
  left_join(sev_sum, by = "IDpol") |> 
  mutate(TotalClaim = ifelse(is.na(TotalClaim), 0, TotalClaim)) |> 
  filter(VehAge <= 22)

worst_freq <- df_all |>
  group_by(VehBrand, VehPower, DrivAge, VehAge, Area) |>
  summarise(Total_Claims = sum(ClaimNb), .groups = "drop") |>
  arrange(desc(Total_Claims)) |>
  head(1)

worst_sev <- df_all |>
  filter(TotalClaim > 0) |>
  group_by(VehBrand, VehPower, DrivAge, VehAge, Area) |>
  summarise(Avg_Payout = mean(TotalClaim), Count = n(), .groups = "drop") |>
  filter(Count >= 3) |> 
  arrange(desc(Avg_Payout)) |>
  head(1)

cat("\n--- ФІНАЛЬНИЙ ЗВІТ ПО РИЗИКАХ ---\n\n")

cat(paste0(
  "1. Найризикованіша комбінація ЗА КІЛЬКІСТЮ: ", 
  worst_freq$VehBrand, ", потужність ", worst_freq$VehPower, 
  ", водій ", worst_freq$DrivAge, " років, авто ", worst_freq$VehAge, 
  " р., місцевість ", worst_freq$Area, 
  " (Всього: ", worst_freq$Total_Claims, " випадків).\n\n"
))

cat(paste0(
  "2. Найризикованіша комбінація ЗА СЕРЕДНЬОЮ СУМОЮ: ", 
  worst_sev$VehBrand, ", потужність ", worst_sev$VehPower, 
  ", водій ", worst_sev$DrivAge, " років, авто ", worst_sev$VehAge, 
  " р., місцевість ", worst_sev$Area, 
  " (Середня виплата: ", round(worst_sev$Avg_Payout, 2), " EUR).\n"
))