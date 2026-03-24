library(dplyr)
library(ggplot2)

setwd("D:/R")
freq <- read.csv("freMTPL2freq.csv")
sev <- read.csv("freMTPL2sev.csv")

sev_sum <- sev |> group_by(IDpol) |> summarise(TotalClaim = sum(ClaimAmount), .groups = "drop")

all_powers <- as.character(4:15)
df_all <- freq |> 
  left_join(sev_sum, by = "IDpol") |> 
  mutate(TotalClaim = ifelse(is.na(TotalClaim), 0, TotalClaim)) |> 
  filter(VehAge <= 22) |> 
  mutate(VehPower = factor(as.character(VehPower), levels=all_powers))

out_dir <- "7.5"
dir.create(out_dir, showWarnings = FALSE)

brands <- sort(unique(df_all$VehBrand))

for(b in brands) {
  
  data_brand <- df_all |> filter(VehBrand == b & TotalClaim > 0)
  
  if(nrow(data_brand) > 0) {
    p <- ggplot(data_brand, aes(x = VehPower, y = TotalClaim, fill = TotalClaim)) +
      stat_summary(fun = "mean", geom = "bar") +
      scale_fill_gradient(low = "yellow", high = "red") + 
      facet_grid(VehAge ~ Area, scales = "free_y") + 
      scale_x_discrete(drop = FALSE) +
      theme_minimal() +
      labs(title = paste("Карта фінансових аномалій (Середня виплата):", b),
           subtitle = "Колонки: Райони | Рядки: Вік авто (0-22) | Колір: Висота виплати",
           x = "Потужність (4-15)", y = "Середня виплата (€)",
           fill = "Сума (€)") +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 90, size = 6),
        panel.spacing = unit(0.1, "lines"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey80", fill = NA)
      )
    
    fname <- paste0("Severity_Map_", b, ".png")
    ggsave(file.path(out_dir, fname), p, width = 30, height = 45, limitsize = FALSE)
  }
}