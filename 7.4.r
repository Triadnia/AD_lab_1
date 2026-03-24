library(dplyr)
library(ggplot2)

setwd("D:/R")
freq <- read.csv("freMTPL2freq.csv")

all_powers <- as.character(4:15)
df_all <- freq |> 
  filter(VehAge <= 22) |> 
  mutate(VehPower = factor(as.character(VehPower), levels=all_powers))

out_dir <- "7.4"
dir.create(out_dir, showWarnings = FALSE)

brands <- sort(unique(df_all$VehBrand))

for(b in brands) {
  
  data_brand <- df_all |> filter(VehBrand == b & ClaimNb > 0)
  
  if(nrow(data_brand) > 0) {
    p <- ggplot(data_brand, aes(x = VehPower, y = ClaimNb, fill = VehGas)) +
      geom_bar(stat = "identity") +
      facet_grid(VehAge ~ Area, scales = "free_y") + 
      scale_x_discrete(drop = FALSE) +
      theme_minimal() +
      labs(title = paste("Карта аномалій для марки:", b),
           subtitle = "Колонки: Райони | Рядки: Вік автомобіля",
           x = "Потужність (4-15)", y = "К-ть ДТП") +
      theme(
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(angle = 90, size = 6),
        panel.spacing = unit(0.1, "lines"),
        panel.border = element_rect(color = "grey", fill = NA)
      )
    
    fname <- paste0("Anomaly_Map_", b, ".png")
    ggsave(file.path(out_dir, fname), p, width = 25, height = 40, limitsize = FALSE)
  }
}