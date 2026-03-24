library(dplyr)
library(ggplot2)

setwd("D:/R")
freq <- read.csv("freMTPL2freq.csv")

all_powers <- as.character(4:15)
df_all <- freq |> 
  filter(VehAge <= 22) |> 
  mutate(VehPower = factor(as.character(VehPower), levels=all_powers))

age_intervals <- list(
  c(18, 29), c(30, 41), c(42, 53), c(54, 65), c(66, 77), c(78, 89), c(90, 101)
)

main_dir <- "7.1"
dir.create(main_dir, showWarnings = FALSE)

brands <- sort(unique(df_all$VehBrand))
areas <- sort(unique(df_all$Area))

for(b in brands) {
  brand_dir <- file.path(main_dir, b)
  dir.create(brand_dir, showWarnings = FALSE)
  
  for(a in areas) {
    area_dir <- file.path(brand_dir, paste0("Area_", a))
    dir.create(area_dir, showWarnings = FALSE)
    
    data_ba <- df_all |> filter(VehBrand == b & Area == a)
    veh_ages <- sort(unique(data_ba$VehAge))
    
    for(v_age in veh_ages) {
      data_v <- data_ba |> filter(VehAge == v_age)
      
      if(sum(data_v$ClaimNb) > 0) {
        v_age_dir <- file.path(area_dir, paste0("VehAge_", v_age))
        dir.create(v_age_dir, showWarnings = FALSE)
        
        for(inter in age_intervals) {
          start_age <- inter[1]
          end_age <- inter[2]
          
          data_plot_raw <- data_v |> filter(DrivAge >= start_age & DrivAge <= end_age & ClaimNb > 0)
          
          if(nrow(data_plot_raw) > 0) {
            
            data_plot <- data_plot_raw |>
              group_by(DrivAge, VehPower, VehGas) |>
              summarise(TotalClaims = sum(ClaimNb), .groups = "drop")
            
            data_plot$DrivAge <- factor(data_plot$DrivAge, levels = start_age:end_age)
            
            p <- ggplot(data_plot, aes(x = VehPower, y = TotalClaims, fill = VehGas)) +
              geom_bar(stat = "identity", position = "dodge") +
              facet_wrap(~DrivAge, drop = FALSE, ncol = 4) +
              scale_x_discrete(drop = FALSE) +
              theme_bw() +
              labs(title = paste("Марка:", b, "| Район:", a, "| Вік авто:", v_age),
                   subtitle = paste("СУМАРНА кількість аварій | Інтервал:", start_age, "-", end_age),
                   x = "Потужність (VehPower)", y = "Кількість аварій (Сума)") +
              theme(strip.background = element_rect(fill="lightblue"),
                    strip.text = element_text(face="bold"))
            
            fname <- paste0("DriverAge_", start_age, "_to_", end_age, ".png")
            ggsave(file.path(v_age_dir, fname), p, width = 14, height = 10)
          }
        }
      }
    }
  }
}
