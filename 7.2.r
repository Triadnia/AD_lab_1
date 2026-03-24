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

age_intervals <- list(
  c(18, 29), c(30, 41), c(42, 53), c(54, 65), c(66, 77), c(78, 89), c(90, 101)
)

main_dir <- "7.2"
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
      
      if(sum(data_v$TotalClaim) > 0) {
        v_age_dir <- file.path(area_dir, paste0("VehAge_", v_age))
        dir.create(v_age_dir, showWarnings = FALSE)
        
        for(inter in age_intervals) {
          start_age <- inter[1]
          end_age <- inter[2]
          
          data_plot <- data_v |> filter(DrivAge >= start_age & DrivAge <= end_age & TotalClaim > 0)
          
          if(nrow(data_plot) > 0) {
            data_plot$DrivAge <- factor(data_plot$DrivAge, levels = start_age:end_age)
            
            p <- ggplot(data_plot, aes(x = VehPower, y = TotalClaim, fill = VehGas)) +
              stat_summary(fun = "mean", geom = "bar", position = "dodge") +
              facet_wrap(~DrivAge, drop = FALSE, ncol = 4) +
              scale_x_discrete(drop = FALSE) +
              theme_bw() +
              labs(title = paste("Марка:", b, "| Район:", a, "| Вік авто:", v_age),
                   subtitle = paste("СЕРЕДНЯ ВИПЛАТА (Євро) | Водії:", start_age, "-", end_age, "р."),
                   x = "Потужність (VehPower)", y = "Середня сума виплати (€)") +
              theme(strip.background = element_rect(fill="lightgreen"),
                    strip.text = element_text(face="bold"))
            
            fname <- paste0("AvgSeverity_", start_age, "_to_", end_age, ".png")
            ggsave(file.path(v_age_dir, fname), p, width = 14, height = 10)
          }
        }
      }
    }
  }
}
