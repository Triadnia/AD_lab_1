

library(data.table)
library(ggplot2)

freq <- fread("B:/AD/freMTPL2freq.csv")
sev  <- fread("B:/AD/freMTPL2sev.csv")

sev_agg <- aggregate(ClaimAmount ~ IDpol, data = sev, sum)
colnames(sev_agg)[2] <- "total_claim"


insurance_data <- merge(freq, sev_agg, by = "IDpol", all.x = TRUE)
insurance_data$total_claim[is.na(insurance_data$total_claim)] <- 0

head(insurance_data)

threshold <- quantile(insurance_data$total_claim[insurance_data$total_claim > 0], 0.95)
insurance_data$large_claim <- ifelse(insurance_data$total_claim > threshold, 1, 0)

insurance_data$BM_group <- cut(insurance_data$BonusMalus, breaks = c(0, 80, 100, 120, 200, Inf), labels = c("low", "good", "medium", "bad", "very_bad"))

bm_analysis <- aggregate(cbind(large_claim, total_claim) ~ BM_group, data = insurance_data, FUN = mean)

print("Bonus-malusS")
print(bm_analysis)

region_stats <- aggregate(cbind(Exposure, ClaimNb, total_claim) ~ Region, data = insurance_data, sum)
region_stats$freq <- region_stats$ClaimNb / region_stats$Exposure
region_stats$avg_claim <- ifelse(region_stats$ClaimNb > 0, region_stats$total_claim / region_stats$ClaimNb, 0)
region_stats$loss_ratio <- region_stats$total_claim / region_stats$Exposure

head(region_stats)
features <- scale(region_stats[, c("freq", "avg_claim", "loss_ratio")])
set.seed(5)

km <- kmeans(features, centers = 4)
region_stats$cluster <- as.factor(km$cluster)

print(region_stats)

ggplot(region_stats, aes(x = freq, y = avg_claim, color = cluster)) + geom_point(size = 4) + geom_text(aes(label = Region), vjust = -1) + theme_minimal() + labs(title = "Кластеризація регіонів Франції",
       x = "Частота збитків", y = "Середній збиток")

ggplot(insurance_data, aes(x = BM_group, y = total_claim)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Розподіл виплат по Bonus-Malus",
       x = "Bonus-Malus група",
       y = "Виплати")

top_regions <- region_stats[order(-region_stats$avg_claim), ]
head(top_regions, 5)
