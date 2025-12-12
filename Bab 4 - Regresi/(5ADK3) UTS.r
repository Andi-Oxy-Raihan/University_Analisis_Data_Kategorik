library(readr)
library(dplyr)

# Import data
data_birth <- birth_data

# Model Poisson
model_pois <- glm(Jumlah_anak_lahir ~ listrik + pendidikan_istri + agefbrth +
                    jumlah_anak_hidup + tahu_kontrasepsi + pakai_kontrasepsi +
                    jumlah_anak_ideal + pendidikan_suami + tidak_sekolah + status_menikah,
                  family = poisson(link = "log"),
                  data = data_birth)

summary(model_pois)
# Visualisasi residual
plot(model_pois$fitted.values, residuals(model_pois, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     main = "Plot Residual vs Fitted (Model Poisson)",
     col = "darkgreen", pch = 16)
abline(h = 0, lty = 2, col = "red")
