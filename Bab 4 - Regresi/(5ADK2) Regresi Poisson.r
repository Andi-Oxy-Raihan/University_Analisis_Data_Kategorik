# Parameter lambda (rata-rata kejadian)
lambda <- 4  

# Rentang nilai x (jumlah kejadian)
x <- 0:10  

# Hitung probabilitas untuk setiap x
prob <- dpois(x, lambda)

# Plot kurva Poisson
plot(x, prob, type = "h", lwd = 3, col = "steelblue",
     main = paste("Distribusi Poisson (λ =", lambda, ")"),
     xlab = "Jumlah Kejadian (x)",
     ylab = "Probabilitas P(X = x)")
points(x, prob, pch = 16, col = "darkblue")

#Studi Kasus Pengaruh Penggunaan alat kontrasepsi terhadap penyakit HIV AIDS
# Set seed untuk replikasi hasil
set.seed(123)

# Jumlah data
n <- 200

# Variabel prediktor
X1 <- runif(n, 0, 10)       # nilai acak uniform antara 0-10
X2 <- rnorm(n, mean=5, sd=2) # nilai acak normal

# Parameter sebenarnya (true beta)
beta0 <- 0.5
beta1 <- 0.3
beta2 <- -0.2

# Hitung mean (lambda) sesuai model Poisson log-link
lambda <- exp(beta0 + beta1*X1 + beta2*X2)

# Bangkitkan Y ~ Poisson(lambda)
Y <- rpois(n, lambda)

# Buat data frame
data_pois <- data.frame(Y, X1, X2)

head(data_pois)


# Estimasi model regresi Poisson
model_pois <- glm(Y ~ X1 + X2, family = poisson(link = "log"), data = data_pois)

# Ringkasan hasil
summary(model_pois)