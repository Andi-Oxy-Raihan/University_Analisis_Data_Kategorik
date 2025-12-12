#Import Data
library(readxl)
data_reg <- read_excel("Data.xlsx",sheet = "13_OutlierNormalitas")

#Membuat summary data
#install.packages("Hmisc")
library(Hmisc)
describe(data_reg)
summary(data_reg)
table(data_reg$Y3)

#Membuat model regresi logistik
model_log = glm(Y3~X1+X2+X3,family=binomial,data=data_reg)
model_log
summary(model_log)
fitted(model_log)

#1. Uji Kelayakan model
# Instal dan muat paket ResourceSelection
#install.packages("ResourceSelection")
library(ResourceSelection)

# Lakukan Uji Hosmer-Lemeshow
hoslem.test(data_reg$Y3, fitted(model_log)) 
#jika p-value > 0.05 maka model dianggap layak

#2. Uji Simultan model
#install.packages("pscl")
library(pscl)
pR2(model_log)
#df = 3, jumlah variabel bebas
qchisq(0.95,3)
#jika G2 > Chi Squared tabel maka terdapat pengaruh serentak yang signifikan

#3. Uji Parsial
summary(model_log)

#4. Koefisien R Squared
#install.packages("rcompanion")
library(rcompanion)
nagelkerke(model_log)
#0.369 = 36.9%

#5. Mengecek nilai odds ratio
beta = coef(model_log)
OR_beta = exp(beta)

cbind(beta, OR_beta)

#syntax regresi multinomial
#https://rpubs.com/reniamelia/responsi7adk1