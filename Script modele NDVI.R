##### PhD Track NDVI model #####


# Packages ----------------------------------------------------------------
library(tidyverse)


# Data --------------------------------------------------------------------
ndvi <- read.table("NDVI_15j_LEA_nouveau_sept.txt",header = TRUE,stringsAsFactors = TRUE, na.strings = "NA",dec = ",")
humidite <- read.csv("humidite.csv")
precip <- read.csv("precip.csv")
deb_couesnon <- read.csv("deb_couesnon.csv")


# Formatage ---------------------------------------------------------------
# Format Dates R
humidite$Date <- as.Date(humidite$Date,"%Y-%m-%d")
precip$Date <- as.Date(precip$Date,"%Y-%m-%d")
deb_couesnon$Date <- as.Date(deb_couesnon$Date,"%Y-%m-%d")
ndvi$Debut <- as.Date(ndvi$Debut,"%Y-%m-%d")
ndvi$Fin <- as.Date(ndvi$Fin,"%Y-%m-%d")
# La colonne 'debut' et 'fin' correspondent au premier et dernier jours comptes dans la moyenne des 15 jours pour le NDVI
ndvi$Date <- ndvi$Fin
colnames(precip) <- c("Date","precip")

# Association NDVI et variables sur des echelles communes 
L <- list()
for (i in seq(15,60,5)) {
  temp <- deb_couesnon
  for (j in i:length(temp$Debits)) {
    temp$Debits[j]<- mean(temp$Debits[(j-i):j])
  }
  L[[as.character(i)]] <- left_join(x = ndvi,y = temp, by = "Date")
  temp <- humidite
  for (k in i:length(temp$RH2M)) {
    temp$RH2M[k] <- mean(temp$RH2M[(k-i):k])
    temp$T2M[k]<- mean(temp$T2M[(k-i):k])
    temp$TS[k]<- mean(temp$TS[(k-i):k])
    temp$T2M_MAX[k]<- mean(temp$T2M_MAX[(k-i):k])
    temp$T2M_MIN[k]<- mean(temp$T2M_MIN[(k-i):k])
  }
  L[[as.character(i)]] <- left_join(x = L[[as.character(i)]],y = temp, by = "Date")
  temp <- precip
  for (m in i:length(temp$precip)) {
    temp$precip[m]<- mean(temp$precip[(m-i):m])
  }
  L[[as.character(i)]] <- left_join(x = L[[as.character(i)]],y = temp, by = "Date")
}

# Extraction d'un dataframe pour les moyennes sur 15 jours 
df <- data.frame(L[[1]])

# Choix fait d'enlever les NAs
df <- na.omit(df)

# Exploration de Y - NDVI Tourbiere Landemarais -------------------------------------------------
par(mfrow=c(2,2))
# Boxplot
boxplot(df$NDVI_moyen_Tourbiere_Landemarais,col='green',ylab='NDVI moyen Tourbiere Landemarais')
# Cleveland plot
dotchart(df$NDVI_moyen_Tourbiere_Landemarais,pch=16,col='green',xlab='NDVI moyen Tourbiere Landemarais')
# Histogram
hist(df$NDVI_moyen_Tourbiere_Landemarais,col='green',xlab="NDVI moyen Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df$NDVI_moyen_Tourbiere_Landemarais,pch=16,col='green',xlab='')
qqline(df$NDVI_moyen_Tourbiere_Landemarais,col='red')

# Extraction des outliers
quartiles <- quantile(df$NDVI_moyen_Tourbiere_Landemarais, probs = c(0.25, 0.75))
IQR <- quartiles[2] - quartiles[1]
seuil_inf <- quartiles[1] - 1.5 * IQR
seuil_sup <- quartiles[2] + 1.5 * IQR
outliers <- df$NDVI_moyen_Tourbiere_Landemarais < seuil_inf | df$NDVI_moyen_Tourbiere_Landemarais > seuil_sup
df.tourbiere <- df[!outliers, ]

# Plot sans les outliers
# Boxplot
boxplot(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais,col='green',ylab='NDVI moyen Tourbiere Landemarais')
# Cleveland plot
dotchart(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais,pch=16,col='green',xlab='NDVI moyen Tourbiere Landemarais')
# Histogram
hist(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais,col='green',xlab="NDVI moyen Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais,pch=16,col='green',xlab='')
qqline(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais,col='red')


# Exploration de Y - NDVI Marais Sougeal -----------------------------------------
# Boxplot
boxplot(df$NDVI_moyen_Marais_Sougeal,col='green',ylab='NDVI moyen Tourbiere Landemarais')
# Cleveland plot
dotchart(df$NDVI_moyen_Marais_Sougeal,pch=16,col='green',xlab='NDVI moyen Tourbiere Landemarais')
# Histogram
hist(df$NDVI_moyen_Marais_Sougeal,col='green',xlab="NDVI moyen Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df$NDVI_moyen_Marais_Sougeal,pch=16,col='green',xlab='')
qqline(df$NDVI_moyen_Marais_Sougeal,col='red')

# Extraction des outliers 
quartiles <- quantile(df$NDVI_moyen_Marais_Sougeal, probs = c(0.25, 0.75))
IQR <- quartiles[2] - quartiles[1]
seuil_inf <- quartiles[1] - 1.5 * IQR
seuil_sup <- quartiles[2] + 1.5 * IQR
outliers <- df$NDVI_moyen_Marais_Sougeal < seuil_inf | df$NDVI_moyen_Marais_Sougeal > seuil_sup
df.marais <- df[!outliers, ]

# Plot sans les outliers
# Boxplot
boxplot(df.marais$NDVI_moyen_Marais_Sougeal,col='green',ylab='NDVI moyen Marais Sougeal')
# Cleveland plot
dotchart(df.marais$NDVI_moyen_Marais_Sougeal,pch=16,col='green',xlab='NDVI moyen Marais Sougeal')
# Histogram
hist(df.marais$NDVI_moyen_Marais_Sougeal,col='green',xlab="NDVI moyen Marais Sougeal",main="")
# Quantile-Quantile plot
qqnorm(df.marais$NDVI_moyen_Marais_Sougeal,pch=16,col='green',xlab='')
qqline(df.marais$NDVI_moyen_Marais_Sougeal,col='red')


# Exploration de X - Temperature Tourbiere Landemarais ------------------------------------------
# Boxplot
boxplot(df.tourbiere$T2M,col='orange',ylab='Temperature moyenne Tourbiere Landemarais')
# Cleveland plot
dotchart(df.tourbiere$T2M,pch=16,col='orange',xlab='Temperature moyenne Tourbiere Landemarais')
# Histogram
hist(df.tourbiere$T2M,col='orange',xlab="Temperature moyenne Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df.tourbiere$T2M,pch=16,col='orange',xlab='')
qqline(df.tourbiere$T2M,col='red')

# # Extraction des outliers
# quartiles <- quantile(df.tourbiere$T2M, probs = c(0.25, 0.75))
# IQR <- quartiles[2] - quartiles[1]
# seuil_inf <- quartiles[1] - 1.5 * IQR
# seuil_sup <- quartiles[2] + 1.5 * IQR
# outliers <- df.tourbiere$T2M < seuil_inf | df.tourbiere$T2M > seuil_sup
# df.tourbiere <- df.tourbiere[!outliers, ]

# # Plot sans les outliers
# # Boxplot
# boxplot(df.tourbiere$T2M,col='orange',ylab='Temperature moyenne Tourbiere Landemarais')
# # Cleveland plot
# dotchart(df.tourbiere$T2M,pch=16,col='orange',xlab='Temperature moyenne Tourbiere Landemarais')
# # Histogram
# hist(df.tourbiere$T2M,col='orange',xlab="Temperature moyenne Tourbiere Landemarais",main="")
# # Quantile-Quantile plot
# qqnorm(df.tourbiere$T2M,pch=16,col='orange',xlab='')
# qqline(df.tourbiere$T2M,col='red')


# Exploration de X - Temperature Sougeal ----------------------------------
# Boxplot
boxplot(df.marais$T2M,col='orange',ylab='Temperature moyenne Marais Sougeal')
# Cleveland plot
dotchart(df.marais$T2M,pch=16,col='orange',xlab='Temperature moyenne Marais Sougeal')
# Histogram
hist(df.marais$T2M,col='orange',xlab="Temperature moyenne Marais Sougeal",main="")
# Quantile-Quantile plot
qqnorm(df.marais$T2M,pch=16,col='orange',xlab='')
qqline(df.marais$T2M,col='red')

# # Extraction des outliers
# quartiles <- quantile(df.marais$T2M, probs = c(0.25, 0.75))
# IQR <- quartiles[2] - quartiles[1]
# seuil_inf <- quartiles[1] - 1.5 * IQR
# seuil_sup <- quartiles[2] + 1.5 * IQR
# outliers <- df.marais$T2M < seuil_inf | df.marais$T2M > seuil_sup
# df.marais <- df.marais[!outliers, ]

# # Plot sans les outliers
# # Boxplot
# boxplot(df.marais$T2M,col='orange',ylab='Temperature moyenne Marais Sougeal')
# # Cleveland plot
# dotchart(df.marais$T2M,pch=16,col='orange',xlab='Temperature moyenne Marais Sougeal')
# # Histogram
# hist(df.marais$T2M,col='orange',xlab="Temperature moyenne Marais Sougeal",main="")
# # Quantile-Quantile plot
# qqnorm(df.marais$T2M,pch=16,col='orange',xlab='')
# qqline(df.marais$T2M,col='red')


# Exploration de X - Debit Tourbiere Landemarais --------------------------
# Boxplot
boxplot(df.tourbiere$Debits,col='blue',ylab='Debit moyen Tourbiere Landemarais')
# Cleveland plot
dotchart(df.tourbiere$Debits,pch=16,col='blue',xlab='Debit moyen Tourbiere Landemarais')
# Histogram
hist(df.tourbiere$Debits,col='blue',xlab="Debit moyen Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df.tourbiere$Debits,pch=16,col='blue',xlab='')
qqline(df.tourbiere$Debits,col='red')


# Exploration de X - Debit Marais Sougeal ---------------------------------
# Boxplot
boxplot(df.marais$Debits,col='blue',ylab='Debit moyen Marais Sougeal')
# Cleveland plot
dotchart(df.marais$Debits,pch=16,col='blue',xlab='Debit moyen Marais Sougeal')
# Histogram
hist(df.marais$Debits,col='blue',xlab="Debit moyen Marais Sougeal",main="")
# Quantile-Quantile plot
qqnorm(df.marais$Debits,pch=16,col='blue',xlab='')
qqline(df.marais$Debits,col='red')


# Exploration de X - Humidite Tourbiere Landemarais -----------------------
# Boxplot
boxplot(df.tourbiere$RH2M,col='cyan',ylab='Humidite moyenne Tourbiere Landemarais')
# Cleveland plot
dotchart(df.tourbiere$RH2M,pch=16,col='cyan',xlab='Humidite moyenne Tourbiere Landemarais')
# Histogram
hist(df.tourbiere$RH2M,col='cyan',xlab="Humidite moyenne Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df.tourbiere$RH2M,pch=16,col='cyan',xlab='')
qqline(df.tourbiere$RH2M,col='red')

# # Extraction des outliers
# quartiles <- quantile(df.tourbiere$RH2M, probs = c(0.25, 0.75))
# IQR <- quartiles[2] - quartiles[1]
# seuil_inf <- quartiles[1] - 1.5 * IQR
# seuil_sup <- quartiles[2] + 1.5 * IQR
# outliers <- df.tourbiere$RH2M < seuil_inf | df.tourbiere$RH2M > seuil_sup
# df.tourbiere <- df.tourbiere[!outliers, ]

# # Plot sans les outliers 
# # Boxplot
# boxplot(df.tourbiere$RH2M,col='cyan',ylab='Humidite moyenne Tourbiere Landemarais')
# # Cleveland plot
# dotchart(df.tourbiere$RH2M,pch=16,col='cyan',xlab='Humidite moyenne Tourbiere Landemarais')
# # Histogram
# hist(df.tourbiere$RH2M,col='cyan',xlab="Humidite moyenne Tourbiere Landemarais",main="")
# # Quantile-Quantile plot
# qqnorm(df.tourbiere$RH2M,pch=16,col='cyan',xlab='')
# qqline(df.tourbiere$RH2M,col='red')


# Exploration de X - Humidite Marais Sougeal ------------------------------
# Boxplot
boxplot(df.marais$RH2M,col='cyan',ylab='Humidite moyenne Marais Sougeal')
# Cleveland plot
dotchart(df.marais$RH2M,pch=16,col='cyan',xlab='Humidite moyenne Marais Sougeal')
# Histogram
hist(df.marais$RH2M,col='cyan',xlab="Humidite moyenne Marais Sougeal",main="")
# Quantile-Quantile plot
qqnorm(df.marais$RH2M,pch=16,col='cyan',xlab='')
qqline(df.marais$RH2M,col='red')

# # Extraction des outliers
# quartiles <- quantile(df.marais$RH2M, probs = c(0.25, 0.75))
# IQR <- quartiles[2] - quartiles[1]
# seuil_inf <- quartiles[1] - 1.5 * IQR
# seuil_sup <- quartiles[2] + 1.5 * IQR
# outliers <- df.marais$RH2M < seuil_inf | df.marais$RH2M > seuil_sup
# df.marais <- df.marais[!outliers, ]

# # Plot sans les outliers 
# boxplot(df.marais$RH2M,col='cyan',ylab='Humidite moyenne Marais Sougeal')
# # Cleveland plot
# dotchart(df.marais$RH2M,pch=16,col='cyan',xlab='Humidite moyenne Marais Sougeal')
# # Histogram
# hist(df.marais$RH2M,col='cyan',xlab="Humidite moyenne Marais Sougeal",main="")
# # Quantile-Quantile plot
# qqnorm(df.marais$RH2M,pch=16,col='cyan',xlab='')
# qqline(df.marais$RH2M,col='red')


# Exploration de X - Precipitation Tourbiere Landemarais ------------------
# Boxplot
boxplot(df.tourbiere$precip,col='grey',ylab='Precipitation moyenne Tourbiere Landemarais')
# Cleveland plot
dotchart(df.tourbiere$precip,pch=16,col='grey',xlab='Precipitation moyenne Tourbiere Landemarais')
# Histogram
hist(df.tourbiere$precip,col='grey',xlab="Precipitation moyenne Tourbiere Landemarais",main="")
# Quantile-Quantile plot
qqnorm(df.tourbiere$precip,pch=16,col='grey',xlab='')
qqline(df.tourbiere$precip,col='red')

dev.off()
# Visualisation des outliers
plot(df.tourbiere$precip)

# # Extraction des outliers
# quartiles <- quantile(df.tourbiere$precip, probs = c(0.25, 0.75), na.rm = TRUE)
# IQR <- quartiles[2] - quartiles[1]
# seuil_inf <- quartiles[1] - 1.5 * IQR
# seuil_sup <- quartiles[2] + 1.5 * IQR
# outliers <- df.tourbiere$precip < seuil_inf | df.tourbiere$precip > seuil_sup
# df.tourbiere <- df.tourbiere[!outliers, ]

# # Plot sans les outliers 
# # Boxplot
# boxplot(df.tourbiere$precip,col='grey',ylab='Precipitation moyenne Tourbiere Landemarais')
# # Cleveland plot
# dotchart(df.tourbiere$precip,pch=16,col='grey',xlab='Precipitation moyenne Tourbiere Landemarais')
# # Histogram
# hist(df.tourbiere$precip,col='grey',xlab="Precipitation moyenne Tourbiere Landemarais",main="")
# # Quantile-Quantile plot
# qqnorm(df.tourbiere$precip,pch=16,col='grey',xlab='')
# qqline(df.tourbiere$precip,col='red')


# Exploration de X - Precipitation Marais Sougeal -------------------------
par(mfrow=c(2,2))
# Boxplot
boxplot(df.marais$precip,col='grey',ylab='Precipitation moyenne Marais Sougeal')
# Cleveland plot
dotchart(df.marais$precip,pch=16,col='grey',xlab='Precipitation moyenne Marais Sougeal')
# Histogram
hist(df.marais$precip,col='grey',xlab="Precipitation moyenne Marais Sougeal",main="")
# Quantile-Quantile plot
qqnorm(df.marais$precip,pch=16,col='grey',xlab='')
qqline(df.marais$precip,col='red')

dev.off()
# Visualisation des outliers
plot(df.marais$precip)

# # Extraction des outliers
# quartiles <- quantile(df.marais$precip, probs = c(0.25, 0.75), na.rm = TRUE)
# IQR <- quartiles[2] - quartiles[1]
# seuil_inf <- quartiles[1] - 1.5 * IQR
# seuil_sup <- quartiles[2] + 1.5 * IQR
# outliers <- df.marais$precip < seuil_inf | df.marais$precip > seuil_sup
# df.marais <- df.marais[!outliers, ]

# # Plot sans les outliers 
# # Boxplot
# boxplot(df.marais$precip,col='grey',ylab='Precipitation moyenne Marais Sougeal')
# # Cleveland plot
# dotchart(df.marais$precip,pch=16,col='grey',xlab='Precipitation moyenne Marais Sougeal')
# # Histogram
# hist(df.marais$precip,col='grey',xlab="Precipitation moyenne Marais Sougeal",main="")
# # Quantile-Quantile plot
# qqnorm(df.marais$precip,pch=16,col='grey',xlab='')
# qqline(df.marais$precip,col='red')


# Relation entre Y et Xs - Tourbiere Landemarais ---------------------------------
# Temperature
plot(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais~df.tourbiere$T2M,pch=16,col='pink',xlab='Temperature moyenne',ylab='NDVI Tourbiere Landemarais')
# Debit
plot(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais~df.tourbiere$Debits,pch=16,col='pink',xlab='Debit moyen',ylab='NDVI Tourbiere Landemarais')
# Humidite
plot(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais~df.tourbiere$RH2M,pch=16,col='pink',xlab='Humidite moyenne',ylab='NDVI Tourbiere Landemarais')
# Precipitation
plot(df.tourbiere$NDVI_moyen_Tourbiere_Landemarais~df.tourbiere$precip,pch=16,col='pink',xlab='Precipitation moyenne',ylab='NDVI Tourbiere Landemarais')


# Relation entre Y et Xs - Marais Sougeal ---------------------------------
# Temperature
plot(df.marais$NDVI_moyen_Tourbiere_Landemarais~df.marais$T2M,pch=16,col='purple',xlab='Temperature moyenne',ylab='NDVI Tourbiere Landemarais')
# Debit
plot(df.marais$NDVI_moyen_Tourbiere_Landemarais~df.marais$Debits,pch=16,col='purple',xlab='Debit moyen',ylab='NDVI Tourbiere Landemarais')
# Humidite
plot(df.marais$NDVI_moyen_Tourbiere_Landemarais~df.marais$RH2M,pch=16,col='purple',xlab='Humidite moyenne',ylab='NDVI Tourbiere Landemarais')
# Precipitation
plot(df.marais$NDVI_moyen_Tourbiere_Landemarais~df.marais$precip,pch=16,col='purple',xlab='Precipitation moyenne',ylab='NDVI Tourbiere Landemarais')


# Correlations entre les Xs - Tourbiere Landemarais -----------------------
# Plots
plot(df.tourbiere[,c("T2M","Debits","RH2M","precip")],pch=16,col="#CC0066")
# Calcul des correlations
library(corrplot)
dev.off()
M <- cor(df.tourbiere[,c("T2M","Debits","RH2M","precip")])
corrplot.mixed(M,upper="square",lower.col="black", tl.col="black",cl.cex = 0.7,tl.cex = 0.6,number.cex =0.7)
# On choisi de retirer la température


# Modele Analysis ------------------------------------------------------------------
# Full model Tourbiere Landemerais
mod.tourbiere1<-lm(NDVI_moyen_Tourbiere_Landemarais ~ Debits * RH2M * precip, data = df.tourbiere)
summary(mod.tourbiere1)
# Full model Marais Sougeal
mod.marais1<-lm(NDVI_moyen_Tourbiere_Landemarais ~ Debits * RH2M * precip, data = df.marais)
summary(mod.marais1)


# Check for colinearity ---------------------------------------------------


