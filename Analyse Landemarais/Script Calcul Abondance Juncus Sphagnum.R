##### Ouverture des données
data<-read.table("Tableau_global.txt", header=T)

##### Tri de seulement les espèces qui nous intéressent :
colnames(data)

liste_sp <- c("Placette", "date_du_releve", "Juncus_bulbosus", "Juncus_effusus_conglomeratus", "Sphagnum_capillifolium", "Sphagnum_sp")

## Tri dans le tableau
data <- data[,which(colnames(data) %in% liste_sp)]

# Conversion des valeurs de B-B en % de recouvrement :

for (i in 1:nrow(data)) {
  for (j in 2:ncol(data)) {
    if (!(is.na(data[i,j]))) {
      if (data[i,j]=="i") {
        data[i,j]<-0
      }
      if (data[i,j]=="r") {
        data[i,j]<-0.01
      }
      if (data[i,j]=="+") {
        data[i,j]<-0.1
      }
      if (data[i,j]=="2") {
        data[i,j]<-17.5
      }
      if (data[i,j]=="3") {
        data[i,j]<-37.5
      }
      if (data[i,j]=="4") {
        data[i,j]<-62.5
      }
      if (data[i,j]=="5") {
        data[i,j]<-87.5
      }
      if (data[i,j]=="1") {
        data[i,j]<-5
      }
    } else { data[i,j] <- 0 }
    if (class(data[i,j])=="character") {
      data[i,j]<-as.numeric(data[i,j])
    }
  }
}

# Transformation des classes des variables : 
data$Juncus_bulbosus <- as.numeric(data$Juncus_bulbosus)
data$Juncus_effusus_conglomeratus <- as.numeric(data$Juncus_effusus_conglomeratus)
data$Sphagnum_capillifolium <- as.numeric(data$Sphagnum_capillifolium)
data$Sphagnum_sp <- as.numeric(data$Sphagnum_sp)

# Obtention des valeurs pour les joncs totaux et les sphaignes totales :
data$Juncus_tot <- data$Juncus_bulbosus + data$Juncus_effusus_conglomeratus
data$Sphagnum_tot <- data$Sphagnum_capillifolium + data$Sphagnum_sp

# Changement de forme des dataframes
Juncus <- data[,c(1,2,7)]
Juncus <- Juncus %>%
  pivot_wider(names_from = date_du_releve, values_from = Juncus_tot)
Juncus$Placette<- as.numeric(gsub("P", "", Juncus$Placette))
Sphagnum <- data[,c(1,2,8)]
Sphagnum <- Sphagnum %>%
  pivot_wider(names_from = date_du_releve, values_from = Sphagnum_tot)
Sphagnum$Placette<- as.numeric(gsub("P", "", Sphagnum$Placette))

# Imputation des NAs
library(missMDA)
Juncus[,-1] <- as.data.frame(imputePCA(Juncus[,-1], ncp = 2)$completeObs)
Sphagnum[,-1] <- as.data.frame(imputePCA(Sphagnum[,-1], ncp = 2)$completeObs)

# Remplacement des valeurs négatives par des 0
Juncus[Juncus < 0] <- 0
Sphagnum[Sphagnum <0] <- 0

# Remodification de la forme des dataframes
Juncus <- melt(Juncus, id.vars = "Placette", variable.name = "date_du_releve", value.name = "Juncus_tot")
Sphagnum <- melt(Sphagnum, id.vars = "Placette", variable.name = "date_du_releve", value.name = "Sphagnum_tot")
data <- Sphagnum
data$Juncus_tot <- Juncus$Juncus_tot

# Representation des donnees : 
library(tidyverse)

# Pour Juncus total :
ggplot(data, aes(x=as.factor(date_du_releve),y=Juncus_tot)) +
  geom_boxplot(aes(color=as.factor(date_du_releve))) +
  xlab("Date") +
  ylab("% recouvrement total Juncus sp.") +
  ggtitle("Landemarais - Recouvrement Juncus sp.") +
  theme(legend.position = "none") +
  theme_classic()

# Pour Sphagnum total : 
ggplot(data, aes(x=as.factor(date_du_releve),y=Sphagnum_tot)) +
  geom_boxplot(aes(color=as.factor(date_du_releve))) +
  xlab("Date") +
  ylab("% recouvrement total Sphagnum sp.") +
  ggtitle("Landemarais - Recouvrement Sphagnum sp.") +
  theme(legend.position = "none") +
  theme_classic()

## On refait avec barplot car on a des % !!!! Donc mieux : 

## Pour Juncus total :

data$date_du_releve <- as.factor(data$date_du_releve)
se_j <- data %>% group_by(date_du_releve) %>% summarise(mean = mean(Juncus_tot), se = sqrt( (mean(Juncus_tot)*(mean(Juncus_tot)-1)) / (length(Juncus_tot)-1) ))

ggplot(data = se_j, aes(y = mean, x = date_du_releve, color= date_du_releve)) +
  geom_bar(position = position_dodge(), stat = "summary", fill = "white" ,width = 0.5, lwd = 0.8) + 
  geom_errorbar(aes(ymin = mean, ymax = mean+se), width = 0.2, lwd = 0.9) +
  theme_classic() +
  xlab("Date") +
  ylab("% recouvrement total Juncus sp.") +
  ggtitle("Landemarais - Recouvrement Juncus sp.") +
  theme(legend.position="none") 

## Pour Sphagnum total :

se_s <- data %>% group_by(date_du_releve) %>% summarise(mean = mean(Sphagnum_tot), se = sqrt( (mean(Sphagnum_tot)*(mean(Sphagnum_tot)-1)) / (length(Sphagnum_tot)-1) ))

ggplot(data = se_s, aes(y = mean, x = date_du_releve, color= date_du_releve)) +
  geom_bar(position = position_dodge(), stat = "summary", fill = "white" ,width = 0.5, lwd = 0.8) + 
  geom_errorbar(aes(ymin = mean, ymax = mean+se), width = 0.2, lwd = 0.9) +
  theme_classic() +
  xlab("Date") +
  ylab("% recouvrement total Sphagnum sp.") +
  ggtitle("Landemarais - Recouvrement Sphagnum sp.") +
  theme(legend.position="none") 

write.csv(data, "Abondance_juncus_sphagnum.csv", row.names = TRUE)

