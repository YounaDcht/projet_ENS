##### Ouverture des données : 
P1<-read.table("placette1.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P2<-read.table("placette2.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P3<-read.table("placette3.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P4<-read.table("placette4.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P5<-read.table("placette5.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P6<-read.table("placette6.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P7<-read.table("placette7.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P8<-read.table("placette8.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P9<-read.table("placette9.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P10<-read.table("placette10.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P11<-read.table("placette11.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P12<-read.table("placette12.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P13<-read.table("placette13.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P14<-read.table("placette14.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P15<-read.table("placette15.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P16<-read.table("placette16.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)
P17<-read.table("placette17.txt", header = F, stringsAsFactors = TRUE, dec = ".",na.strings = c("","NA"), fill = TRUE)

##### Conservation des lignes utilisées seulement : dates et espèces 
P1<-P1[-c(seq(2,14)),]
P2<-P2[-c(seq(2,16)),]
P3<-P3[-c(seq(2,16)),]
P4<-P4[-c(seq(2,16)),]
P5<-P5[-c(seq(2,16)),]
P6<-P6[-c(seq(2,16)),]
P7<-P7[-c(seq(2,16)),]
P8<-P8[-c(seq(2,16)),]
P9<-P9[-c(seq(2,14)),]
P10<-P10[-c(seq(2,14)),]
P11<-P11[-c(seq(2,14)),]
P12<-P12[-c(seq(2,16)),]
P13<-P13[-c(seq(2,14)),]
P14<-P14[-c(seq(2,16)),]
P15<-P15[-c(seq(2,15)),]
P16<-P16[-c(seq(2,14)),]
P17<-P17[-c(seq(2,14)),]

##### Rotation des dataframes :
P1<-as.data.frame(t(P1))
P2<-as.data.frame(t(P2))
P3<-as.data.frame(t(P3))
P4<-as.data.frame(t(P4))
P5<-as.data.frame(t(P5))
P6<-as.data.frame(t(P6))
P7<-as.data.frame(t(P7))
P8<-as.data.frame(t(P8))
P9<-as.data.frame(t(P9))
P10<-as.data.frame(t(P10))
P11<-as.data.frame(t(P11))
P12<-as.data.frame(t(P12))
P13<-as.data.frame(t(P13))
P14<-as.data.frame(t(P14))
P15<-as.data.frame(t(P15))
P16<-as.data.frame(t(P16))
P17<-as.data.frame(t(P17))

# Passage de la première ligne en nom de colonnes :
colnames(P1) <- P1[1,]
colnames(P2) <- P2[1,]
colnames(P3) <- P3[1,]
colnames(P4) <- P4[1,]
colnames(P5) <- P5[1,]
colnames(P6) <- P6[1,]
colnames(P7) <- P7[1,]
colnames(P8) <- P8[1,]
colnames(P9) <- P9[1,]
colnames(P10) <- P10[1,]
colnames(P11) <- P11[1,]
colnames(P12) <- P12[1,]
colnames(P13) <- P13[1,]
colnames(P14) <- P14[1,]
colnames(P15) <- P15[1,]
colnames(P16) <- P16[1,]
colnames(P17) <- P17[1,]

# Delete first row:
P1 <- P1[-1,]
P2 <- P2[-1,]
P3 <- P3[-1,]
P4 <- P4[-1,]
P5 <- P5[-1,]
P6 <- P6[-1,]
P7 <- P7[-1,]
P8 <- P8[-1,]
P9 <- P9[-1,]
P10 <- P10[-1,]
P11 <- P11[-1,]
P12 <- P12[-1,]
P13 <- P13[-1,]
P14 <- P14[-1,]
P15 <- P15[-1,]
P16 <- P16[-1,]
P17 <- P17[-1,]

# Ajout d'une colonne avec le nom de la parcelle :
P1 <- mutate(P1,Placette = "P1")
P2 <- mutate(P2,Placette = "P2")
P3 <- mutate(P3,Placette = "P3")
P4 <- mutate(P4,Placette = "P4")
P5 <- mutate(P5,Placette = "P5")
P6 <- mutate(P6,Placette = "P6")
P7 <- mutate(P7,Placette = "P7")
P8 <- mutate(P8,Placette = "P8")
P9 <- mutate(P9,Placette = "P9")
P10 <- mutate(P10,Placette = "P10")
P11 <- mutate(P11,Placette = "P11")
P12 <- mutate(P12,Placette = "P12")
P13 <- mutate(P13,Placette = "P13")
P14 <- mutate(P14,Placette = "P14")
P15 <- mutate(P15,Placette = "P15")
P16 <- mutate(P16,Placette = "P16")
P17 <- mutate(P17,Placette = "P17")

# Fusion de data.frames :
library(tidyverse)
Final <- bind_rows(list(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17))

# Re-organisation :
rownames(Final) <- 1:116
Final$date_du_releve <- substring(Final$date_du_releve,5,6)
Final <- Final[,c(1,19,2:18,20:69)]
Final <- Final[,c(2,1,3:69)]
ordre <- sort(colnames(Final[,3:69]))
Final <- Final[,c("Placette","date_du_releve",ordre)]

# Exportation : 
write.table(Final, "Tableau_global.txt", sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

colnames(Final)
