##### Ouverture des donn?es : 
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

##### Conservation des lignes utilis?es seulement : dates et esp?ces 
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
P1<-t(P1)
P2<-t(P2)
P3<-t(P3)
P4<-t(P4)
P5<-t(P5)
P6<-t(P6)
P7<-t(P7)
P8<-t(P8)
P9<-t(P9)
P10<-t(P10)
P11<-t(P11)
P12<-t(P12)
P13<-t(P13)
P14<-t(P14)
P15<-t(P15)
P16<-t(P16)
P17<-t(P17)

##### Transformation des valeurs avec la correction de Gillet sur le recouvrement

# P1
for (i in 2:nrow(P1)) {
  for (j in 2:ncol(P1)) {
    if (P1[i,j]=="i") {
      P1[i,j]<-0
    }
    if (P1[i,j]=="r") {
      P1[i,j]<-0.01
    }
    if (P1[i,j]=="+") {
      P1[i,j]<-0.1
    }
    if (P1[i,j]=="2") {
      P1[i,j]<-17.5
    }
    if (P1[i,j]=="3") {
      P1[i,j]<-37.5
    }
    if (P1[i,j]=="4") {
      P1[i,j]<-62.5
    }
    if (P1[i,j]=="5") {
      P1[i,j]<-87.5
    }
    if (P1[i,j]=="1") {
      P1[i,j]<-5
    }
    if (class(P1[i,j])=="character") {
      P1[i,j]<-as.numeric(P1[i,j])
    }
  }
}

# P2
for (i in 2:nrow(P2)) {
  for (j in 2:ncol(P2)) {
    if (P2[i,j]=="i") {
      P2[i,j]<-0
    }
    if (P2[i,j]=="r") {
      P2[i,j]<-0.01
    }
    if (P2[i,j]=="+") {
      P2[i,j]<-0.1
    }
    if (P2[i,j]=="2") {
      P2[i,j]<-17.5
    }
    if (P2[i,j]=="3") {
      P2[i,j]<-37.5
    }
    if (P2[i,j]=="4") {
      P2[i,j]<-62.5
    }
    if (P2[i,j]=="5") {
      P2[i,j]<-87.5
    }
    if (P2[i,j]=="1") {
      P2[i,j]<-5
    }
    if (class(P2[i,j])=="character") {
      P2[i,j]<-as.numeric(P2[i,j])
    }
  }
}

# P3
for (i in 2:nrow(P3)) {
  for (j in 2:ncol(P3)) {
    if (P3[i,j]=="i") {
      P3[i,j]<-0
    }
    if (P3[i,j]=="r") {
      P3[i,j]<-0.01
    }
    if (P3[i,j]=="+") {
      P3[i,j]<-0.1
    }
    if (P3[i,j]=="2") {
      P3[i,j]<-17.5
    }
    if (P3[i,j]=="3") {
      P3[i,j]<-37.5
    }
    if (P3[i,j]=="4") {
      P3[i,j]<-62.5
    }
    if (P3[i,j]=="5") {
      P3[i,j]<-87.5
    }
    if (P3[i,j]=="1") {
      P3[i,j]<-5
    }
    if (class(P3[i,j])=="character") {
      P3[i,j]<-as.numeric(P3[i,j])
    }
  }
}

# P4
for (i in 2:nrow(P4)) {
  for (j in 2:ncol(P4)) {
    if (P4[i,j]=="i") {
      P4[i,j]<-0
    }
    if (P4[i,j]=="r") {
      P4[i,j]<-0.01
    }
    if (P4[i,j]=="+") {
      P4[i,j]<-0.1
    }
    if (P4[i,j]=="2") {
      P4[i,j]<-17.5
    }
    if (P4[i,j]=="3") {
      P4[i,j]<-37.5
    }
    if (P4[i,j]=="4") {
      P4[i,j]<-62.5
    }
    if (P4[i,j]=="5") {
      P4[i,j]<-87.5
    }
    if (P4[i,j]=="1") {
      P4[i,j]<-5
    }
    if (class(P4[i,j])=="character") {
      P4[i,j]<-as.numeric(P4[i,j])
    }
  }
}

# P5
for (i in 2:nrow(P5)) {
  for (j in 2:ncol(P5)) {
    if (P5[i,j]=="i") {
      P5[i,j]<-0
    }
    if (P5[i,j]=="r") {
      P5[i,j]<-0.01
    }
    if (P5[i,j]=="+") {
      P5[i,j]<-0.1
    }
    if (P5[i,j]=="2") {
      P5[i,j]<-17.5
    }
    if (P5[i,j]=="3") {
      P5[i,j]<-37.5
    }
    if (P5[i,j]=="4") {
      P5[i,j]<-62.5
    }
    if (P5[i,j]=="5") {
      P5[i,j]<-87.5
    }
    if (P5[i,j]=="1") {
      P5[i,j]<-5
    }
    if (class(P5[i,j])=="character") {
      P5[i,j]<-as.numeric(P5[i,j])
    }
  }
}

# P6
for (i in 2:nrow(P6)) {
  for (j in 2:ncol(P6)) {
    if (P6[i,j]=="i") {
      P6[i,j]<-0
    }
    if (P6[i,j]=="r") {
      P6[i,j]<-0.01
    }
    if (P6[i,j]=="+") {
      P6[i,j]<-0.1
    }
    if (P6[i,j]=="2") {
      P6[i,j]<-17.5
    }
    if (P6[i,j]=="3") {
      P6[i,j]<-37.5
    }
    if (P6[i,j]=="4") {
      P6[i,j]<-62.5
    }
    if (P6[i,j]=="5") {
      P6[i,j]<-87.5
    }
    if (P6[i,j]=="1") {
      P6[i,j]<-5
    }
    if (class(P6[i,j])=="character") {
      P6[i,j]<-as.numeric(P6[i,j])
    }
  }
}

# P7
for (i in 2:nrow(P7)) {
  for (j in 2:ncol(P7)) {
    if (P7[i,j]=="i") {
      P7[i,j]<-0
    }
    if (P7[i,j]=="r") {
      P7[i,j]<-0.01
    }
    if (P7[i,j]=="+") {
      P7[i,j]<-0.1
    }
    if (P7[i,j]=="2") {
      P7[i,j]<-17.5
    }
    if (P7[i,j]=="3") {
      P7[i,j]<-37.5
    }
    if (P7[i,j]=="4") {
      P7[i,j]<-62.5
    }
    if (P7[i,j]=="5") {
      P7[i,j]<-87.5
    }
    if (P7[i,j]=="1") {
      P7[i,j]<-5
    }
    if (class(P7[i,j])=="character") {
      P7[i,j]<-as.numeric(P7[i,j])
    }
  }
}

# P8
for (i in 2:nrow(P8)) {
  for (j in 2:ncol(P8)) {
    if (P8[i,j]=="i") {
      P8[i,j]<-0
    }
    if (P8[i,j]=="r") {
      P8[i,j]<-0.01
    }
    if (P8[i,j]=="+") {
      P8[i,j]<-0.1
    }
    if (P8[i,j]=="2") {
      P8[i,j]<-17.5
    }
    if (P8[i,j]=="3") {
      P8[i,j]<-37.5
    }
    if (P8[i,j]=="4") {
      P8[i,j]<-62.5
    }
    if (P8[i,j]=="5") {
      P8[i,j]<-87.5
    }
    if (P8[i,j]=="1") {
      P8[i,j]<-5
    }
    if (class(P8[i,j])=="character") {
      P8[i,j]<-as.numeric(P8[i,j])
    }
  }
}

# P9
for (i in 2:nrow(P9)) {
  for (j in 2:ncol(P9)) {
    if (P9[i,j]=="i") {
      P9[i,j]<-0
    }
    if (P9[i,j]=="r") {
      P9[i,j]<-0.01
    }
    if (P9[i,j]=="+") {
      P9[i,j]<-0.1
    }
    if (P9[i,j]=="2") {
      P9[i,j]<-17.5
    }
    if (P9[i,j]=="3") {
      P9[i,j]<-37.5
    }
    if (P9[i,j]=="4") {
      P9[i,j]<-62.5
    }
    if (P9[i,j]=="5") {
      P9[i,j]<-87.5
    }
    if (P9[i,j]=="1") {
      P9[i,j]<-5
    }
    if (class(P9[i,j])=="character") {
      P9[i,j]<-as.numeric(P9[i,j])
    }
  }
}

# P10
for (i in 2:nrow(P10)) {
  for (j in 2:ncol(P10)) {
    if (P10[i,j]=="i") {
      P10[i,j]<-0
    }
    if (P10[i,j]=="r") {
      P10[i,j]<-0.01
    }
    if (P10[i,j]=="+") {
      P10[i,j]<-0.1
    }
    if (P10[i,j]=="2") {
      P10[i,j]<-17.5
    }
    if (P10[i,j]=="3") {
      P10[i,j]<-37.5
    }
    if (P10[i,j]=="4") {
      P10[i,j]<-62.5
    }
    if (P10[i,j]=="5") {
      P10[i,j]<-87.5
    }
    if (P10[i,j]=="1") {
      P10[i,j]<-5
    }
    if (class(P10[i,j])=="character") {
      P10[i,j]<-as.numeric(P10[i,j])
    }
  }
}

# P11
for (i in 2:nrow(P11)) {
  for (j in 2:ncol(P11)) {
    if (P11[i,j]=="i") {
      P11[i,j]<-0
    }
    if (P11[i,j]=="r") {
      P11[i,j]<-0.01
    }
    if (P11[i,j]=="+") {
      P11[i,j]<-0.1
    }
    if (P11[i,j]=="2") {
      P11[i,j]<-17.5
    }
    if (P11[i,j]=="3") {
      P11[i,j]<-37.5
    }
    if (P11[i,j]=="4") {
      P11[i,j]<-62.5
    }
    if (P11[i,j]=="5") {
      P11[i,j]<-87.5
    }
    if (P11[i,j]=="1") {
      P11[i,j]<-5
    }
    if (class(P11[i,j])=="character") {
      P11[i,j]<-as.numeric(P11[i,j])
    }
  }
}

# P12
for (i in 2:nrow(P12)) {
  for (j in 2:ncol(P12)) {
    if (P12[i,j]=="i") {
      P12[i,j]<-0
    }
    if (P12[i,j]=="r") {
      P12[i,j]<-0.01
    }
    if (P12[i,j]=="+") {
      P12[i,j]<-0.1
    }
    if (P12[i,j]=="2") {
      P12[i,j]<-17.5
    }
    if (P12[i,j]=="3") {
      P12[i,j]<-37.5
    }
    if (P12[i,j]=="4") {
      P12[i,j]<-62.5
    }
    if (P12[i,j]=="5") {
      P12[i,j]<-87.5
    }
    if (P12[i,j]=="1") {
      P12[i,j]<-5
    }
    if (class(P12[i,j])=="character") {
      P12[i,j]<-as.numeric(P12[i,j])
    }
  }
}

# P13
for (i in 2:nrow(P13)) {
  for (j in 2:ncol(P13)) {
    if (P13[i,j]=="i") {
      P13[i,j]<-0
    }
    if (P13[i,j]=="r") {
      P13[i,j]<-0.01
    }
    if (P13[i,j]=="+") {
      P13[i,j]<-0.1
    }
    if (P13[i,j]=="2") {
      P13[i,j]<-17.5
    }
    if (P13[i,j]=="3") {
      P13[i,j]<-37.5
    }
    if (P13[i,j]=="4") {
      P13[i,j]<-62.5
    }
    if (P13[i,j]=="5") {
      P13[i,j]<-87.5
    }
    if (P13[i,j]=="1") {
      P13[i,j]<-5
    }
    if (class(P13[i,j])=="character") {
      P13[i,j]<-as.numeric(P13[i,j])
    }
  }
}

# P14
for (i in 2:nrow(P14)) {
  for (j in 2:ncol(P14)) {
    if (P14[i,j]=="i") {
      P14[i,j]<-0
    }
    if (P14[i,j]=="r") {
      P14[i,j]<-0.01
    }
    if (P14[i,j]=="+") {
      P14[i,j]<-0.1
    }
    if (P14[i,j]=="2") {
      P14[i,j]<-17.5
    }
    if (P14[i,j]=="3") {
      P14[i,j]<-37.5
    }
    if (P14[i,j]=="4") {
      P14[i,j]<-62.5
    }
    if (P14[i,j]=="5") {
      P14[i,j]<-87.5
    }
    if (P14[i,j]=="1") {
      P14[i,j]<-5
    }
    if (class(P14[i,j])=="character") {
      P14[i,j]<-as.numeric(P14[i,j])
    }
  }
}

# P15
for (i in 2:nrow(P15)) {
  for (j in 2:ncol(P15)) {
    if (P15[i,j]=="i") {
      P15[i,j]<-0
    }
    if (P15[i,j]=="r") {
      P15[i,j]<-0.01
    }
    if (P15[i,j]=="+") {
      P15[i,j]<-0.1
    }
    if (P15[i,j]=="2") {
      P15[i,j]<-17.5
    }
    if (P15[i,j]=="3") {
      P15[i,j]<-37.5
    }
    if (P15[i,j]=="4") {
      P15[i,j]<-62.5
    }
    if (P15[i,j]=="5") {
      P15[i,j]<-87.5
    }
    if (P15[i,j]=="1") {
      P15[i,j]<-5
    }
    if (class(P15[i,j])=="character") {
      P15[i,j]<-as.numeric(P15[i,j])
    }
  }
}

# P16
for (i in 2:nrow(P16)) {
  for (j in 2:ncol(P16)) {
    if (P16[i,j]=="i") {
      P16[i,j]<-0
    }
    if (P16[i,j]=="r") {
      P16[i,j]<-0.01
    }
    if (P16[i,j]=="+") {
      P16[i,j]<-0.1
    }
    if (P16[i,j]=="2") {
      P16[i,j]<-17.5
    }
    if (P16[i,j]=="3") {
      P16[i,j]<-37.5
    }
    if (P16[i,j]=="4") {
      P16[i,j]<-62.5
    }
    if (P16[i,j]=="5") {
      P16[i,j]<-87.5
    }
    if (P16[i,j]=="1") {
      P16[i,j]<-5
    }
    if (class(P16[i,j])=="character") {
      P16[i,j]<-as.numeric(P16[i,j])
    }
  }
}

# P17
for (i in 2:nrow(P17)) {
  for (j in 2:ncol(P17)) {
    if (P17[i,j]=="i") {
      P17[i,j]<-0
    }
    if (P17[i,j]=="r") {
      P17[i,j]<-0.01
    }
    if (P17[i,j]=="+") {
      P17[i,j]<-0.1
    }
    if (P17[i,j]=="2") {
      P17[i,j]<-17.5
    }
    if (P17[i,j]=="3") {
      P17[i,j]<-37.5
    }
    if (P17[i,j]=="4") {
      P17[i,j]<-62.5
    }
    if (P17[i,j]=="5") {
      P17[i,j]<-87.5
    }
    if (P17[i,j]=="1") {
      P17[i,j]<-5
    }
    if (class(P17[i,j])=="character") {
      P17[i,j]<-as.numeric(P17[i,j])
    }
  }
}



##### Creation du dataframe pour les trois indices a calculer : 

## On va ajouter les dates disponibles dans un vecteur :
## On va ensuite ajouter un vecteur qui conserve le num?ro de placette dans un vecteur :
k=1
dates<-NULL
placettes<-NULL

# P1 :
for (i in 2:nrow(P1)) {
  dates[k]<-substr(P1[i,1],5,6)
  placettes[k]<-1
  k<-k+1
}

# P2 :
for (i in 2:nrow(P2)) {
  dates[k]<-substr(P2[i,1],5,6)
  placettes[k]<-2
  k<-k+1
}

# P3 :
for (i in 2:nrow(P3)) {
  dates[k]<-substr(P3[i,1],5,6)
  placettes[k]<-3
  k<-k+1
}

# P4 :
for (i in 2:nrow(P4)) {
  dates[k]<-substr(P4[i,1],5,6)
  placettes[k]<-4
  k<-k+1
}

# P5 :
for (i in 2:nrow(P5)) {
  dates[k]<-substr(P5[i,1],5,6)
  placettes[k]<-5
  k<-k+1
}

# P6 :
for (i in 2:nrow(P6)) {
  dates[k]<-substr(P6[i,1],5,6)
  placettes[k]<-6
  k<-k+1
}

# P7 :
for (i in 2:nrow(P7)) {
  dates[k]<-substr(P7[i,1],5,6)
  placettes[k]<-7
  k<-k+1
}

# P8 :
for (i in 2:nrow(P8)) {
  dates[k]<-substr(P8[i,1],5,6)
  placettes[k]<-8
  k<-k+1
}

# P9 :
for (i in 2:nrow(P9)) {
  dates[k]<-substr(P9[i,1],5,6)
  placettes[k]<-9
  k<-k+1
}

# P10 :
for (i in 2:nrow(P10)) {
  dates[k]<-substr(P10[i,1],5,6)
  placettes[k]<-10
  k<-k+1
}

# P11 :
for (i in 2:nrow(P11)) {
  dates[k]<-substr(P11[i,1],5,6)
  placettes[k]<-11
  k<-k+1
}

# P12 :
for (i in 2:nrow(P12)) {
  dates[k]<-substr(P12[i,1],5,6)
  placettes[k]<-12
  k<-k+1
}

# P13 :
for (i in 2:nrow(P13)) {
  dates[k]<-substr(P13[i,1],5,6)
  placettes[k]<-13
  k<-k+1
}

# P14 :
for (i in 2:nrow(P14)) {
  dates[k]<-substr(P14[i,1],5,6)
  placettes[k]<-14
  k<-k+1
}

# P15 :
for (i in 2:nrow(P15)) {
  dates[k]<-substr(P15[i,1],5,6)
  placettes[k]<-15
  k<-k+1
}

# P16 :
for (i in 2:nrow(P16)) {
  dates[k]<-substr(P16[i,1],5,6)
  placettes[k]<-16
  k<-k+1
}

# P17 :
for (i in 2:nrow(P17)) {
  dates[k]<-substr(P17[i,1],5,6)
  placettes[k]<-17
  k<-k+1
}

## On cr?e le dataframe :
Indices<-as.data.frame(matrix(NA,nrow=length(dates),ncol=5))
colnames(Indices)<-c("Placette","Date","Shannon","Simpson","Hill")
Indices$Placette<-placettes
Indices$Date<-dates

#### Cacule de l'indice de Shannon et Simpson :

## Comme avant, on va tout r?cup?rer dans des vecteurs
shan<-NULL
simp<-NULL
k<-1

# P1 :
for ( i in 2:nrow(P1)) { # Chaque date de P1
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P1)) { # Chaque sp
    # Calcule Shannon
    if (P1[i,j]!=0) {
      stockSh[l]<-as.numeric(P1[i,j])/sum(as.numeric(P1[i,c(2:ncol(P1))])) * log(as.numeric(P1[i,j])/sum(as.numeric(P1[i,c(2:ncol(P1))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P1[i,j])/sum(as.numeric(P1[i,c(2:ncol(P1))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P2 :
for ( i in 2:nrow(P2)) { # Chaque date de P2
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P2)) { # Chaque sp
    # Calcule Shannon
    if (P2[i,j]!=0) {
      stockSh[l]<-as.numeric(P2[i,j])/sum(as.numeric(P2[i,c(2:ncol(P2))])) * log(as.numeric(P2[i,j])/sum(as.numeric(P2[i,c(2:ncol(P2))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P2[i,j])/sum(as.numeric(P2[i,c(2:ncol(P2))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P3 :
for ( i in 2:nrow(P3)) { # Chaque date de P3
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P3)) { # Chaque sp
    # Calcule Shannon
    if (P3[i,j]!=0) {
      stockSh[l]<-as.numeric(P3[i,j])/sum(as.numeric(P3[i,c(2:ncol(P3))])) * log(as.numeric(P3[i,j])/sum(as.numeric(P3[i,c(2:ncol(P3))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P3[i,j])/sum(as.numeric(P3[i,c(2:ncol(P3))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P4 :
for ( i in 2:nrow(P4)) { # Chaque date de P4
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P4)) { # Chaque sp
    # Calcule Shannon
    if (P4[i,j]!=0) {
      stockSh[l]<-as.numeric(P4[i,j])/sum(as.numeric(P4[i,c(2:ncol(P4))])) * log(as.numeric(P4[i,j])/sum(as.numeric(P4[i,c(2:ncol(P4))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P4[i,j])/sum(as.numeric(P4[i,c(2:ncol(P4))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P5 :
for ( i in 2:nrow(P5)) { # Chaque date de P5
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P5)) { # Chaque sp
    # Calcule Shannon
    if (P5[i,j]!=0) {
      stockSh[l]<-as.numeric(P5[i,j])/sum(as.numeric(P5[i,c(2:ncol(P5))])) * log(as.numeric(P5[i,j])/sum(as.numeric(P5[i,c(2:ncol(P5))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P5[i,j])/sum(as.numeric(P5[i,c(2:ncol(P5))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P6 :
for ( i in 2:nrow(P6)) { # Chaque date de P6
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P6)) { # Chaque sp
    # Calcule Shannon
    if (P6[i,j]!=0) {
      stockSh[l]<-as.numeric(P6[i,j])/sum(as.numeric(P6[i,c(2:ncol(P6))])) * log(as.numeric(P6[i,j])/sum(as.numeric(P6[i,c(2:ncol(P6))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P6[i,j])/sum(as.numeric(P6[i,c(2:ncol(P6))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P7 :
for ( i in 2:nrow(P7)) { # Chaque date de P7
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P7)) { # Chaque sp
    # Calcule Shannon
    if (P7[i,j]!=0) {
      stockSh[l]<-as.numeric(P7[i,j])/sum(as.numeric(P7[i,c(2:ncol(P7))])) * log(as.numeric(P7[i,j])/sum(as.numeric(P7[i,c(2:ncol(P7))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P7[i,j])/sum(as.numeric(P7[i,c(2:ncol(P7))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P8 :
for ( i in 2:nrow(P8)) { # Chaque date de P8
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P8)) { # Chaque sp
    # Calcule Shannon
    if (P8[i,j]!=0) {
      stockSh[l]<-as.numeric(P8[i,j])/sum(as.numeric(P8[i,c(2:ncol(P8))])) * log(as.numeric(P8[i,j])/sum(as.numeric(P8[i,c(2:ncol(P8))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P8[i,j])/sum(as.numeric(P8[i,c(2:ncol(P8))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P9 :
for ( i in 2:nrow(P9)) { # Chaque date de P9
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P9)) { # Chaque sp
    # Calcule Shannon
    if (P9[i,j]!=0) {
      stockSh[l]<-as.numeric(P9[i,j])/sum(as.numeric(P9[i,c(2:ncol(P9))])) * log(as.numeric(P9[i,j])/sum(as.numeric(P9[i,c(2:ncol(P9))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P9[i,j])/sum(as.numeric(P9[i,c(2:ncol(P9))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P10 :
for ( i in 2:nrow(P10)) { # Chaque date de P10
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P10)) { # Chaque sp
    # Calcule Shannon
    if (P10[i,j]!=0) {
      stockSh[l]<-as.numeric(P10[i,j])/sum(as.numeric(P10[i,c(2:ncol(P10))])) * log(as.numeric(P10[i,j])/sum(as.numeric(P10[i,c(2:ncol(P10))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P10[i,j])/sum(as.numeric(P10[i,c(2:ncol(P10))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P11 :
for ( i in 2:nrow(P11)) { # Chaque date de P11
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P11)) { # Chaque sp
    # Calcule Shannon
    if (P11[i,j]!=0) {
      stockSh[l]<-as.numeric(P11[i,j])/sum(as.numeric(P11[i,c(2:ncol(P11))])) * log(as.numeric(P11[i,j])/sum(as.numeric(P11[i,c(2:ncol(P11))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P11[i,j])/sum(as.numeric(P11[i,c(2:ncol(P11))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P12 :
for ( i in 2:nrow(P12)) { # Chaque date de P12
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P12)) { # Chaque sp
    # Calcule Shannon
    if (P12[i,j]!=0) {
      stockSh[l]<-as.numeric(P12[i,j])/sum(as.numeric(P12[i,c(2:ncol(P12))])) * log(as.numeric(P12[i,j])/sum(as.numeric(P12[i,c(2:ncol(P12))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P12[i,j])/sum(as.numeric(P12[i,c(2:ncol(P12))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P13 :
for ( i in 2:nrow(P13)) { # Chaque date de P13
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P13)) { # Chaque sp
    # Calcule Shannon
    if (P13[i,j]!=0) {
      stockSh[l]<-as.numeric(P13[i,j])/sum(as.numeric(P13[i,c(2:ncol(P13))])) * log(as.numeric(P13[i,j])/sum(as.numeric(P13[i,c(2:ncol(P13))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P13[i,j])/sum(as.numeric(P13[i,c(2:ncol(P13))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P14 :
for ( i in 2:nrow(P14)) { # Chaque date de P14
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P14)) { # Chaque sp
    # Calcule Shannon
    if (P14[i,j]!=0) {
      stockSh[l]<-as.numeric(P14[i,j])/sum(as.numeric(P14[i,c(2:ncol(P14))])) * log(as.numeric(P14[i,j])/sum(as.numeric(P14[i,c(2:ncol(P14))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P14[i,j])/sum(as.numeric(P14[i,c(2:ncol(P14))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P15 :
for ( i in 2:nrow(P15)) { # Chaque date de P15
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P15)) { # Chaque sp
    # Calcule Shannon
    if (P15[i,j]!=0) {
      stockSh[l]<-as.numeric(P15[i,j])/sum(as.numeric(P15[i,c(2:ncol(P15))])) * log(as.numeric(P15[i,j])/sum(as.numeric(P15[i,c(2:ncol(P15))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P15[i,j])/sum(as.numeric(P15[i,c(2:ncol(P15))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P16 :
for ( i in 2:nrow(P16)) { # Chaque date de P16
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P16)) { # Chaque sp
    # Calcule Shannon
    if (P16[i,j]!=0) {
      stockSh[l]<-as.numeric(P16[i,j])/sum(as.numeric(P16[i,c(2:ncol(P16))])) * log(as.numeric(P16[i,j])/sum(as.numeric(P16[i,c(2:ncol(P16))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P16[i,j])/sum(as.numeric(P16[i,c(2:ncol(P16))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

# P17 :
for ( i in 2:nrow(P17)) { # Chaque date de P17
  l<-1
  stockSh=NULL
  stockSi=NULL
  for (j in 2:ncol(P17)) { # Chaque sp
    # Calcule Shannon
    if (P17[i,j]!=0) {
      stockSh[l]<-as.numeric(P17[i,j])/sum(as.numeric(P17[i,c(2:ncol(P17))])) * log(as.numeric(P17[i,j])/sum(as.numeric(P17[i,c(2:ncol(P17))])))
    } else { stockSh[l]<-0}
    # Calcule Simpson :
    stockSi[l]<-(as.numeric(P17[i,j])/sum(as.numeric(P17[i,c(2:ncol(P17))])))^2
    l<-l+1
  }
  shan[k]<- -sum(stockSh)
  simp[k]<- sum(stockSi)
  k<-k+1
}

## On ajoute les vecteurs au data.frame :
Indices$Shannon<-shan
Indices$Simpson<-simp

## On calcule ensuite l'indice de Hill : 
Indices$Hill<-((Indices$Simpson^-1)/(exp(Indices$Shannon)))
Indices$Hill<- 1 - Indices$Hill
Indices$Placette<-as.character(Indices$Placette)

### Modification et exportation de trois data.frame
library(reshape2)
final.Shannon <- dcast(Indices, Placette ~ Date, value.var="Shannon")
final.Simpson <- dcast(Indices, Placette ~ Date, value.var="Simpson")
final.Hill <- dcast(Indices, Placette ~ Date, value.var="Hill")

write.csv(final.Shannon, "Landemarais Shannon.csv", row.names = TRUE)
write.csv(final.Simpson, "Landemarais Simpson.csv", row.names = TRUE)
write.csv(final.Hill, "Landemarais Hill.csv", row.names = TRUE)
