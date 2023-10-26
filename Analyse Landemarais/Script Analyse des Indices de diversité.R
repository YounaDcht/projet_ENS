##### Analyses statistiques Landemarais #####


# Importation des données -------------------------------------------------
Shannon <- read.csv("Imputed Landemarais Shannon.csv")[,-1]
Simpson <- read.csv("Imputed Landemarais Simpson.csv")[,-1]
Hill <- read.csv("Imputed Landemarais Hill.csv")[,-1]


# Représentations graphiques ----------------------------------------------
library(ggplot2)
library(reshape2)

# Ajustement des tableaux de données
# Avec NAs
Shannon.plot <- melt(Shannon, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
Simpson.plot <- melt(Simpson, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
Hill.plot <- melt(Hill, id.vars = "Placette", variable.name = "Année", value.name = "Indice")

# Shannon
ggplot(Shannon.plot, aes(x = Année, y = Indice)) +
  geom_boxplot(aes(color = Année)) +
  ggtitle("Évolution de l'indice de diversité de Shannon") +
  theme_minimal() +
  theme(legend.position = "none")
# Simpson
ggplot(Simpson.plot, aes(x = Année, y = Indice)) +
  geom_boxplot(aes(color = Année)) +
  ggtitle("Évolution de l'indice de diversité de Simpson") +
  theme_minimal() +
  theme(legend.position = "none")
# Hill
ggplot(Hill.plot, aes(x = Année, y = Indice)) +
  geom_boxplot(aes(color = Année)) +
  ggtitle("Évolution de l'indice de diversité de Hill") +
  theme_minimal() +
  theme(legend.position = "none")


# Vérification de la normalité des individus ------------------------------
library(tidyverse)
library(rstatix)
# Shannon
Shannon.plot %>%
  group_by(Année) %>%
  shapiro_test(Indice)
# Simpson
Simpson.plot %>%
  group_by(Année) %>%
  shapiro_test(Indice)

https://www.datanovia.com/en/fr/lessons/anova-sur-mesures-repetees-dans-r/#anova-a-un-facteur-sur-mesures-repetees


# Analyses statistiques ---------------------------------------------------
# Shannon
mod <- aov(Indice ~ Année + Error(Placette/Année), data = Shannon.rs)
summary(mod)
# Simpson 
mod <- aov(Indice ~ Année, data = Shannon.plot)
summary(mod)