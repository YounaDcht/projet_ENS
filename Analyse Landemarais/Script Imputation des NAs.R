##### Imputation des NAs pour les indices de biodiversité #####

# Importation des données -------------------------------------------------
Shannon <- read.csv("Landemarais Shannon.csv")[,-1]
Simpson <- read.csv("Landemarais Simpson.csv")[,-1]
Hill <- read.csv("Landemarais Hill.csv")[,-1]


# Imputation des NAs ------------------------------------------------------
library(missMDA)
impute.Shannon <- as.data.frame(imputePCA(Shannon, ncp = 2)$completeObs)
impute.Simpson <- as.data.frame(imputePCA(Simpson, ncp = 2)$completeObs)
impute.Hill <- as.data.frame(imputePCA(Hill, ncp = 2)$completeObs)


# Reshape des dataframes --------------------------------------------------
## Avec NAs
Shannon.rs <- melt(Shannon, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
Simpson.rs <- melt(Simpson, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
Hill.rs <- melt(Hill, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
## Sans NAs
impute.Shannon.rs <- melt(impute.Shannon, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
impute.Simpson.rs <- melt(impute.Simpson, id.vars = "Placette", variable.name = "Année", value.name = "Indice")
impute.Hill.rs <- melt(impute.Hill, id.vars = "Placette", variable.name = "Année", value.name = "Indice")


# Statistiques descriptives -----------------------------------------------
## Shannon
# Avec NAs
Shannon.rs %>%
  group_by(Année) %>%
  get_summary_stats(Indice, type = "mean_sd")
# Sans NAs
impute.Shannon.rs %>%
  group_by(Année) %>%
  get_summary_stats(Indice, type = "mean_sd")
## Simpson
# Avec NAs
Simpson.rs %>%
  group_by(Année) %>%
  get_summary_stats(Indice, type = "mean_sd")
# Sans NAs
impute.Simpson.rs %>%
  group_by(Année) %>%
  get_summary_stats(Indice, type = "mean_sd")
## Hill
# Avec NAs
Hill.rs %>%
  group_by(Année) %>%
  get_summary_stats(Indice, type = "mean_sd")
# Sans NAs
impute.Hill.rs %>%
  group_by(Année) %>%
  get_summary_stats(Indice, type = "mean_sd")


# Comparaisons graphiques -------------------------------------------------
## Combinaison des dataframes
# Shannon
Shannon.rs$Imputed <- rep("FALSE",nrow(Shannon.rs))
impute.Shannon.rs$Imputed <- rep("TRUE",nrow(impute.Shannon.rs))
bind.Shannon <- rbind(Shannon.rs, impute.Shannon.rs)
# Simpson
Simpson.rs$Imputed <- rep("FALSE",nrow(Simpson.rs))
impute.Simpson.rs$Imputed <- rep("TRUE",nrow(impute.Simpson.rs))
bind.Simpson <- rbind(Simpson.rs, impute.Simpson.rs)
# Hill
Hill.rs$Imputed <- rep("FALSE",nrow(Hill.rs))
impute.Hill.rs$Imputed <- rep("TRUE",nrow(impute.Hill.rs))
bind.Hill <- rbind(Hill.rs, impute.Hill.rs)

## Nested categorical boxplot
# Shannon
ggplot(bind.Shannon, aes(x = Année, y = Indice, fill = Imputed)) +
  geom_boxplot() +
  labs(title = "Nested Categorical Boxplot", x = "Category", y = "Value") +
  theme_minimal()
# Simpson
ggplot(bind.Simpson, aes(x = Année, y = Indice, fill = Imputed)) +
  geom_boxplot() +
  labs(title = "Nested Categorical Boxplot", x = "Category", y = "Value") +
  theme_minimal()
# Hill
ggplot(bind.Hill, aes(x = Année, y = Indice, fill = Imputed)) +
  geom_boxplot() +
  labs(title = "Nested Categorical Boxplot", x = "Category", y = "Value") +
  theme_minimal()


# Exportation des données
write.csv(impute.Shannon, "Imputed Landemarais Shannon.csv", row.names = TRUE)
write.csv(impute.Simpson, "Imputed Landemarais Simpson.csv", row.names = TRUE)
write.csv(impute.Hill, "Imputed Landemarais Hill.csv", row.names = TRUE)
