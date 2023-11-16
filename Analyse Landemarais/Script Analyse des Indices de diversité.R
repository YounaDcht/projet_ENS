##### Analyses statistiques Landemarais #####


# Importation des données -------------------------------------------------
Shannon <- read.csv("Imputed Landemarais Shannon.csv")[,-1]
Simpson <- read.csv("Imputed Landemarais Simpson.csv")[,-1]
Hill <- read.csv("Imputed Landemarais Hill.csv")[,-1]


# Représentations graphiques ----------------------------------------------
library(ggplot2)
library(reshape2)

# Ajustement des tableaux de données
# Avec NA imputés
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


# Vérification des outliers -----------------------------------------------
library(tidyverse)
library(rstatix)
# Shannon
Shannon.plot %>%
  group_by(Année) %>%
  identify_outliers(Indice)
# Simpson
Simpson.plot %>%
  group_by(Année) %>%
  identify_outliers(Indice)
# Hill
Hill.plot %>%
  group_by(Année) %>%
  identify_outliers(Indice)


# Vérification de la normalité des individus ------------------------------
# Shannon
Shannon.plot %>%
  group_by(Année) %>%
  shapiro_test(Indice)
# Simpson
Simpson.plot %>%
  group_by(Année) %>%
  shapiro_test(Indice)
# Hill
Hill.plot %>%
  group_by(Année) %>%
  shapiro_test(Indice)


# QQ Plot -----------------------------------------------------------------
library(ggpubr)
# Shannon
ggqqplot(Shannon.plot, "Indice", facet.by = "Année")
# Simpson
ggqqplot(Simpson.plot, "Indice", facet.by = "Année")
# Hill
ggqqplot(Hill.plot, "Indice", facet.by = "Année")


# Analyses statistiques ---------------------------------------------------
# Shannon
res.shannon <- anova_test(data = Shannon.plot, dv = Indice, wid = Placette, within = Année)
get_anova_table(res.shannon)
# Simpson
res.simpson <- anova_test(data = Simpson.plot, dv = Indice, wid = Placette, within = Année)
get_anova_table(res.simpson)
# Hill
res.hill <- anova_test(data = Hill.plot, dv = Indice, wid = Placette, within = Année)
get_anova_table(res.hill)


# Tests post-hoc ----------------------------------------------------------
# Shannon 
pwc.shannon <- Shannon.plot %>%
  pairwise_t_test(
    Indice ~ Année, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc.shannon
# Simpson
pwc.simpson <- Simpson.plot %>%
  pairwise_t_test(
    Indice ~ Année, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc.simpson
# Hill
pwc.hill <- Hill.plot %>%
  pairwise_t_test(
    Indice ~ Année, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc.hill


# Représentations graphiques ----------------------------------------------
# Shannon
signif.sh <- pwc.shannon %>% filter(p.adj < 0.05)
coo.signif.sh <- signif.sh %>% add_xy_position(x = "Année")
coo.signif.sh$y.position <- seq(3.7,5,length.out=nrow(coo.signif.sh))
ggboxplot(Shannon.plot, x = "Année", y = "Indice", add = "point") + 
  stat_pvalue_manual(coo.signif.sh) +
  labs(
    title = "Évolution de l'indice de Shannon au cours des années",
    subtitle = get_test_label(res.shannon, detailed = TRUE),
    caption = get_pwc_label(coo.signif.sh)
  )
# Simpson
signif.si <- pwc.simpson %>% filter(p.adj < 0.05)
coo.signif.si <- signif.si %>% add_xy_position(x = "Année")
coo.signif.si$y.position <- seq(1.2,1.5,length.out=nrow(coo.signif.si))
ggboxplot(Simpson.plot, x = "Année", y = "Indice", add = "point") + 
  stat_pvalue_manual(coo.signif.si) +
  labs(
    title = "Évolution de l'indice de Simpson au cours des années",
    subtitle = get_test_label(res.simpson, detailed = TRUE),
    caption = get_pwc_label(coo.signif.si)
  )
# Hill
signif.hi <- pwc.hill %>% filter(p.adj < 0.05)
coo.signif.hi <- signif.hi %>% add_xy_position(x = "Année")
ggboxplot(Hill.plot, x = "Année", y = "Indice", add = "point") + 
  stat_pvalue_manual(coo.signif.hi) +
  labs(
    title = "Évolution de l'indice de Hill au cours des années",
    subtitle = get_test_label(res.hill, detailed = TRUE),
    caption = get_pwc_label(coo.signif.hi)
  )


# Tests plus robustes (Non-normalité, outliers) ---------------------------
# https://methodenlehre.github.io/intro-to-rstats/mean-comparisons.html#multiple-dependent-groups
library(jmv)
# Shannon
test.sh <- anovaRMNP(
  data = Shannon,
  measures = vars(X12, X13, X14, X15, X16, X17, X19, X21, X22),
  pairs = TRUE,
  plots = TRUE,
  plotType = "medians")
test.sh$table
test.sh$comp
test.sh$plot
# Simpson
test.si <- anovaRMNP(
  data = Simpson,
  measures = vars(X12, X13, X14, X15, X16, X17, X19, X21, X22),
  pairs = TRUE,
  plots = TRUE,
  plotType = "medians")
test.si$table
test.si$comp
test.si$plot
#Hill
test.hi <- anovaRMNP(
  data = Hill,
  measures = vars(X12, X13, X14, X15, X16, X17, X19, X21, X22),
  pairs = TRUE,
  plots = TRUE,
  plotType = "medians")
test.hi$table
test.hi$comp
test.hi$plot

# Nouvelles représentations graphiques ------------------------------------
# Shannon
pwc.shannon$p <- as.data.frame(test.sh$comp)$p
pwc.shannon$p.adj.signif <- ifelse(pwc.shannon$p <= 0.001, "***", ifelse(pwc.shannon$p <= 0.01, "**", 
                                                                        ifelse(pwc.shannon$p <= 0.05, "*", "")))
signif.sh <- pwc.shannon %>% filter(p < 0.05)
coo.signif.sh <- signif.sh %>% add_xy_position(x = "Année")
coo.signif.sh$y.position <- seq(3.2,8,length.out=nrow(coo.signif.sh))
ggboxplot(Shannon.plot, x = "Année", y = "Indice", add = "point") + 
  stat_pvalue_manual(coo.signif.sh) +
  labs(
    title = "Évolution de l'indice de Shannon au cours des années",
  )
# Simpson
pwc.simpson$p <- as.data.frame(test.si$comp)$p
pwc.simpson$p.adj.signif <- ifelse(pwc.simpson$p <= 0.001, "***", ifelse(pwc.simpson$p <= 0.01, "**", 
                                                                         ifelse(pwc.simpson$p <= 0.05, "*", "")))
signif.si <- pwc.simpson %>% filter(p < 0.05)
coo.signif.si <- signif.si %>% add_xy_position(x = "Année")
coo.signif.si$y.position <- seq(1.5,8,length.out=nrow(coo.signif.si))
ggboxplot(Simpson.plot, x = "Année", y = "Indice", add = "point") + 
  stat_pvalue_manual(coo.signif.si) +
  labs(
    title = "Évolution de l'indice de Simpson au cours des années",
  )
# Hill
pwc.hill$p <- as.data.frame(test.hi$comp)$p
pwc.hill$p.adj.signif <- ifelse(pwc.hill$p <= 0.001, "***", ifelse(pwc.hill$p <= 0.01, "**", 
                                                                         ifelse(pwc.hill$p <= 0.05, "*", "")))
signif.hi <- pwc.hill %>% filter(p < 0.05)
coo.signif.hi <- signif.hi %>% add_xy_position(x = "Année")
coo.signif.hi$y.position <- seq(1.2,2,length.out=nrow(coo.signif.hi))
ggboxplot(Hill.plot, x = "Année", y = "Indice", add = "point") + 
  stat_pvalue_manual(coo.signif.hi) +
  labs(
    title = "Évolution de l'indice de Simpson au cours des années",
  )
