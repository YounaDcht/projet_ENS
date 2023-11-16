##### Analyses statistiques Abondance Landemarais #####

# Importation des données -------------------------------------------------
data <- read.csv("Abondance_juncus_sphagnum.csv")[,-1]
colnames(data)[2] <- "Année"
Juncus <- data[,c(1,2,4)]
Juncus$Année <- as.factor(Juncus$Année)
Sphagnum <- data[,c(1,2,3)]
Sphagnum$Année <- as.factor(Sphagnum$Année)


# Vérification des outliers -----------------------------------------------
library(tidyverse)
library(rstatix)
# Juncus
Juncus %>%
  group_by(Année) %>%
  identify_outliers(Juncus_tot)
# Sphagnum
Sphagnum %>%
  group_by(Année) %>%
  identify_outliers(Sphagnum_tot)


# Vérification de la normalité --------------------------------------------
# Juncus
Juncus %>%
  group_by(Année) %>%
  shapiro_test(Juncus_tot)
# Sphagnum 
Sphagnum %>%
  group_by(Année) %>%
  shapiro_test(Sphagnum_tot)


# QQ plot -----------------------------------------------------------------
library(ggpubr)
# Juncus
ggqqplot(Juncus, "Juncus_tot", facet.by = "Année")
# Sphagnum
ggqqplot(Sphagnum, "Sphagnum_tot", facet.by = "Année")


# Analyses statistiques ---------------------------------------------------
# Juncus
res.juncus <- anova_test(data = Juncus, dv = Juncus_tot, wid = Placette, within = Année)
get_anova_table(res.juncus)
# Sphagnum 
res.sphagnum <- anova_test(data = Sphagnum, dv = Sphagnum_tot, wid = Placette, within = Année)
get_anova_table(res.juncus)


# Analyses post-hoc -------------------------------------------------------
# Juncus 
pwc.juncus <- Juncus %>%
  pairwise_t_test(
    Juncus_tot ~ Année, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc.juncus
# Sphagnum 
pwc.sphagnum <- Sphagnum %>%
  pairwise_t_test(
    Sphagnum_tot ~ Année, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc.sphagnum


# Représentations graphiques ----------------------------------------------
# Juncus
signif.ju <- pwc.juncus %>% filter(p.adj < 0.05)
coo.signif.ju <- signif.ju %>% add_xy_position(x = "Année")
coo.signif.ju$y.position <- seq(100,110,length.out=nrow(coo.signif.ju))
ggboxplot(Juncus, x = "Année", y = "Juncus_tot", add = "point") + 
  stat_pvalue_manual(coo.signif.ju) +
  labs(
    title = "Évolution de l'abondance de Juncus au cours des années",
    subtitle = get_test_label(res.juncus, detailed = TRUE),
    caption = get_pwc_label(coo.signif.ju)
  )
# Sphagnum
signif.sp <- pwc.sphagnum %>% filter(p.adj < 0.05)
coo.signif.sp <- signif.sp %>% add_xy_position(x = "Année")
coo.signif.sp$y.position <- seq(120,150,length.out=nrow(coo.signif.sp))
ggboxplot(Sphagnum, x = "Année", y = "Sphagnum_tot", add = "point") + 
  stat_pvalue_manual(coo.signif.sp) +
  labs(
    title = "Évolution de l'abondance de Sphagnum au cours des années",
    subtitle = get_test_label(res.sphagnum, detailed = TRUE),
    caption = get_pwc_label(coo.signif.sp)
  )
