# Load Packages ----
source('loadpackages.R')

# Read csv files ----
files <- c("carbone", "azote","shade_shade_group")
source('readdatafiles.R')
readdatafiles("data/", files,"_df", ".csv")

# Creating data frames ----
small_df<-azote_df %>% 
  left_join(carbone_df, by='tree_id') #joining carbone data with azote (concentration mg/g)

mid_df <- small_df %>% #separatn tree_id info into sep columns for
  mutate(
    ech = as.numeric(str_extract(tree_id, "^[^_]+")),       #sample
    species = str_extract(tree_id, "(?<=_)[A-Za-z]+"),      #esp
    tree_id = str_remove(tree_id, "^[^_]+_")                #keeping indivual tree_id (just removing sample nb)
  )

full_df<-mid_df %>%
  left_join(shade_group_df, by='species')
full_df<-full_df %>%
  mutate(
    shade_group<-as.factor(shade_group)
  )

nc_df<-full_df %>%
  mutate(
    nc= full_df$n/full_df$carbone
  )

#set shade groupe comme factor

#variables
hist(nc_df$n)
hist(nc_df$carbone)
hist(nc_df$nc)

str(nc_df)

model1<-lm(nc~ech*shade_group, data=nc_df)
summary(model1)
visreg(model1)

nre_df <- full_df %>%
  mutate(
    nc = n / carbone
  ) %>%
  select(tree_id, species, shade_group, ech, nc) %>%
  pivot_wider(
    names_from = ech,
    values_from = nc,
    names_prefix = "ech"
  ) %>%
  mutate(
    NRE = (1 - (ech2 / ech1)) * 100
  )

#VOIR DES AFFAIRES
#Je veux voir mon nuage de point selon l'espèce et selon le groupe pour voir si on a de la bisbille

##code de claude----------------------------------------------------------------

# Moyenne par espèce
moy <- aggregate(NRE ~ species + shade_group, data = reabs_df, FUN = mean)

nre_df$species     <- factor(nre_df$species)
nre_df$shade_group <- factor(nre_df$shade_group)

# Table espèce -> groupe -> position x
sp_tab <- unique(data.frame(species     = as.character(nre_df$species),
                            shade_group = as.character(nre_df$shade_group)))
sp_tab <- sp_tab[order(sp_tab$shade_group, sp_tab$species), ]
sp_tab$rank <- ave(seq_len(nrow(sp_tab)), sp_tab$shade_group, FUN = seq_along)
sp_tab$n    <- ave(sp_tab$rank,           sp_tab$shade_group, FUN = max)
sp_tab$gi   <- match(sp_tab$shade_group, levels(nre_df$shade_group))

w <- 0.25  # écart horizontal entre espèces d'un même groupe
sp_tab$x <- sp_tab$gi + (sp_tab$rank - (sp_tab$n + 1) / 2) * w

# Position x ajoutée aux individus
nre_df$x <- sp_tab$x[match(as.character(nre_df$species), sp_tab$species)]

# Moyennes par espèce, avec la même position x
moy <- aggregate(NRE ~ species + shade_group, data = nre_df, FUN = mean)
moy$x <- sp_tab$x[match(as.character(moy$species), sp_tab$species)]

# Couleurs : une par espèce
sp_levels <- sp_tab$species
# Palette "Dark2" : couleurs plus foncées, bien visibles sur fond blanc
cols <- setNames(RColorBrewer::brewer.pal(max(3, length(sp_levels)), "Dark2")[seq_along(sp_levels)],
                 sp_levels)
noms_groupes <- c("light" = "Intolérantes à l'ombre",
                  "shade" = "Tolérantes à l'ombre")
noms <- c("BP" = "Betula papyrifera",
          "EP" = "Acer pensylvanicum",
          "ES" = "Acer saccharum",
          "H" = "Fagus grandifolia",
          "PGD" = "Populus grandidentata",
          "S" = "Salix bebbiana/eriocephala")
ggplot() +
  # individus
  geom_point(data = nre_df, aes(x = x, y = NRE, color = species),
             size = 2.5, alpha = 0.7) +
  # moyennes : contour noir opaque, remplissage semi-transparent
  geom_point(data = moy, aes(x = x, y = NRE, fill = species),
             shape = 23, size = 6, color = "black", stroke = 1) +
  scale_color_manual(values = cols,
                     labels = noms[sp_levels],
                     name = "Espèce") +
  scale_fill_manual(values = alpha(cols, 0.4)) +
  guides(fill = "none") +
  scale_x_continuous(breaks = seq_along(levels(nre_df$shade_group)),
                     labels = noms_groupes[levels(nre_df$shade_group)])  +
  labs(x = "Groupe",
       y = "Efficacité de réabsorption de l'azote (%)") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"))

### Code pour le carbone maintenant
# Moyenne par espèce
moy <- aggregate(CRE ~ species + shade_group, data = reabs_df, FUN = mean)

reabs_df$species     <- factor(reabs_df$species)
reabs_df$shade_group <- factor(reabs_df$shade_group)

# Table espèce -> groupe -> position x
sp_tab <- unique(data.frame(species     = as.character(reabs_df$species),
                            shade_group = as.character(reabs_df$shade_group)))
sp_tab <- sp_tab[order(sp_tab$shade_group, sp_tab$species), ]
sp_tab$rank <- ave(seq_len(nrow(sp_tab)), sp_tab$shade_group, FUN = seq_along)
sp_tab$n    <- ave(sp_tab$rank,           sp_tab$shade_group, FUN = max)
sp_tab$gi   <- match(sp_tab$shade_group, levels(reabs_df$shade_group))

w <- 0.25  # écart horizontal entre espèces d'un même groupe
sp_tab$x <- sp_tab$gi + (sp_tab$rank - (sp_tab$n + 1) / 2) * w

# Position x ajoutée aux individus
reabs_df$x <- sp_tab$x[match(as.character(reabs_df$species), sp_tab$species)]

# Moyennes par espèce, avec la même position x
moy <- aggregate(CRE ~ species + shade_group, data = reabs_df, FUN = mean)
moy$x <- sp_tab$x[match(as.character(moy$species), sp_tab$species)]

# Couleurs : une par espèce
sp_levels <- sp_tab$species
# Palette "Dark2" : couleurs plus foncées, bien visibles sur fond blanc
cols <- setNames(RColorBrewer::brewer.pal(max(3, length(sp_levels)), "Dark2")[seq_along(sp_levels)],
                 sp_levels)
noms_groupes <- c("light" = "Intolérantes à l'ombre",
                  "shade" = "Tolérantes à l'ombre")
noms <- c("BP" = "Betula papyrifera",
          "EP" = "Acer pensylvanicum",
          "ES" = "Acer saccharum",
          "H" = "Fagus grandifolia",
          "PGD" = "Populus grandidentata",
          "S" = "Salix bebbiana/eriocephala")
ggplot() +
  # individus
  geom_point(data = reabs_df, aes(x = x, y = CRE, color = species),
             size = 2.5, alpha = 0.7) +
  # moyennes : contour noir opaque, remplissage semi-transparent
  geom_point(data = moy, aes(x = x, y = CRE, fill = species),
             shape = 23, size = 6, color = "black", stroke = 1) +
  scale_color_manual(values = cols,
                     labels = noms[sp_levels],
                     name = "Espèce") +
  scale_fill_manual(values = alpha(cols, 0.4)) +
  guides(fill = "none") +
  scale_x_continuous(breaks = seq_along(levels(reabs_df$shade_group)),
                     labels = noms_groupes[levels(reabs_df$shade_group)])  +
  labs(x = "Groupe",
       y = "Efficacité de réabsorption de l'azote (%)") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"))

##### Code pour comparer visuellement CRE NRE
ggplot(reabs_df, aes(x = CRE, y = NRE, color = species)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  labs(x = "Efficacité de réabsorption du carbone (%)",
       y = "Efficacité de réabsorption de l'azote (%)",
       color = "Espèce") +
  theme_classic()

#-------------------------------------------------------------------------------

#MODEL LEAVE-ONE-OUT------------------------------------------------------------
#Test la sensibilité du model linéaire a chacune des espèces en retirant chacune une a la fois   # charge lme4 + p-values


nre_df$species     <- factor(nre_df$species)
nre_df$shade_group <- factor(nre_df$shade_group)

# --- Fonction qui extrait l'effet du groupe d'un modèle ---
extraire <- function(m, etiquette) {
  s   <- summary(m, ddf = "Kenward-Roger")$coefficients
  lig <- grep("shade_group", rownames(s))          # ligne de l'effet du groupe
  data.frame(
    espece_retiree = etiquette,
    estimation     = s[lig, "Estimate"],
    se             = s[lig, "Std. Error"],
    ddl            = s[lig, "df"],
    p              = s[lig, "Pr(>|t|)"],
    singulier      = isSingular(m)
  )
}

# --- Modèle complet (référence) ---
m_full <- lmer(NRE ~ shade_group + (1 | species), data = nre_df)
res_full <- extraire(m_full, "Aucune (modèle complet)")
m_full
# --- Leave-one-species-out ---
especes <- levels(nre_df$species)

res_loo <- do.call(rbind, lapply(especes, function(sp) {
  dsub <- droplevels(subset(nre_df, species != sp))
  m_sp <- lmer(NRE ~ shade_group + (1 | species), data = dsub)
  extraire(m_sp, sp)
}))

loo <- rbind(res_full, res_loo)
loo$groupe_de_l_espece <- c(NA, as.character(
  nre_df$shade_group[match(especes, as.character(nre_df$species))]))
print(loo, digits = 3)

# Plage des estimations
range(res_loo$estimation)

#Simplifie
moy <- aggregate(NRE ~ species + shade_group, data = nre_df, FUN = mean)

loo_moy <- sapply(as.character(moy$species), function(sp) {
  x <- subset(moy, species != sp)
  diff(tapply(x$NRE, x$shade_group, mean))
})
loo_moy

# Référence : différence avec toutes les espèces
diff(tapply(moy$NRE, moy$shade_group, mean))

##GRAPHIQUE DE SENSIBILITE
ggplot(loo, aes(x = estimation, y = reorder(espece_retiree, estimation))) +
  geom_vline(xintercept = res_full$estimation, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_pointrange(aes(xmin = estimation - 1.96 * se,
                      xmax = estimation + 1.96 * se,
                      color = singulier)) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                     name = "Modèle singulier") +
  labs(x = "Effet du groupe (shade − light), en points de NRE",
       y = "Espèce retirée") +
  theme_classic()
#-------------------------------------------------------------------------------
#TESTONS DES AFFAIRES-----------------------------------------------------------

#TEST variance due a l'sp
#Ce test est fait puique la variance inter-espece semble grande après inspection visuel des données

VarCorr(m_full)                 # variance espèce vs résiduelle
performance::icc(m_full)        # part de variance due à l'espèce
confint(m_full, method = "profile")   # IC de sigma espèce

# Variance intra-espèce par espèce
aggregate(NRE ~ species, data = nre_df, FUN = sd)

# Étendue par espèce
aggregate(NRE ~ species, data = nre_df, FUN = range)
#-------------------------------------------------------------------------------
#RESULTAAAAAATS RÉSULTANTS------------------------------------------------------


# NRE par espèce (moyenne, SD, min, max, n)
desc_sp <- do.call(data.frame, aggregate(NRE ~ species + shade_group, data = nre_df,
                                         FUN = function(x) c(n = length(x), moy = mean(x), sd = sd(x), min = min(x), max = max(x))))
print(desc_sp, digits = 3)

# NRE moyenne par groupe (moyenne des moyennes d'espèces = unité de réplication)
moy <- aggregate(NRE ~ species + shade_group, data = nre_df, FUN = mean)
aggregate(NRE ~ shade_group, data = moy, FUN = function(x) c(moy = mean(x), sd = sd(x)))

range(nre_df$NRE)

#TEST DU GROUPE
library(lmerTest)
m_full <- lmer(NRE ~ shade_group + (1 | species), data = nre_df)

s   <- summary(m_full, ddf = "Kenward-Roger")$coefficients
lig <- grep("shade_group", rownames(s))
est <- s[lig, "Estimate"]; se <- s[lig, "Std. Error"]; ddl <- s[lig, "df"]

ic_kr <- est + c(-1, 1) * qt(0.975, ddl) * se   # IC 95 % avec t et non 1,96
c(estimation = est, se = se, ddl = ddl, p = s[lig, "Pr(>|t|)"],
  IC_bas = ic_kr[1], IC_haut = ic_kr[2])


## TABLEAU composante et variance

vc  <- as.data.frame(VarCorr(m_full))
ci  <- confint(m_full, method = "profile")

tab1 <- data.frame(
  composante = c("Entre espèces", "Résiduelle (entre individus)"),
  ecart_type = c(vc$sdcor[vc$grp == "species"], vc$sdcor[vc$grp == "Residual"]),
  IC_bas     = ci[c(".sig01", ".sigma"), 1],
  IC_haut    = ci[c(".sig01", ".sigma"), 2]
)
tab1$ICC_ajustee <- c(vc$vcov[1] / sum(vc$vcov), NA)
print(tab1, digits = 3)

##LEAVE ONE OUT
library(ggplot2)

# loo = table construite avec extraire() (modèle complet + 6 retraits)
loo$ic_bas  <- loo$estimation - qt(0.975, loo$ddl) * loo$se
loo$ic_haut <- loo$estimation + qt(0.975, loo$ddl) * loo$se

fig2 <- ggplot(loo, aes(x = estimation, y = reorder(espece_retiree, estimation))) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_vline(xintercept = loo$estimation[1], linetype = "dashed") +
  geom_pointrange(aes(xmin = ic_bas, xmax = ic_haut)) +
  labs(x = "Effet du groupe (tolérantes − intolérantes), en points de NRE",
       y = "Espèce retirée") +
  theme_classic()
fig2

ggsave("figure2_LOO.png", fig2, width = 7, height = 4, dpi = 300)

##CSV
write.csv(tab1, "tableau1_variance.csv", row.names = FALSE)
write.csv(loo,  "tableau_LOO.csv",       row.names = FALSE)
# Figure 1 : ajoutez ggsave("figure1.png", width = 7, height = 4, dpi = 300) après votre ggplot

#-------------------------------------------------------------------------------


model2<-lm(NRE~shade_group, data=nre_df)

summary(model2)
visreg(model2)

reabs_df <- full_df %>%
  select(tree_id, species, shade_group, ech, n, carbone) %>%
  pivot_wider(
    names_from = ech,
    values_from = c(n, carbone),
    names_prefix = "ech"
  ) %>%
  mutate(
    NRE = (1 - (n_ech2 / n_ech1)) * 100, # Réabsorption azote non corrigée
    
    NRE_corr = (1 - ((n_ech2 / carbone_ech2) /
                       (n_ech1 / carbone_ech1))) * 100, # Réabsorption azote corrigée par le carbone
    
    CRE = (1 - (carbone_ech2 / carbone_ech1)) * 100  # Changement du carbone ##pas sur
  )


model3<-lm(NRE~shade_group, data=reabs_df) #ici nre est pas corrigée
summary(model2)
visreg(model3)

model4<-lmer(NRE~shade_group+(1|species), data=reabs_df)
summary(model4)
visreg(model4)

model5<-lmer(NRE_corr~shade_group+(1|species), data=reabs_df)
summary(model5)
visreg(model5)
isSingular(model5)
