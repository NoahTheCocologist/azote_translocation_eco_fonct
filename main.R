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
