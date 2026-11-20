# Load Packages ----
source('loadpackages.R')

# Read csv files ----
files <- c("carbone", "azote","shade_group")
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

model4<-lmer(NRE~shade_group+(1|species), data=reabs_df)
summary(model4)

model5<-lmer(NRE_corr~shade_group+(1|species), data=reabs_df)
summary(model5)
