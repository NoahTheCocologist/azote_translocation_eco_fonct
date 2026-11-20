# Load Packages ----
source('loadpackages.R')

# Read csv files ----
files <- c("carbone", "azote","shade_group")
source('readdatafiles.R')
readdatafiles("data/", files,"_df", ".csv")

# Creating data frames ----
small_df<-azote_df %>%
  left_join(carbone_df, by='tree_id') #joining carbone data with azote

mid_df <- small_df %>% #separatn tree_id info into sep columns for
  mutate(
    ech = as.numeric(str_extract(tree_id, "^[^_]+")),       #sample
    species = str_extract(tree_id, "(?<=_)[A-Za-z]+"),      #esp
    individual = as.numeric(str_extract(tree_id, "\\d+$")), #individual identifiant
    tree_id = str_remove(tree_id, "^[^_]+_")                #keeping indivual tree_id (just removing sample nb)
  )

full_df<-mid_df %>%
  left_join(shade_group_df, by='species')

nc_df<-full_df %>%
  mutate(
    nc= full_df$n/full_df$carbone
  )

#variables
hist(nc_df$nc)
plot(shade_group,nc,data=nc_df)
ggplot(nc_df, aes(shade_group, nc))
?ggplot

model1<-lmer(nc ~ ech * shade_group + (1|tree_id), 
             data=nc_df)
summary(model1)
visreg(model1)
