# Matthew J. Keefe
# July 27, 2020
# TidyTuesday Coffee Data - Week of 2020-07-07


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(GGally)
library(FactoMineR)
library(reshape2)
library(RCurl)
library(png)
library(gridExtra)
library(grid)

# Helper Functions --------------------------------------------------------
mypng <- function(x
                  , type='cairo-png'
                  , width=2.2*(40/3)/2
                  , height=2.2*(7.5)/2
                  , units='in'
                  , res=300){
  png(x
      , type='cairo-png'
      , width=width
      , height=height
      , units=units
      , res=res)
}

# Load Data ---------------------------------------------------------------
tt_dat <- tidytuesdayR::tt_load('2020-07-07')
coffee_dat <- tt_dat$coffee_ratings


# Explore and Clean Data ------------------------------------------------------------

skimr::skim(coffee_dat)


cleanup_recipe <- recipe(total_cup_points~., data = coffee_dat) %>% 
  step_unknown(all_nominal(), new_level = "missing") %>% 
  step_filter(total_cup_points > 0)

coffee_dat_clean <- cleanup_recipe %>%
  prep() %>% 
  juice()

#total cup points by country
coffee_dat_clean %>% 
  ggplot(aes(x=fct_reorder(country_of_origin, -total_cup_points, .fun = median), y = total_cup_points)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#sweetness by country
coffee_dat_clean %>% 
  ggplot(aes(x=fct_reorder(country_of_origin, -sweetness, .fun = median), y = sweetness)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#aroma by country
coffee_dat_clean %>% 
  ggplot(aes(x=fct_reorder(country_of_origin, -aroma, .fun = median), y = aroma)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#flavor by country
coffee_dat_clean %>% 
  ggplot(aes(x=fct_reorder(country_of_origin, -flavor, .fun = median), y = flavor)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#acidity by country
coffee_dat_clean %>% 
  ggplot(aes(x=fct_reorder(country_of_origin, -acidity, .fun = median), y = acidity)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

coffee_dat_clean %>% 
  filter(altitude_mean_meters < 9000) %>% 
  ggplot(aes(x=altitude_mean_meters, y = total_cup_points)) +
  geom_point() +
  geom_smooth(method='lm')


# Are components of score correlated? -------------------------------------


coffee_dat_clean %>% 
  select(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness, cupper_points, total_cup_points) %>% 
  ggpairs()


# Principal Components Analysis -------------------------------------------
X.train <- coffee_dat_clean %>% 
  select(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness, cupper_points)
  
pca.model <- PCA(X = X.train, ncp=7, scale.unit = TRUE,graph = FALSE)
pca.model.data <- pca.model$eig %>%
  as_tibble(validate=TRUE) %>%
  mutate(component_number = as.numeric(gsub("[^\\d]+", "", row.names(pca.model$eig), perl=TRUE)))
names(pca.model.data)[2:3] <- c('variance_pct', 'cumulative_variance_pct')

#scree plot of variance explained by number of components
coffee_img <- rasterGrob(readPNG(getURLContent('https://creazilla-store.fra1.digitaloceanspaces.com/cliparts/22711/cup-of-coffee-clipart-md.png')))

p1<-ggplot(data = pca.model.data %>% filter(component_number<=100)) +
  geom_point(aes(x=component_number, y = cumulative_variance_pct), color = '#66462F', size = 4)+
  geom_line(aes(x=component_number, y = cumulative_variance_pct), color = '#66462F', lwd = 1.2) +
  annotation_custom(coffee_img, xmin=7, xmax=10, ymin=42, ymax=72) +
  scale_x_continuous(breaks = seq(0,10,1))+
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(x = 'number of principal components',
       y ='cumulative variance explained',
       title = 'Percentage variance explained by number of principal components') +
  theme(plot.title.position = 'plot',
        panel.background = element_rect(fill = '#F5EDDA'),
        plot.title = element_text(color = '#66462F'),
        axis.text = element_text(size = 14, color = '#66462F'),
        axis.title = element_text(size = 16, color = '#66462F'),
        panel.grid.minor = element_blank())

loadings.train <- pca.model$var$coord %>% as.data.frame()

coords.train <- pca.model$ind$coord %>% as_data_frame()

loadingsdat<-pca.model$var$coord %>% as.data.frame() %>% 
  as.matrix() %>% 
  melt() %>% 
  mutate(dim_num = as.numeric(gsub("[^\\d]+", "", Var2, perl=TRUE))) %>%
  mutate(abs_loading = abs(value)) %>%
  mutate(varname = as.character(Var1)) %>%
  group_by(dim_num) %>%
  top_n(n = 20, wt = abs_loading) %>%
  filter(dim_num <=10) %>%
  ungroup() %>% 
  group_by(dim_num) %>% 
  arrange(dim_num, -abs_loading) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()

p2<-ggplot(data = loadingsdat ,aes(x = as.factor(dim_num), y = varname, fill = abs_loading)) + 
  geom_tile()+
  # geom_text(aes(label=rank)) +
  ggtitle("Principal component absolute loadings") +
  xlab("principal component number")+
  ylab("") +
  labs(fill = '') +
  scale_fill_gradient(low = "#F5EDDA", high = "#66462F", limits=c(0,1), breaks = seq(0,1,.2)) +
  theme(plot.title.position = 'plot',
        panel.background = element_rect(fill = '#F5EDDA'),
        plot.title = element_text(color = '#66462F'),
        axis.text = element_text(size = 14, color = '#66462F'),
        axis.title = element_text(size = 16, color = '#66462F'),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size=14))

mypng('~/Repositories/personal/tidytuesday/2020/2020-07-07/coffee_pca.png')
grid.arrange(p1,p2, 
             ncol=2,
             top = textGrob("Principal Component Analysis of Coffee Scoring\n", gp=gpar(fontsize=26,font=3, col = '#66462F')))
dev.off()
