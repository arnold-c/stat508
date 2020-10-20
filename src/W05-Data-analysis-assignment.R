# Set Up ------------------------------------------------------------------
library(tidyverse)
library(ggfortify)
library(cowplot)
library(factoextra)
library(here)

RNGkind(sample.kind = "Rounding")
set.seed(1)


# Data pre-processing -----------------------------------------------------

SR <- read.table(
    here("data", "ABBREV.txt"), 
    header = F, 
    row.names = 1,
    sep = "^", 
    quote = "~"
)

SR <- na.omit(SR) # remove rows with missing values
SR <- SR[row.names(SR) != "13352", ] # remove "duplicate" entry
row.names(SR) <- SR[, 1] # set more meaningful row names
SR <- SR[, -1]

names(SR) <- c(
  "Water_(g)",
  "Energ_Kcal",
  "Protein_(g)",
  "Lipid_Tot_(g)",
  "Ash_(g)",
  "Carbohydrt_(g)",
  "Fiber_TD_(g)",
  "Sugar_Tot_(g)",
  "Calcium_(mg)",
  "Iron_(mg)",
  "Magnesium_(mg)",
  "Phosphorus_(mg)",
  "Potassium_(mg)",
  "Sodium_(mg)",
  "Zinc_(mg)",
  "Copper_(mg)",
  "Manganese_(mg)",
  "Selenium_(µg)",
  "Vit_C_(mg)",
  "Thiamin_(mg)",
  "Riboflavin_(mg)",
  "Niacin_(mg)",
  "Panto_Acid_(mg)",
  "Vit_B6_(mg)",
  "Folate_Tot_(µg)",
  "Folic_Acid_(µg)",
  "Food_Folate_(µg)",
  "Folate_DFE_(µg)",
  "Choline_Tot_(mg)",
  "Vit_B12_(µg)",
  "Vit_A_IU",
  "Vit_A_RAE",
  "Retinol_(µg)",
  "Alpha_Carot_(µg)",
  "Beta_Carot_(µg)",
  "Beta_Crypt_(µg)",
  "Lycopene_(µg)",
  "Lut+Zea_(µg)",
  "Vit_E_(mg)",
  "Vit_D_µg",
  "Vit_D_IU",
  "Vit_K_(µg)",
  "FA_Sat_(g)",
  "FA_Mono_(g)",
  "FA_Poly_(g)",
  "Cholestrl_(mg)",
  "GmWt_1",
  "GmWt_Desc1",
  "GmWt_2",
  "GmWt_Desc2",
  "Refuse_Pct"
)
SRp <- SR[, c(1:46)] # restrict to just the nutrient variables


# Data exploration --------------------------------------------------------

sort(apply(SRp, 2, mean))

sort(apply(SRp, 2, var))

# vastly different means and variances

# PCA ---------------------------------------------------------------------

## Summary stats
pr.out <- prcomp(SRp, scale = TRUE)

sort(pr.out$center)
sort(pr.out$scale)
round(pr.out$rotation, 3)

dim(pr.out$x)

### Scree plot
pr.var <- pr.out$sdev^2
pr.var

pve <- pr.var / sum(pr.var)
pve

plot(pve, 
     main = "Scree plot of Eigenvalues",
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 0.2), 
     type = "b")

plot(cumsum(pve), 
     main = "Cumulative proportion of variance explained",
     xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), 
     type = "b")

round(pve, 3)
cumsum(pve) # first 8 components explain 60% variance


## Biplots 

### Base PC2 vs PC1
biplot(pr.out, scale = 0)


### GGfortify PC2 vs PC1
autoplot(pr.out,
  x = 1, y = 2, data = SRp, color = "gray30", fill = "cyan", shape = 23,
  loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3,
  loadings.colour = "black", loadings.label.colour = "black",
  loadings.label.repel = TRUE
) +
  labs(title = "Biplot of PC2 vs PC1")

### Select variables PC2 vs PC1
Categories <- c(
  rep.int("Proximates", 1), rep.int("Energy", 1), rep.int("Proximates", 6),
  rep.int("Minerals", 10), rep.int("Vitamins", 24), rep.int("Lipids", 4)
)
new.contrib.cat <- data.frame(cbind(pr.out$rotation, Categories))

fviz_pca_var(pr.out, labels = FALSE, habillage = new.contrib.cat$Categories)

### Individual PC2 vs PC1
Water <- fviz_pca_ind(pr.out, # Most important variable for PC1
  label = "none", col.ind = SRp$`Water_(g)`,
  title = "Water"
) 

Protein <- fviz_pca_ind(pr.out, # Most important variable for PC2
  label = "none", col.ind = SRp$`Protein_(g)`,
  title = "Protein"
) 

Energy <- fviz_pca_ind(pr.out, # Most important variable for PC3
                       label = "none", col.ind = SRp$Energ_Kcal,
                       title = "Energy"
)

Lipid <- fviz_pca_ind(pr.out, # Most important variable for PC4
                      label = "none", col.ind = SRp$`Lipid_Tot_(g)`,
                      title = "Total Lipids"
)

Ash <- fviz_pca_ind(pr.out, # Most important variable for PC5
  label = "none", col.ind = SRp$`Ash_(g)`,
  title = "Ash"
)

Copper <- fviz_pca_ind(pr.out, # Most important variable for PC6
  label = "none", col.ind = SRp$`Copper_(mg)`,
  title = "Copper"
)

Sodium <- fviz_pca_ind(pr.out, # Most important variable for PC7
                       label = "none", col.ind = SRp$`Sodium_(mg)`,
                       title = "Sodium"
)

Sugar <- fviz_pca_ind(pr.out, # Most important variable for PC8
                       label = "none", col.ind = SRp$`Sugar_Tot_(g)`,
                       title = "Sugar"
)

Individual.plot <- plot_grid(
    Water, Protein, Energy, Lipid, Ash, Copper, Sodium, Sugar, align = "h"
    )


title <- ggdraw() + draw_label("Individiual Plots- First and Second Component", x = 0, hjust = 0, fontfamily = "serif", size = 16) +
  theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(title, Individual.plot, ncol = 1, rel_heights = c(0.1, 1))

### Principal component scores
Cols <- function(vec) {
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1, 2))

plot(pr.out$x[, 1:2],
     col = Cols(names(SRp)), pch = 19,
     main = "Projection of nutrients over principal \ncomponent scores for PC1 to PC3",
     xlab = "Z1", ylab = "Z2"
)

plot(pr.out$x[, c(1, 3)],
     col = Cols(names(SRp)), pch = 19,
     xlab = "Z1", ylab = "Z3"
)

