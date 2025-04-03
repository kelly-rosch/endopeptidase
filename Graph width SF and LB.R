rm(list = ls()) # clear out previous data 
dev.off()       # close all previous plots
cat("\014")     # clear console of previous output

library("ggplot2")
library("RColorBrewer")
library("MetBrewer")
library("stringr")
library("tidyr")
library("dplyr")
library("ggridges")
library("viridis")
library("hrbrthemes")

#------------------------------------------------------------

setwd("/Users/me/Documents/Doerr Lab/ShyA manuscript/R data")

load("AllClists.RData")

table(data$strain, data$medium)

paper_strains <- filter(data, strain == "Empty" | 
                          strain == "ShyA" | 
                          strain == "WT")

table(paper_strains$strain, paper_strains$medium)

# Dropping unused levels
paper_strains <- droplevels(paper_strains)
# Checking
table(paper_strains$strain, paper_strains$medium)

# Box plot for width
ggplot(paper_strains, aes(x = full_strain_name, y = widthbirth, color = full_strain_name)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(colour = "black", outlier.shape = NA, fill = NA) +
  labs(x = "Strain", y = "Cell width (μm)", title = "") + 
  theme_light() +
  ylim(0, 1.25) +
  facet_grid(cols = vars(medium)) +
  theme(strip.text.x = element_text(colour = "black", size = 13)) +
  theme(strip.background.x = element_rect(fill = "white", colour = "black")) +
  scale_color_manual(values = met.brewer("Johnson", n = 4)) +
  theme(axis.text.x = element_text(size = 13)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position = "none")


# ------------Stats for this-------------

# Splitting into LB vs SF
LB <- filter(paper_strains, medium == "LB")
SF <- filter(paper_strains, medium == "SF")


# -----------------LB first -------------------

# Selecting the factor that I want to appear on the X axis of the plot
table(LB$strain) # new variable as factor. use original in plot

# Check that strain is a factor before continuing

# This makes a linear model called "mod" looking at whether width is significantly different
# based on strain
mod <- lm(widthbirth ~ strain, data = LB)
summary(mod)
anova(mod)
# This tells us that the strains are different, but we don't know which 

# Now this step will tell us which strains are different from which
emmeans(mod, list(pairwise ~ strain))
# these are values shown in plot


# ------------------ SF -------------------------

table(SF$strain)

mod <- lm(widthbirth ~ strain, data = SF)
summary(mod)
anova(mod)
emmeans(mod, list(pairwise ~ strain))
# these are values shown in plot


# ----Calculating average width for each strain-------

# ---------LB first --------
LB %>%
  group_by(strain) %>%
  dplyr::summarize(Mean = mean(widthbirth, na.rm = TRUE))

# --------- SF --------
SF %>%
  group_by(strain) %>%
  dplyr::summarize(Mean = mean(widthbirth, na.rm = TRUE))



