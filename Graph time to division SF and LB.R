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
library("emmeans")

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


# Box plot for time to division
ggplot(paper_strains, aes(x = full_strain_name, y = age*5, color = full_strain_name)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(colour = "black", outlier.shape = NA, fill = NA) +
  labs(x = "Strain", y = "Time to division (min)", title = "") + 
  theme_light() +
  ylim(0, 160) +
  facet_grid(cols = vars(medium)) +
  theme(strip.text.x = element_text(colour = "black", size = 13)) +
  theme(strip.background.x = element_rect(fill = "white", colour = "black")) +
  scale_color_manual(values = met.brewer("Isfahan2", n = 3)) +
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
mod <- lm(age ~ strain, data = LB)
summary(mod)
anova(mod)
# This tells us that the strains are different, but we don't know which 

# Now this step will tell us which strains are different from which
emmeans(mod, list(pairwise ~ strain))
# these are values shown in plot


# ------------------ SF -------------------------

table(SF$strain)

mod <- lm(age ~ strain, data = SF)
summary(mod)
anova(mod)
emmeans(mod, list(pairwise ~ strain))
# these are values shown in plot


# ----Calculating average width for each strain-------

# ---------LB first --------
LB %>%
  group_by(strain) %>%
  dplyr::summarize(Mean = mean(age*5, na.rm = TRUE),
                   SD = sd(age*5, na.rm = TRUE))

# --------- SF --------
SF %>%
  group_by(strain) %>%
  dplyr::summarize(Mean = mean(age*5, na.rm = TRUE),
                   SD = sd(age*5, na.rm = TRUE))



# ---------- BINNED GRAPHS ----------------

#--------- SF -----------

# first bin cells by birth frame: 1-5, 6-10, 11-15, 16-20, 21-25, 26-30
SF$birthframe_bin <- with(SF, ifelse(birthframe == '1', '1-5',
                              ifelse(birthframe == '2', '1-5',
                              ifelse(birthframe == '3', '1-5',
                              ifelse(birthframe == '4', '1-5',
                              ifelse(birthframe == '5', '1-5',
                              ifelse(birthframe == '6', '6-10',
                              ifelse(birthframe == '7', '6-10',
                              ifelse(birthframe == '8', '6-10',
                              ifelse(birthframe == '9', '6-10',
                              ifelse(birthframe == '10', '6-10',
                              ifelse(birthframe == '11', '11-15',
                              ifelse(birthframe == '12', '11-15',
                              ifelse(birthframe == '13', '11-15',
                              ifelse(birthframe == '14', '11-15',
                              ifelse(birthframe == '15', '11-15',
                              ifelse(birthframe == '16', '16-20',
                              ifelse(birthframe == '17', '16-20',
                              ifelse(birthframe == '18', '16-20',
                              ifelse(birthframe == '19', '16-20',
                              ifelse(birthframe == '20', '16-20',
                              ifelse(birthframe == '21', '21-25',
                              ifelse(birthframe == '22', '21-25',
                              ifelse(birthframe == '23', '21-25',
                              ifelse(birthframe == '24', '21-25',
                              ifelse(birthframe == '25', '21-25',
                              ifelse(birthframe == '26', '26-30',
                              ifelse(birthframe == '27', '26-30',
                              ifelse(birthframe == '28', '26-30',
                              ifelse(birthframe == '29', '26-30',
                              '26-30'))))))))))))))))))))))))))))))

# ordering the bins
SF$birthframe_bin <- factor(SF$birthframe_bin,  levels = c("1-5",
                                                                   "6-10",
                                                                   "11-15",
                                                                   "16-20",
                                                                   "21-25",
                                                                   "26-30"))

# ordering the strains
SF$strain <- factor(SF$strain,  levels = c("WT", "ShyA", "Empty"))

# Density ridges with facet grid
ggplot(SF, aes(x = age*5, y = birthframe_bin, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.05, quantile_lines = TRUE, quantiles = 2) +
  facet_grid(rows = vars(strain)) +
  scale_fill_viridis_c(name = "Time to division (mins)", direction = -1) +
  labs(x = "Time to division (mins)", y = "Birth frame", title = "SF") +
  theme_light() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(strip.text.y = element_text(colour = "black", size = 13)) +
  theme(strip.background.y = element_rect(fill = "white", colour = "black")) 


# ------------- LB ----------------


# first bin cells by birth frame: 1-5, 6-10, 11-15, 16-20, 21-25, 26-30
LB$birthframe_bin <- with(LB, ifelse(birthframe == '1', '1-5',
                                     ifelse(birthframe == '2', '1-5',
                                            ifelse(birthframe == '3', '1-5',
                                                   ifelse(birthframe == '4', '1-5',
                                                          ifelse(birthframe == '5', '1-5',
                                                                 ifelse(birthframe == '6', '6-10',
                                                                        ifelse(birthframe == '7', '6-10',
                                                                               ifelse(birthframe == '8', '6-10',
                                                                                      ifelse(birthframe == '9', '6-10',
                                                                                             ifelse(birthframe == '10', '6-10',
                                                                                                    ifelse(birthframe == '11', '11-15',
                                                                                                           ifelse(birthframe == '12', '11-15',
                                                                                                                  ifelse(birthframe == '13', '11-15',
                                                                                                                         ifelse(birthframe == '14', '11-15',
                                                                                                                                ifelse(birthframe == '15', '11-15',
                                                                                                                                       ifelse(birthframe == '16', '16-20',
                                                                                                                                              ifelse(birthframe == '17', '16-20',
                                                                                                                                                     ifelse(birthframe == '18', '16-20',
                                                                                                                                                            ifelse(birthframe == '19', '16-20',
                                                                                                                                                                   ifelse(birthframe == '20', '16-20',
                                                                                                                                                                          ifelse(birthframe == '21', '21-25',
                                                                                                                                                                                 ifelse(birthframe == '22', '21-25',
                                                                                                                                                                                        ifelse(birthframe == '23', '21-25',
                                                                                                                                                                                               ifelse(birthframe == '24', '21-25',
                                                                                                                                                                                                      ifelse(birthframe == '25', '21-25',
                                                                                                                                                                                                             ifelse(birthframe == '26', '26-30',
                                                                                                                                                                                                                    ifelse(birthframe == '27', '26-30',
                                                                                                                                                                                                                           ifelse(birthframe == '28', '26-30',
                                                                                                                                                                                                                                  ifelse(birthframe == '29', '26-30',
                                                                                                                                                                                                                                         '26-30'))))))))))))))))))))))))))))))

# ordering the bins
LB$birthframe_bin <- factor(LB$birthframe_bin,  levels = c("1-5",
                                                           "6-10",
                                                           "11-15",
                                                           "16-20",
                                                           "21-25",
                                                           "26-30"))

# ordering the strains
LB$strain <- factor(LB$strain,  levels = c("WT", "ShyA", "Empty"))

# Density ridges with facet grid
ggplot(LB, aes(x = age*5, y = birthframe_bin, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.05, quantile_lines = TRUE, quantiles = 2) +
  facet_grid(rows = vars(strain)) +
  scale_fill_viridis_c(name = "Time to division (mins)", direction = -1) +
  labs(x = "Time to division (mins)", y = "Birth frame", title = "LB") +
  theme_light() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(strip.text.y = element_text(colour = "black", size = 13)) +
  theme(strip.background.y = element_rect(fill = "white", colour = "black")) 






#------ If I want to graph them separately instead:------

# separating by strain so I can graph separately
N16961_SF <- SF[SF$strain=="WT",]
ShyA_SF <- SF[SF$strain=="ShyA",]
G81A_SF <- SF[SF$strain=="G81A",]
I87A_SF <- SF[SF$strain=="I87A",]
Empty_SF <- SF[SF$strain=="Empty",]

# graphing bins for each strain
#------E103A--------
ggplot(N16961_SF, aes(x = age*5, y = birthframe_bin, fill = after_stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Time to division (mins)", option = "C") +
  labs(x = "Time to division (mins)", y = "Birth frame", title = "ΔshyA pFIS-shyA E103A") +
  theme_light() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 16))
