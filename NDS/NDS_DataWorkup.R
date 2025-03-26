####################
#NDS Data Analysis 
####################

# setting up the library to have pacakges ready to go
library(ggplot2)
library(dplyr)
library(car)

# Set the working directory
setwd("~/Dropbox/SWEL/CopperRiverDelta/Elodea/NDS_Data")

NDS_DataAnalysis <- read.csv("NDS_DataAnalysis.csv", header = TRUE)
attach(NDS_DataAnalysis)

# filter out NAs
data <- NDS_DataAnalysis %>% 
  filter(Top != "NA")

# first to set up the data for the "top = glass" data and the "top = sponge" data
glass <- data %>%
  filter(Top == "glass")

sponge <- data %>%
  filter(Top == "sponge")

###############################
#starting with glass top data
###############################

N_values <- glass %>% group_by(Nutrient) %>% summarise(N = n())
print(N_values)


# statistics for NEP with glass tops

mod1 <- aov(NEP ~ Nutrient, data = glass)
summary(mod1)

N_mod1<- nrow(mod1$model)

# because the factors were significant, we looking into the nutrient effect more carefully

TukeyHSD(mod1, which = "Nutrient")

# statistics for NEP with glass tops

mod2 <- aov(CR ~ Nutrient, data = glass)
summary(mod2)

N_mod2 <- nrow(mod2$model)

# because the factors were significant, we looking into the nutrient effect more carefully

TukeyHSD(mod2, which = "Nutrient")


# statistics for NEP with glass tops

mod3 <- aov(GPP ~ Nutrient, data = glass)
summary(mod3)

N_mod3 <- nrow(mod3$model)

# because the factors were significant, we looking into the nutrient effect more carefully

TukeyHSD(mod3, which = "Nutrient")

###############################
#starting with sponge top data
###############################

N_values <- sponge %>% group_by(Nutrient) %>% summarise(N = n())
print(N_values)

# statistics for NEP with glass tops

mod4 <- aov(NEP ~ Nutrient, data = sponge)
summary(mod4)

N_mod4 <- nrow(mod4$model)

# because the factors were significant, we looking into the nutrient effect more carefully

TukeyHSD(mod4, which = "Nutrient")

# statistics for CR with sponge tops

mod5 <- aov(CR ~ Nutrient, data = sponge)
summary(mod5)

N_mod5 <- nrow(mod5$model)

# because the factors were significant, we looking into the nutrient effect more carefully

TukeyHSD(mod5, which = "Nutrient")

# statistics for GPP with glass tops

mod6 <- aov(GPP ~ Nutrient, data = sponge)
summary(mod6)

N_mod6 <- nrow(mod6$model)

# because the factors were significant, we looking into the nutrient effect more carefully

TukeyHSD(mod6, which = "Nutrient")







