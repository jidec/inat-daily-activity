library(splines)
library(lme4)
library(sjPlot)

sscs <- readRDS("sscs.rds")
sscs4 <- readRDS("sscs4.rds")
sscs$midday_dist <- abs(14 - sscs$median_hr_bs)
sscs4$midday_dist <- abs(14 - sscs4$median_hr_bs)
sscs <- mergeLeptraitsToSSCs(sscs)

# 8 season intervals, median_hr_bs
model <- lmer(median_hr_bs ~ ns(temp,2) + wingspan + open_closed + (1|species), data = sscs)
plot_model(model)
summary(model)

# 8 season intervals, midday_dist
model <- lmer(midday_dist ~ ns(temp,2) + wingspan + open_closed + (1|species), data = sscs)
plot_model(model)
summary(model)

# 4 season intervals, median_hr_bs
model <- lmer(median_hr_bs ~ ns(temp,2) + wingspan + open_closed + season + (1|species), data = sscs4)
plot_model(model)
summary(model)

# 4 season intervals, midday_dist
model <- lmer(midday_dist ~ ns(temp,2) + wingspan + open_closed + (1|species), data = sscs4)
plot_model(model)
summary(model)

library(car)
library(performance)
vif(model)
r2_nakagawa(model)
# could expect larger butterflies in the afternoon because they can dissapate heat easier?
