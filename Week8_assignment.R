install.packages('raster')
library(raster)
install.packages('geodata')
library(geodata)
library(tidyverse)
library(broom)

clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

clim$altitude = as.numeric(gsub(',', '', clim$altitude))
clim$p_mean = as.numeric(gsub(',', '', clim$p_mean))

climfrar <- clim[1:34,]

mdl <- lm(t_mean ~ altitude + lat + lon, data = climfrar)
mdl
# 
# Call:
#   lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)
# 
# Coefficients:
#   (Intercept)     altitude          lat          lon  
# 37.265036    -0.006414    -0.533960     0.032101 

mdl$coefficients

# Mean Annual Temperature = mdl$coefficients[1] + mdl$coefficients[2]*altitude + mdl$coefficients[3]*lat + mdl$coefficients[4]*lon
summary(mdl)

# Interpretation
# The intercept is 37.27 when all the variables such as altitude, latitude and longitude are zero. For every one unit increase in altitude and latitude,
# the intercept decreases by 0.006 and 0.533 respectively. Also for every one unit increase in longitude the intercept increases by 0.032
# The altitude and latitude are significant but the longitude is insignificant with a p-value of 0.424. 
# The multi-linear regression model shows that the mean annual temperature is 37.3 degrees when the altitude, latitude, and longitude are kept at zero. However, the mean temperature changes based on the values of the predictors. The result shows that there is a negative relationship between altitude and latitude with respect to the mean temperature. Likewise, there is a non-significant positive relationship between longitude and temperature. For every one unit increase in altitude and latitude, the mean temperature is expected to fall by 0.0064 and 0.534 respectively. 
# The model showcased that altitude and latitude significantly influences the mean temperature with p-values < .05 while longitude does not. Furthermore, the model has an overall fit with an R2  of  83% which explains a substantial proportion of the variance in the mean temperature.


# Question 2

mdl2 <- lm(t_mean ~ altitude + lat, data = climfrar)
mdl2
# Call:
#   lm(formula = t_mean ~ altitude + lat, data = climfrar)
# 
# Coefficients:
#   (Intercept)     altitude          lat  
# 37.914757    -0.006264    -0.546532  

mdl2$coefficients

#Equation
#Mean Annual Temperature = mdl2$coefficients[1] + mdl2$coefficients[2]*altitude + mdl2$coefficients[3]*lat

summary(mdl2)

#Predictions

new_data <- clim[35:36,1:3]

predictions <- predict(mdl2, new_data, data= new_data, interval = 'p', level =0.95)

#Interpretation
#

# The estimated value for Mont-Ventoux is 6.19 within the 95% confidence interval, which ranges from 3.81 to 8.56. Based on this confidence level, it is quite certain that the true value will fall between the confidence interval. However, I noticed that the actual value is 3.6 which is below the lower bound of the confidence interval (3.81). Hence, the estimated range from the analysis did not capture the actual value.
# Similarly, the estimated value is 3.46 which falls within the 95% confidence interval, which ranges from -8.36 to 1.44. This indicates that, based on the analysis, there is confidence that the true value of the mean temperature would be somewhere in this interval. The actual value is -1.2 which falls within the predicted 95% confidence interval. Therefore, the model captures the actual value for Pic-du-Midi.
# Confidence interval is an indication that, given the uncertainty in the estimation process, the observed value is in line with the model's predictions for this particular instance.



Question 3
scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(mdl2)

The result of model 2 is very similar to model1. 
# There is a slight increase in the mean annual temperature at baseline to 37.91 degrees when altitude and latitude are held at zero. 
# The negative relationship between both altitude and latitude with temperature still holds. 
# For every one unit increase in altitude and latitude, there is a decrease in mean temperature by 0.0063 and 0.547 respectively. 
# The model also accounts for 83% variance in the outcome variable.



G1 <- raster::getData(country = "France", level = 1)
library(ggplot2)
ggplot() +
  geom_polygon(
    data = G1,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()

library(scatterplot3d)
