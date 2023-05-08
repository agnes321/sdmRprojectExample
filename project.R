library(dismo)
library(dplyr)
library(ggplot2)
hooded_warb_data = read.csv("hooded_warb_locations.csv")
env_data_current = stack("env_current.grd")
env_data_forecast = stack("env_forecast.grd")
plot(env_data_current$tmin)
plot(env_data_current$precip)
hooded_warb_locations = select(hooded_warb_data, lon, lat)
hooded_warb_env = extract(env_data_current, hooded_warb_locations)
hooded_warb_data = cbind(hooded_warb_data, hooded_warb_env)

ggplot(hooded_warb_data, 
       mapping = aes(x = tmin, y = precip, color = present)) + 
  geom_point()

logistic_regr_model = glm(present ~ tmin + precip,
                          family = binomial(link = "logit"),
                          data = hooded_warb_data)
summary(logistic_regr_model)

precence_data = filter(hooded_warb_data, present == 1)
absence_data = filter(hooded_warb_data, present == 0)
evaluation = evaluate(precence_data, absence_data, logistic_regr_model)
plot(evaluation, 'ROC')

predictions = predict(env_data_current,
                      logistic_regr_model,
                      type = "response")
plot(predictions, ext = extent(-140, -50, 25, 60))
points(precence_data[c("lon", "lat")], pch = "+", cex = 0.5)

plot(predictions > 0.5, ext = extent(-140, -50, 25, 60))

tr = threshold(evaluation, stat = "prevalence")

plot(predictions > tr, ext = extent(-140, -50, 25, 60))
points(precence_data[c("lon", "lat")], pch = "+", cex = 0.5)

forecasts = predict(env_data_forecast, logistic_regr_model,
                    type = "response")
plot(forecasts, ext = extent(-140, -50, 25, 60))
plot(forecasts > tr, ext = extent(-140, -50, 25, 60))
plot(forecasts - predictions, ext = extent(-140, -50, 25, 60))
