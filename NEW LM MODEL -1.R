 
library(stats)

 
glm_model_aar <- glm(aar ~ macsidnumber + Precipitation + Temperature, family = gaussian(link = "identity"), data = data)
glm_model_eeaa <- glm(eeaa ~ macsidnumber + Precipitation + Temperature, family = gaussian(link = "identity"), data = data)
glm_model_pea <- glm(peaa ~ macsidnumber + Precipitation + Temperature, family = gaussian(link = "identity"), data = data)
glm_model_geaa <- glm(geaa ~ macsidnumber + Precipitation + Temperature, family = gaussian(link = "identity"), data = data)
glm_model_dnamtladjage <- glm(dnamtladjage ~ macsidnumber + Precipitation + Temperature, family = gaussian(link = "identity"), data = data)

 
summary(glm_model_aar)
summary(glm_model_eeaa)
summary(glm_model_pea)
summary(glm_model_geaa)
summary(glm_model_dnamtladjage)

 
par(mfrow = c(3, 2))   
plot(glm_model_aar$fitted.values, glm_model_aar$residuals, main = "Residuals vs Fitted for aar", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
plot(glm_model_eeaa$fitted.values, glm_model_eeaa$residuals, main = "Residuals vs Fitted for eeaa", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
plot(glm_model_pea$fitted.values, glm_model_pea$residuals, main = "Residuals vs Fitted for peaa", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
plot(glm_model_geaa$fitted.values, glm_model_geaa$residuals, main = "Residuals vs Fitted for geaa", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")
plot(glm_model_dnamtladjage$fitted.values, glm_model_dnamtladjage$residuals, main = "Residuals vs Fitted for dnamtladjage", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

 
new_data <- data.frame(macsidnumber = seq(min(data$macsidnumber), max(data$macsidnumber), length.out = 100),
                       Precipitation = mean(data$Precipitation),
                       Temperature = mean(data$Temperature))

 
predictions_aar <- predict(glm_model_aar, newdata = new_data, type = "response")
predictions_eeaa <- predict(glm_model_eeaa, newdata = new_data, type = "response")
predictions_pea <- predict(glm_model_pea, newdata = new_data, type = "response")
predictions_geaa <- predict(glm_model_geaa, newdata = new_data, type = "response")
predictions_dnamtladjage <- predict(glm_model_dnamtladjage, newdata = new_data, type = "response")

 
print(predictions_aar)
print(predictions_eeaa)
print(predictions_pea)
print(predictions_geaa)
print(predictions_dnamtladjage)
