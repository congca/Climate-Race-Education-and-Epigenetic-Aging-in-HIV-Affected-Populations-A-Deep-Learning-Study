 
library(dplyr)
library(car)
library(broom)

 
 
dependent_vars <- c("aar", "eeaa", "peaa", "geaa", "dnamtladjage")

 
model_results <- data.frame()

 
for (dep_var in dependent_vars) {
 
  data$macsidnumber <- as.factor(data$macsidnumber)
 
  formula <- as.formula(paste(dep_var, "~ macsidnumber * (Precipitation + Temperature + white + educbas)"))
  
  model <- lm(formula, data = data)
  
 
  vif_values <- vif(model, type = 'predictor')
  
 
  model_summary <- summary(model)
  
 
  independent_vars <- all.vars(formula)[-1]   
  independent_vars_str <- paste(independent_vars, collapse = ", ")   
  
 
  result <- tibble(
    dependent_variable = dep_var,   
    independent_variables = independent_vars_str,   
    coefficients = list(model_summary$coefficients),   
    r_squared = model_summary$r.squared, 
    vif = list(vif_values)   
  )
  
  model_results <- bind_rows(model_results, result)
}

 
print(model_results)
R.version.string
