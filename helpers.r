
calc_rmse = function(model, tst_data){
  sqrt(mean((predict(model, tst_data) - tst_data$total_UPDRS) ^ 2))
}

calc_mae = function(model, tst_data){
  mean(abs(predict(model, tst_data) - tst_data$total_UPDRS))
}

get_bp = function(model) {
  unname(bptest(model)$p.value)
}

get_sw = function(model) {
  unname(shapiro.test(resid(model))$p.value)
}

get_num_params = function(model) {
  length(coef(model))
}

get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

get_adj_r2 = function(model) {
  summary(model)$adj.r.squared
}

eval_model = function(model, tst_data){
  list(b_pagan = get_bp(model),
       shap_wilk = get_sw(model),
       rmse_loocv = get_loocv_rmse(model),
       rmse_trn = sqrt(mean(model$residuals^2)),
       rmse_tst = calc_rmse(model, tst_data),
       adj_r2 = get_adj_r2(model),
       num_p = get_num_params(model),
       mae_tst = calc_mae(model, tst_data))
}

diagnostic_plots = function(model, pcol="grey", lcol="dodgerblue"){
  par(mfrow = c(1,2))
  plot(fitted(model), resid(model), col = pcol, 
       xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
  abline(h=0, col = lcol)
  qqnorm(resid(model), col = pcol, main = "Normal Q-Q plot")
  qqline(resid(model), col = lcol)
}

non_influential_filter = function(model){
  cooks.distance(model) <= (4 / length(cooks.distance(model)))
}

plot_pva = function(predicted, actual){
  plot(actual, predicted, 
     col = "darkgrey",
     xlab = "Actual",
     ylab = "Predicted",
     main = "Predicted vs Actual")
  grid()
  abline(0, 1, col = "dodgerblue")
}
