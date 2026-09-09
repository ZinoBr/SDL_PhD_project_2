

# Print parameter plots in two columns
par(mfrow=c(1,2))

numb <- data.frame(seq(1,9, 2), seq(2,10, 2))

for (i in 1:5){
  

png(
  filename = paste0("lasso_mvs_default_lambdaplots_", i, ".png"), 
  width = 2000,                  # Width in pixels (default)
  height = 600,                 # Height in pixels (default)
  res = 200                     # Resolution in PPI
)
  
j <- numb[i,1]
k <- numb[i,2] 
  
par(mfrow=c(1,2))
plot(res_mvs_default[[j]]$model$`Level 2`$models[[1]] , sign.lambda = 1)
plot(res_mvs_default[[k]]$model$`Level 2`$models[[1]] , sign.lambda = 1)

dev.off()

}
