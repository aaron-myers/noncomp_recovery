
results$bias=results$mean-results$true
results$bias2=results$bias^2
results$abs_bias=abs(results$bias)
results$abs_relbias=abs(results$bias)/results$tru

item_parms=results[(N*3+1):nrow(results),]
