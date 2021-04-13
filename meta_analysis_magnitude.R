# Meta-analysis of magnitude

# load packages
library(metafor)
library(orchaRd)
# Function for caltulating sampling variance of magnitude    
folded_error <-function(mean, variance){
  mu <- mean
  sigma <- sqrt(variance)
  fold_mu <- sigma*sqrt(2/pi)*exp((-mu^2)/(2*sigma^2)) + mu*(1 - 2*pnorm(-mu/sigma))
  fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
  # adding se to make bigger mean
  fold_v <-fold_se^2
  fold_v
}


#load data for SMD, adjusted SMD (sampling variance and effective sample size, respectively)
load(here("data","model_est_SMD.RData"))
load(here("data","model_est_adjusted_SMD.RData"))
load(here("data","model_est_adjusted_SMD.esz.RData"))

# SMD
## absoulte value of mean
model_est_SMD$abs_mean <- abs(model_est_SMD$mu_SMD)
## folded sampling variance
### get orignal sampling variance first
model_est_SMD$variance <- (model_est_SMD$SE_SMD)^2
### get folded sampling variance then
model_est_SMD$folded_v <- folded_error(model_est_SMD$mu_SMD, model_est_SMD$variance)

## meta-meta-analysis
MMA_magnitude_SMD <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_SMD) 
# or rma(yi = abs_mean, sei = sqrt(folded_v), test = "t", method = "REML", data = model_est_SMD) 

# adjusted_SMD based on sampling variance
## absoulte value of mean
model_est_adjusted_SMD$abs_mean <- abs(model_est_adjusted_SMD$mu_SMD)
## folded sampling variance
### get orignal sampling variance first
model_est_adjusted_SMD$variance <- (model_est_adjusted_SMD$SE_SMD)^2
### get folded sampling variance then
model_est_adjusted_SMD$folded_v <- folded_error(model_est_adjusted_SMD$mu_SMD, model_est_adjusted_SMD$variance)

## meta-meta-analysis
MMA_magnitude_adjusted_SMD <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_adjusted_SMD) 




# adjusted_SMD.esz based on effective sample size
## absoulte value of mean
model_est_adjusted_SMD.esz$abs_mean <- abs(model_est_adjusted_SMD.esz$mu_SMD)
## folded sampling variance
### get orignal sampling variance first
model_est_adjusted_SMD.esz$variance <- (model_est_adjusted_SMD.esz$SE_SMD)^2
### get folded sampling variance then
model_est_adjusted_SMD.esz$folded_v <- folded_error(model_est_adjusted_SMD.esz$mu_SMD, model_est_adjusted_SMD.esz$variance)

## meta-meta-analysis
MMA_magnitude_adjusted_SMD.esz <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_adjusted_SMD.esz) 



# if you are interested in other effect size, see following
load(here("data","model_est_RR.RData"))
load(here("data","model_est_adjusted_RR.RData"))
load(here("data","model_est_lnrr.RData"))
load(here("data","model_est_adjusted_lnrr.RData"))
load(here("data","model_est_SMDH.RData"))
load(here("data","model_est_adjusted_SMDH.RData"))
load(here("data","model_est_VR.RData"))
load(here("data","model_est_CVR.RData"))



# lnRR*
## absoulte value of mean
model_est_RR$abs_mean <- abs(model_est_RR$mu_RR)
## folded sampling variance
### get orignal sampling variance first
model_est_RR$variance <- (model_est_RR$SE_RR)^2
model_est_RR$folded_v <- folded_error(model_est_RR$mu_RR, model_est_RR$variance)

## meta-meta-analysis
MMA_magnitude_RR <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_RR) # or rma.uni(yi = abs_mean, sei = sqrt(folded_v), test = "t", method = "REML", data = model_est_RR)


# adjusted_lnRR*
## absoulte value of mean
model_est_adjusted_RR$abs_mean <- abs(model_est_adjusted_RR$mu_RR)
## folded sampling variance
### get orignal sampling variance first
model_est_adjusted_RR$variance <- (model_est_adjusted_RR$SE_RR)^2
model_est_adjusted_RR$folded_v <- folded_error(model_est_adjusted_RR$mu_RR, model_est_adjusted_RR$variance)

## meta-meta-analysis
MMA_magnitude_adjusted_RR <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_adjusted_RR) 


# lnrr
## absoulte value of mean
model_est_lnrr$abs_mean <- abs(model_est_lnrr$mu_lnrr)
## folded sampling variance
### get orignal sampling variance first
model_est_lnrr$variance <- (model_est_lnrr$SE_lnrr)^2
model_est_lnrr$folded_v <- folded_error(model_est_lnrr$mu_lnrr, model_est_lnrr$variance)

## meta-meta-analysis
MMA_magnitude_lnrr <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_lnrr) 


# adjusted_lnrr
## absoulte value of mean
model_est_adjusted_lnrr$abs_mean <- abs(model_est_adjusted_lnrr$mu_lnrr)
## folded sampling variance
### get orignal sampling variance first
model_est_adjusted_lnrr$variance <- (model_est_adjusted_lnrr$SE_lnrr)^2
model_est_adjusted_lnrr$folded_v <- folded_error(model_est_adjusted_lnrr$mu_lnrr, model_est_adjusted_lnrr$variance)

## meta-meta-analysis
MMA_magnitude_adjusted_lnrr <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_adjusted_lnrr) 



# SMDH
## absoulte value of mean
model_est_SMDH$abs_mean <- abs(model_est_SMDH$mu_SMDH)
## folded sampling variance
### get orignal sampling variance first
model_est_SMDH$variance <- (model_est_SMDH$SE_SMDH)^2
model_est_SMDH$folded_v <- folded_error(model_est_SMDH$mu_SMDH, model_est_SMDH$variance)

## meta-meta-analysis
MMA_magnitude_SMDH <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_SMDH) 




# adjusted_SMDH
## absoulte value of mean
model_est_adjusted_SMDH$abs_mean <- abs(model_est_adjusted_SMDH$mu_SMDH)
## folded sampling variance
### get orignal sampling variance first
model_est_adjusted_SMDH$variance <- (model_est_adjusted_SMDH$SE_SMDH)^2
model_est_adjusted_SMDH$folded_v <- folded_error(model_est_adjusted_SMDH$mu_SMDH, model_est_adjusted_SMDH$variance)

## meta-meta-analysis
MMA_magnitude_adjusted_SMDH <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_adjusted_SMDH) 



# lnVR
## absoulte value of mean
model_est_VR$abs_mean <- abs(model_est_VR$mu_VR)
## folded sampling variance
### get orignal sampling variance first
model_est_VR$variance <- (model_est_VR$SE_VR)^2
model_est_VR$folded_v <- folded_error(model_est_VR$mu_VR, model_est_VR$variance)

## meta-meta-analysis
MMA_magnitude_VR <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_VR) 




# lnCVR
## absoulte value of mean
model_est_CVR$abs_mean <- abs(model_est_CVR$mu_CVR)
## folded sampling variance
### get orignal sampling variance first
model_est_CVR$variance <- (model_est_CVR$SE_CVR)^2
model_est_CVR$folded_v <- folded_error(model_est_CVR$mu_CVR, model_est_CVR$variance)

## meta-meta-analysis
MMA_magnitude_CVR <- rma(yi = abs_mean, vi = folded_v, test = "t", method = "REML", data = model_est_CVR) 




# using orchard package to make figure
## MAOM
MMA_magnitude_RR_results <- mod_results(MMA_magnitude_RR, mod = "Int")
MMA_magnitude_lnrr_results <- mod_results(MMA_magnitude_lnrr, mod = "Int")
MMA_magnitude_SMD_results <- mod_results(MMA_magnitude_SMD, mod = "Int")
MMA_magnitude_SMDH_results <- mod_results(MMA_magnitude_SMDH, mod = "Int")

MMA_magnitude_mean_results <- submerge(MMA_magnitude_SMDH_results, MMA_magnitude_SMD_results, MMA_magnitude_lnrr_results,  MMA_magnitude_RR_results, mix = TRUE)


table_results <- MMA_magnitude_mean_results$mod_table
orchard_magnitude_mean <- orchaRd::orchard_plot(MMA_magnitude_mean_results, mod = "Int", xlab = "Magnitude of mean differences (MAOM)", k = FALSE, transfm = "none", angle = 0)+ xlim(-1.5, 6) + scale_y_discrete(labels = c("SMDH", "SMD","lnRR", "lnRR*")) + labs(title = "(A)", y = "Mean differences") +
  annotate(geom = "text",
           x = 4.3, y = 1.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[1]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[1]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[1]], 3), " to ",
                                                     round(table_results$upperPR[[1]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 1.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 4.3, y = 2.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[2]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[2]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[2]], 3), " to ",
                                                     round(table_results$upperPR[[2]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 2.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 4.3, y = 3.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[3]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[3]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[3]], 3), " to ",
                                                     round(table_results$upperPR[[3]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 3.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 4.3, y = 4.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[4]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[4]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[4]], 3), " to ",
                                                     round(table_results$upperPR[[4]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 4.2, size = 3, label = paste("italic(k)==", "30"), parse = TRUE)




png(filename = "./Figures/orchard_magnitude_mean.png", width = 4, height = 4, units = "in", type = "windows", res = 400)
orchard_magnitude_mean
dev.off() 



## cMAOM
MMA_magnitude_adjusted_RR_results <- mod_results(MMA_magnitude_adjusted_RR, mod = "Int")
MMA_magnitude_adjusted_lnrr_results <- mod_results(MMA_magnitude_adjusted_lnrr, mod = "Int")
MMA_magnitude_adjusted_SMD_results <- mod_results(MMA_magnitude_adjusted_SMD, mod = "Int")
MMA_magnitude_adjusted_SMDH_results <- mod_results(MMA_magnitude_adjusted_SMDH, mod = "Int")

MMA_magnitude_adjusted_mean_results <- submerge(MMA_magnitude_adjusted_SMDH_results, MMA_magnitude_adjusted_SMD_results, MMA_magnitude_adjusted_lnrr_results,  MMA_magnitude_adjusted_RR_results, mix = TRUE)


table_results <- MMA_magnitude_adjusted_mean_results$mod_table
orchard_magnitude_adjusted_mean <- orchard_plot(MMA_magnitude_adjusted_mean_results, mod = "Int", xlab = "Magnitude of mean differences (cMAOM)", transfm = "none", angle = 0, k = FALSE,) + xlim(-1.5, 6) + scale_y_discrete(labels = c("SMDH", "SMD","lnRR", "lnRR*")) +
  labs(title = "(B)", y = "Mean differences") +
  annotate(geom = "text",
           x = 4.3, y = 1.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[1]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[1]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[1]], 3), " to ",
                                                     round(table_results$upperPR[[1]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 1.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 4.3, y = 2.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[2]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[2]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[2]], 3), " to ",
                                                     round(table_results$upperPR[[2]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 2.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 4.3, y = 3.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[3]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[3]], 3),  " to ", round(table_results$upperCL[[3]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[3]], 3), " to ",
                                                     round(table_results$upperPR[[3]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 3.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 4.3, y = 4.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[4]],
                                                                     3), "\n", "[95% CI = ", round(table_results$lowerCL[[4]], 3),  " to ", round(table_results$upperCL[[4]], 3), "]", "\n", "[95% PI = ",
                                                     round(table_results$lowerPR[[4]], 3), " to ",
                                                     round(table_results$upperPR[[4]], 3), "]")) +
  annotate(geom = "text", x = -1, y = 4.2, size = 3, label = paste("italic(k)==", "30"), parse = TRUE)


png(filename = "./Figures/orchard_magnitude_adjusted_mean.png", width = 4, height = 4, units = "in", type = "windows", res = 400)
orchard_magnitude_adjusted_mean
dev.off() 


# layout
orchard_magnitude_mean_all <- orchard_magnitude_mean + orchard_magnitude_adjusted_mean + plot_layout(ncol = 2, nrow = 1, widths = 8, heights = 4)

png(filename = "./Figures/orchard_magnitude_mean_all.png", width = 8, height = 4, units = "in", type = "windows", res = 400)
orchard_magnitude_mean_all
dev.off() 





# variance differences
MMA_magnitude_VR_results <- mod_results(MMA_magnitude_VR, mod = "Int")
MMA_magnitude_CVR_results <- mod_results(MMA_magnitude_CVR, mod = "Int")

MMA_magnitude_variance_results <- submerge(MMA_magnitude_CVR_results, MMA_magnitude_VR_results, mix = TRUE)

table_results <- MMA_magnitude_variance_results$mod_table

orchard_magnitude_variance <- orchaRd::orchard_plot(MMA_magnitude_variance_results, mod = "Int", xlab = "Magnitude of variance differences (MAOM)", k = FALSE, transfm = "none", angle = 0)+ xlim(-0.2, 1) + scale_y_discrete(labels = c("lnCVR", "lnVR")) + labs(title = "", y = "Variance differences") +
  annotate(geom = "text",
           x = 0.83, y = 1.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[1]],
                                                                      3), "\n", "[95% CI = ", round(table_results$lowerCL[[1]], 3),  " to ", round(table_results$upperCL[[1]], 3), "]", "\n", "[95% PI = ",
                                                      round(table_results$lowerPR[[1]], 3), " to ",
                                                      round(table_results$upperPR[[1]], 3), "]")) +
  annotate(geom = "text", x = -0.1, y = 1.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) +
  
  annotate(geom = "text",
           x = 0.83, y = 2.2, size = 3,label = paste0("MMM = ", round(table_results$estimate[[2]],
                                                                      3), "\n", "[95% CI = ", round(table_results$lowerCL[[2]], 3),  " to ", round(table_results$upperCL[[2]], 3), "]", "\n", "[95% PI = ",
                                                      round(table_results$lowerPR[[2]], 3), " to ",
                                                      round(table_results$upperPR[[2]], 3), "]")) +
  annotate(geom = "text", x = -0.1, y = 2.2, size = 3, label = paste("italic(k)==", "12"), parse = TRUE) 




png(filename = "./Figures/orchard_magnitude_variance.png", width = 4.8, height = 4, units = "in", type = "windows", res = 400)
orchard_magnitude_variance
dev.off() 
