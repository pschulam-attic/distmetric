#' Create plots displaying residuals for a simple linear model

library(ggplot2)
library(plyr)
library(reshape2)
library(rjson)

config_file <- "config.json"
config <- fromJSON(file = config_file)

data_fn <- file.path(config$data_work, config$cross_sectional_data)
data <- read.csv(data_fn, stringsAsFactors = FALSE)

data <-
    transform(data,
              sex = factor(sex, levels = 0:1),
              afr.am = factor(afr.am, levels = 0:1),
              smoker = factor(smoker, levels = 0:2),
              rp_severity = factor(rp_severity, levels = 0:4),
              gi_severity = factor(gi_severity, levels = 0:4))

glmfit <-
    glm(worst_pfvc ~
       sex +
       afr.am +
       smoker +
       height +
       weight +
       age +
       total_skin +
       rp_severity +
       gi_severity +
       base_pfvc +
       base_pfev1 +
       base_ptlc +
       base_pdlco,
       data = data)

require(boot)

cv_results <- cv.glm(data, glmfit)
cat(sprintf("LOOCV MSE = %f", cv_results$delta[2]))

nplots <- 0
dir.create(config$plots$dir, showWarnings = FALSE)

plot_name <- function(plot_num) {
    bn <- paste0(config$plots[["linear-model-residuals"]], "-", plot_num, config$plots$ext)
    file.path(config$plots$dir, bn)
}

#---------------------------------------------------------------------
#' Plot 1: Residuals of linear regression model
#---------------------------------------------------------------------

p <- qplot(resid(glmfit))
title <- sprintf("Residuals of Linear Model (LOOCV MSE = %f)", cv_results$delta[2])
p + labs(title = title)
nplots <- nplots + 1

ggsave(plot_name(nplots))

#---------------------------------------------------------------------
#' Plot 2: Standard errors of fit values 
#---------------------------------------------------------------------

results <- predict(glmfit, se.fit=TRUE)
p <- qplot(results$se.fit * results$residual.scale)
title <- sprintf("Fit Standard Errors")
p + labs(title = title)
nplots <- nplots + 1

ggsave(plot_names(nplots))
