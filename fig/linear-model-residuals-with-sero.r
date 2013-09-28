#' Create plots comparing residuals of a model that uses serological
#' data and one that does not.

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
              gi_severity = factor(gi_severity, levels = 0:4),
              aca = factor(aca, levels = 0:1),
              scl_70 = factor(scl_70, levels = 0:1))

data <- na.omit(data)

glmfit_sero <-
    glm(worst_pfvc ~
       sex +
       afr.am +
       smoker +
       rp_severity +
       gi_severity +
       base_pfvc +
       base_pfev1 +
       base_ptlc +
       base_pdlco +
       aca +
       scl_70,
       data = data)

require(boot)

cv_sero <- cv.glm(data, glmfit_sero)
cat(sprintf("Sero LOOCV MSE = %f\n", cv_sero$delta[1]))

glmfit_nosero <-
    glm(worst_pfvc ~
       sex +
       afr.am +
       smoker +
       rp_severity +
       gi_severity +
       base_pfvc +
       base_pfev1 +
       base_ptlc +
       base_pdlco,
       data = data)

cv_nosero <- cv.glm(data, glmfit_nosero)
cat(sprintf("No Sero LOOCV MSE = %f\n", cv_nosero$delta[1]))
