#' Create plots displaying residuals for simple NW regression

library(ggplot2)
library(plyr)
library(reshape2)
library(rjson)
library(R.matlab)

config_file <- "config.json"
config <- fromJSON(file=config_file)

data_fn <- file.path(config$data_work, config$cross_sectional_data)
data <- read.csv(data_fn, stringsAsFactors=FALSE)

xvars <- c("sex", "smoker", "afr.am", "total_skin", "rp_severity",
            "gi_severity", "height", "weight", "age", "base_pfvc",
            "base_pfev1", "base_ptlc", "base_pdlco")

yvar <- "worst_pfvc"

X <- as.matrix(data[, xvars])
Y <- data[, yvar]
A <- readMat("data/work/quant20A.mat")$A

source("src/nadarayawatson.r")

nwfit <- nadarayawatson(X, Y, "mahalanobis", A)

p <- qplot(predict(nwfit) - Y)
title <-
    sprintf("Residuals of Learned Metric Nad. Wat. (LOOCV MSE = %f)",
            loocv_mse(nwfit))
p + labs(title=title)
