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
       rp_severity +
       gi_severity +
       base_pfvc +
       base_pfev1 +
       base_ptlc +
       base_pdlco,
       data = data)

require(boot)

cv_results <- cv.glm(data, glmfit)
cat(sprintf("LOOCV MSE = %f", cv_results$delta[1]))

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
p + labs(title = "Residuals of Linear Model")
nplots <- nplots + 1

ggsave(plot_name(nplots))

#---------------------------------------------------------------------
#' Plot 2: How do residuals change across sex?
#---------------------------------------------------------------------

resid_data <-
    transform(data,
              resid = resid(glmfit),
              sex = factor(ifelse(sex == 0, "male", "female"), levels = c("male", "female")),
              afr.am = factor(ifelse(afr.am == 0, "other", "afr. am."), levels = c("other", "afr. am.")),
              smoker = factor(ifelse(smoker == 0, "never",
                  ifelse(smoker == 1, "former", "current")), levels = c("never", "former", "current")))

p <- ggplot(resid_data, aes(x = resid))
p <- p + geom_histogram() + facet_wrap(~ sex, ncol = 1)
p + labs(title = "Residuals as a Function of Sex")
nplots <- nplots + 1

ggsave(plot_name(nplots))

#---------------------------------------------------------------------
#' Plot 3: How do residuals change across race?
#---------------------------------------------------------------------

p <- ggplot(resid_data, aes(x = resid))
p <- p + geom_histogram() + facet_wrap(~ afr.am, ncol = 1)
p + labs(title = "Residuals as a Function of Race")
nplots <- nplots + 1

ggsave(plot_name(nplots))

#---------------------------------------------------------------------
#' Plot 4: How do residuals change across smoking status?
#---------------------------------------------------------------------

p <- ggplot(resid_data, aes(x = resid))
p <- p + geom_histogram() + facet_wrap(~ smoker, ncol = 1)
p + labs(title = "Residuals as a Function of Smoking Status")
nplots <- nplots + 1

ggsave(plot_name(nplots))

#---------------------------------------------------------------------
#' Plot 5: Residuals as a function baseline PFTs
#---------------------------------------------------------------------

resid_data_m <-
    melt(resid_data,
         id.vars = "resid",
         measure.vars = names(resid_data)[grep("base_", names(resid_data))])

p <- ggplot(resid_data_m, aes(x = value, y = resid))
p <- p + geom_point(alpha = 0.5) + facet_wrap(~ variable)
p + labs(title = "Residuals as a Function of Baseline PFTs")
nplots <- nplots + 1

ggsave(plot_name(nplots))

#---------------------------------------------------------------------
#' Plot 5: Residuals as a function baseline PFTs
#---------------------------------------------------------------------

resid_data_m <-
    melt(resid_data,
         id.vars = "resid",
         measure.vars = names(resid_data)[grep("worst_", names(resid_data))])

p <- ggplot(resid_data_m, aes(x = value, y = resid))
p <- p + geom_point(alpha = 0.5) + facet_wrap(~ variable)
p + labs(title = "Residuals as a Function of Worst Follow-up PFTs")
nplots <- nplots + 1

ggsave(plot_name(nplots))

