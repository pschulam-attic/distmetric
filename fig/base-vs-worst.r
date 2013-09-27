#' Create plots related to the difference between baseline
#' measurements and worst follow up measurements for patients.

library(ggplot2)
library(plyr)
library(reshape2)
library(rjson)

config.file <- "config.json"
config <- fromJSON(file = config.file)

cross_data <-
    local({
        fn <- file.path(config$data_work, config$cross_sectional_data)
        read.csv(fn, stringsAsFactors = FALSE)
    })


f1 <- function(df, prefix) {
    transform(melt(df, id.vars = "patient_id",
                   measure.vars = grep(prefix, names(df)),
                   value.name = paste0(prefix, "value")),
              variable = sub(prefix, "", variable))
}

juxt_data <-
    arrange(merge(f1(cross_data, "base_"),
                  f1(cross_data, "worst_"),
                  by = c("patient_id", "variable")),
            patient_id, variable)

nplots <- 0
dir.create(config$plots$dir, showWarnings = FALSE)

#---------------------------------------------------------------------
#' Plot 1: Within-variable differences
#---------------------------------------------------------------------

p <- ggplot(juxt_data, aes(worst_value - base_value))
p <- p + geom_histogram() + facet_wrap(~ variable, nrow = 2)
p + labs(title = "Change in PFT from Base to Worst")
nplots <- nplots + 1

ggsave(local({
    bn <- paste0(config$plots[["base-vs-worst"]], "-", nplots, config$plots$ext)
    fn <- file.path(config$plots$dir, bn)
}))

