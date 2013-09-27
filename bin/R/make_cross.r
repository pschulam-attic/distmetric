library(rjson)
library(reshape2)
library(plyr)

args <- commandArgs(TRUE)

if (length(args) != 1) {
    stop("usage: make_cross.r CONFIG")
} else {
    config.file <- args[1]
}

config <- fromJSON(file = config.file)

# There are 3 initial sources of data that we will look at
#   - Clinical
#   - Pulmonary Function Test (PFT)
#   - Serological

# First, we want to read in the patient data itself.

patient.data <- read.csv(
    file.path(config$data_input, config$patient[["out"]]),
    stringsAsFactors = FALSE
    )

patient.data <-
    transform(patient.data,
              afr.am = ifelse(race1 == 2, 1, 0))

patient.data <-
    subset(patient.data,
           smoker != 9,
           select = -c(race1, race2, ethnicity))

first.seen <- local({
    patient.to.date <- with(patient.data, {
        structure(as.Date(date_first_seen), names = patient_id)
    })

    function(pid) {
        patient.to.date[as.character(pid)]
    }
})

# This is a general function for selecting dates that fall within some
# specified number of days of when the patient was first seen.

is.date.within <- function(ndays, pid, date) {
    seen <- first.seen(pid)
    date.diff <- as.Date(date) - seen
    date.diff >= 0 & date.diff <= ndays
}

is.date.outside <- function(ndays, pid, date) {
    seen <- first.seen(pid)
    date.diff <- as.Date(date) - seen
    date.diff >= ndays
}

# This is a general function for summarizing multiple baseline
# measurements for a patient. We will typically take the mean of their
# baseline measurements. If it is a factor, we take the mean and
# round.

summarize.baseline <- function(measurements) {
    is.fac <- is.factor(measurements)
    m <- na.omit(measurements)
    s <- NA

    if (length(m) > 0) {
        if (is.fac) {
            s <- max(m)
        } else {
            s <- mean(m)
        }
    }
    return(s)
}

# Examine the clinical data first. We'd like to know how many patients
# have sufficient clinical data within a year of first being seen.

clinic.data <- read.csv(
    file.path(config$data_input, config$clinic[["out"]]),
    stringsAsFactors = FALSE
    )

clinic.data <- transform(
    clinic.data,
    date = as.Date(date),
    total_skin = factor(total_skin, levels = 0:51, ordered = TRUE),
    rp_severity = factor(rp_severity, levels = 0:4, ordered = TRUE),
    gi_severity = factor(gi_severity, levels = 0:4, ordered = TRUE)
    )

clinic.data <- subset(clinic.data, select = -c(skin_severity))

is_bl <- is.date.within(365, clinic.data$patient_id, clinic.data$date)
clinic.baseline <- clinic.data[is_bl, ]

clinic.baseline.m <- melt(
    clinic.baseline,
    measure.vars = c("total_skin", "rp_severity", "gi_severity")
    )

clinic.baseline.m <- transform(
    clinic.baseline.m,
    value = factor(value, levels = 0:51, ordered = TRUE)
    )

clinic.baseline.m <- ddply(
    clinic.baseline.m,
    ~ patient_id + variable,
    function(df) {
        v <- na.omit(df$value)

        if (length(v) > 0) {
            data.frame(base_value = max(v))
        } else {
            data.frame()
        }
    }
    )

clinic <- dcast(clinic.baseline.m, patient_id ~ variable, value.var = "base_value")
clinic <- transform(
    clinic,
    total_skin = factor(as.integer(total_skin), levels = 0:51, ordered = TRUE),
    rp_severity = factor(as.integer(rp_severity), levels = 0:4, ordered = TRUE),
    gi_severity = factor(as.integer(gi_severity), levels = 0:4, ordered = TRUE)
    )

clinic <- na.omit(clinic)

# Examine the PFT data. We're interested in similar questions as
# before.

pft.data <- read.csv(
    file.path(config$data_input, config$pft[["out"]]),
    stringsAsFactors = FALSE
    )

pft.data <- transform(
    pft.data,
    date = as.Date(date)
    )

pft.data <- subset(pft.data, select = -c(fvc, fev1, tlc, dlco))

is_bl <- is.date.within(365, pft.data$patient_id, pft.data$date)
pft.baseline <- pft.data[is_bl, ]

pft.baseline.m <-
    melt(pft.baseline,
         id.vars = c("patient_id", "date"))

pft.base.sum <-
    ddply(pft.baseline.m,
          ~ patient_id + variable,
          summarize,
          base_value = summarize.baseline(value))

pft <- dcast(pft.base.sum, patient_id ~ variable, value.var = "base_value")
pft <- rename(pft,
              list(pfvc = "base_pfvc",
                   pfev1 = "base_pfev1",
                   ptlc = "base_ptlc",
                   pdlco = "base_pdlco"))

pft <- na.omit(pft)

#---------------------------------------------------------------------
# Get lifetime worst measurements
#---------------------------------------------------------------------

is_lt_low <- is.date.outside(365 * 2, pft.data$patient_id, pft.data$date)
is_lt_high <- is.date.within(365 * 7, pft.data$patient_id, pft.data$date)
is_lt <- is_lt_low & is_lt_high
pft.lifetime <- pft.data[is_lt, ]

worst.data <- ddply(
    melt(
        pft.lifetime,
        id.vars = c("patient_id", "date"),
        measure.vars = c("pfvc", "pfev1", "ptlc", "pdlco")),
    ~ patient_id + variable,
    function(df) {
        v <- na.omit(df$value)

        if (length(v) > 0) {
            w <- median(quantile(v, 0.1))
            data.frame(worst = w, nsamples = length(v))
        } else {
            data.frame()
        }
    })

worst.data <- dcast(worst.data, patient_id ~ variable, value.var = "worst")
worst.names <- names(worst.data)
names(worst.data) <-
    c(worst.names[1],
      vapply(worst.names[-1], function(n) paste0("worst_", n),character(1)))
worst.data <- na.omit(worst.data)

# Unify the data sets

p1 <- unique(patient.data$patient_id)
p2 <- unique(clinic$patient_id)
p3 <- unique(pft$patient_id)
p4 <- unique(worst.data$patient_id)

patient_ids <- Reduce(intersect, list(p1, p2, p3, p4))

final.data <-
    Reduce(
        function(d1, d2) merge(d1, d2, by = "patient_id"),
        lapply(
            list(patient.data, clinic, pft, worst.data),
            function(d) subset(d, patient_id %in% patient_ids)))

final.data <-
    local({
        n <- names(final.data)
        final.data[, -grep("date_", n)]
    })

final.data <- subset(final.data, gi_severity != 4)  # Only one person has GI = 4

dir.create(config$data_work, showWarnings = FALSE)
cross_file <- file.path(config$data_work, config$cross_sectional_data)
write.csv(final.data, file = cross_file, row.names = FALSE)
