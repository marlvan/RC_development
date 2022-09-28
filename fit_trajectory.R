library(xlsx)
library(gamm4)
library(itsadug)
library(ggplot2)
library(extrafont)
library(zoo)
library(MASS)
library(boot)
library(resample)


# Load data ---------------------------------------------------------------

# Specify the input/output suffix
suf <- 'data'

# Load a text file containing (a) subject ID ("Subject"), (b) data to fit trajectory to ("Data"), (c) corresponding age ("Age"), and (d) corresponding sex ("Sex")
data <-
  read.delim(paste(suf, '.txt', sep = ''), header = TRUE, sep = "\t")

# Specify the

# Determine optimal model (Table 2) --------------------------------------------

# Compute the AIC values for the males
AIC_val_M <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(AIC_val_M) <-
  c("Intercept", "Linear", "Quadratic", "Cubic", "GAMM")
tbl_sex <- subset(tbl, tbl$Sex == 'M')
mdl1 <-
  gamm4(
    formula = Data ~ 1,
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl2 <-
  gamm4(
    formula = Data ~ Age,
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl3 <-
  gamm4(
    formula = Data ~ Age + I(Age ^ 2),
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl4 <-
  gamm4(
    formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl5 <-
  gamm4(
    formula = Data ~ s(Age),
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
AIC_val_M$Intercept = AIC(mdl1$mer)
AIC_val_M$Linear = AIC(mdl2$mer)
AIC_val_M$Quadratic = AIC(mdl3$mer)
AIC_val_M$Cubic = AIC(mdl4$mer)
AIC_val_M$GAMM = AIC(mdl5$mer)

# Determine the optimal model for the males
opt_mdlM = which.min(as.numeric(unlist(AIC_val_M)))

# Compute the AIC values for the females
AIC_val_F <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(AIC_val_F) <-
  c("Intercept", "Linear", "Quadratic", "Cubic", "GAFF")
tbl_sex <- subset(tbl, tbl$Sex == 'F')
mdl1 <-
  gamm4(
    formula = Data ~ 1,
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl2 <-
  gamm4(
    formula = Data ~ Age,
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl3 <-
  gamm4(
    formula = Data ~ Age + I(Age ^ 2),
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl4 <-
  gamm4(
    formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
mdl5 <-
  gamm4(
    formula = Data ~ s(Age),
    random = ~ (1 | Subject),
    data = tbl_sex,
    REML = FALSE
  )
AIC_val_F$Intercept = AIC(mdl1$mer)
AIC_val_F$Linear = AIC(mdl2$mer)
AIC_val_F$Quadratic = AIC(mdl3$mer)
AIC_val_F$Cubic = AIC(mdl4$mer)
AIC_val_F$GAFF = AIC(mdl5$mer)

# Determine the optimal model for the males
opt_mdlF = which.min(as.numeric(unlist(AIC_val_F)))


# Plot the trajectories (Figure 1) ----------------------------------------

# Plot each frequency band separately, with both sexes
tbl_sexF <- tbl
tbl_sexF <- subset(tbl_sexF, Sex == 'F')
tbl_sexM <- tbl
tbl_sexM <- subset(tbl_sexM, Sex == 'M')
if (opt_mdlF == 1) {
  mdlF <-
    gamm4(
      formula = Data ~ 1,
      random = ~ (1 | Subject),
      data = tbl_sexF,
      REML = TRUE
    )
} else if (opt_mdlF == 2) {
  mdlF <-
    gamm4(
      formula = Data ~ Age,
      random = ~ (1 | Subject),
      data = tbl_sexF,
      REML = TRUE
    )
} else if (opt_mdlF == 3) {
  mdlF <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2),
      random = ~ (1 | Subject),
      data = tbl_sexF,
      REML = TRUE
    )
} else if (opt_mdlF == 4) {
  mdlF <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
      random = ~ (1 | Subject),
      data = tbl_sexF,
      REML = TRUE
    )
}
if (opt_mdlM == 1) {
  mdlM <-
    gamm4(
      formula = Data ~ 1,
      random = ~ (1 | Subject),
      data = tbl_sexM,
      REML = TRUE
    )
} else if (opt_mdlM == 2) {
  mdlM <-
    gamm4(
      formula = Data ~ Age,
      random = ~ (1 | Subject),
      data = tbl_sexM,
      REML = TRUE
    )
} else if (opt_mdlM == 3) {
  mdlM <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2),
      random = ~ (1 | Subject),
      data = tbl_sexM,
      REML = TRUE
    )
} else if (opt_mdlM == 4) {
  mdlM <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
      random = ~ (1 | Subject),
      data = tbl_sexM,
      REML = TRUE
    )
}
new_dataF <-
  with(tbl_sexF, expand.grid(
    Age = seq(min(Age), max(Age), length = 500),
    Subject = as.character(tbl_sexF$Subject[1])
  ))
new_dataM <-
  with(tbl_sexM, expand.grid(
    Age = seq(min(Age), max(Age), length = 500),
    Subject = as.character(tbl_sexM$Subject[1])
  ))
predF <-
  predict(mdlF$gam,
          newdata = new_dataF,
          exclude = "(1|Subject)",
          se.fit = TRUE)
predF <- cbind(predF, new_dataF)
ilink <- family(mdlF$mer)$linkinv
predF <-
  transform(
    predF,
    lwr_ci = ilink(fit - (2 * se.fit)),
    uppr_ci = ilink(fit + (2 * se.fit)),
    fitted = ilink(fit)
  )
predM <-
  predict(mdlM$gam,
          newdata = new_dataM,
          exclude = "(1|Subject)",
          se.fit = TRUE)
predM <- cbind(predM, new_dataM)
ilink <- family(mdlM$mer)$linkinv
predM <-
  transform(
    predM,
    lwr_ci = ilink(fit - (2 * se.fit)),
    uppr_ci = ilink(fit + (2 * se.fit)),
    fitted = ilink(fit)
  )
ggplot() +
  geom_ribbon(
    data = predF,
    aes(
      x = Age,
      y = fitted,
      ymin = lwr_ci,
      ymax = uppr_ci
    ),
    fill = 'deeppink3',
    alpha = 0.2
  ) +
  geom_line(data = predF, aes(x = Age, y = fitted), color = 'deeppink3') +
  geom_point(data = tbl_sexF, aes(x = Age, y = Data), color = 'deeppink3') +
  geom_ribbon(
    data = predM,
    aes(
      x = Age,
      y = fitted,
      ymin = lwr_ci,
      ymax = uppr_ci
    ),
    fill = 'dodgerblue3',
    alpha = 0.2
  ) +
  geom_line(data = predM, aes(x = Age, y = fitted), color = 'dodgerblue3') +
  geom_point(data = tbl_sexM, aes(x = Age, y = Data), color = 'dodgerblue3') +
  labs(x = 'Age (years)', y = 'Data') +
  ylim(-2, 24) +
  theme_minimal(base_size = 24, base_family = 'Futura Md BT') +
  theme(
    axis.title.x = element_text(vjust = 0),
    axis.title.y = element_text(vjust = 2),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
ggsave(filename = paste(suf, "_sex.png", sep = ""), dpi = 300)


# Extract statistics (Supplemental Table 1) -------------------------------

# Females
s_F <- summary(mdlF$gam)

# Males
s_M <- summary(mdlM$gam)

# Compute features of interest with bootstrapping (Table 3) ---------------

# Set up parallel
library(foreach)
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(n.cores,
                                    type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)
set.seed(1)

# Define functions to compute the features of interest

# Slope
get_slope <- function(tbl, ind) {
  tbl_subset <- tbl[ind, ]
  mdl <-
    gamm4(
      formula = Data ~ Age,
      random = ~ (1 | Subject),
      data = tbl_subset,
      REML = TRUE
    )
  slope <- as.numeric(mdl$gam$coefficients[2])
  return(slope)
}

# Local maximum
get_max <- function(tbl, ind) {
  tbl_subset <- tbl[ind, ]
  mdl <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
      random = ~ (1 | Subject),
      data = tbl_subset,
      REML = TRUE
    )
  new_data <-
    with(tbl_subset, expand.grid(
      Age = seq(min(Age), max(Age), length = 10000),
      Subject = as.character(tbl_subset$Subject[1])
    ))
  pp <-
    predict(mdl$gam,
            newdata = new_data,
            exclude = "(1|Subject)",
            type = "response")
  imax <- rollapply(as.zoo(pp), 3, function(x)
    which.max(x) == 2)
  imax <- index(imax)[coredata(imax)]
  if (length(imax) > 0) {
    return(new_data$Age[imax])
  } else {
    return(NA)
  }
}

# Local minimum
get_min <- function(tbl, ind) {
  tbl_subset <- tbl[ind, ]
  mdl <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
      random = ~ (1 | Subject),
      data = tbl_subset,
      REML = TRUE
    )
  new_data <-
    with(tbl_subset, expand.grid(
      Age = seq(min(Age), max(Age), length = 10000),
      Subject = as.character(tbl_subset$Subject[1])
    ))
  pp <-
    predict(mdl$gam,
            newdata = new_data,
            exclude = "(1|Subject)",
            type = "response")
  imin <- rollapply(as.zoo(pp), 3, function(x)
    which.min(x) == 2)
  imin <- index(imin)[coredata(imin)]
  if (length(imin) > 0) {
    return(new_data$Age[imin])
  } else {
    return(NA)
  }
}

# Get inflection point
get_inf <- function(tbl, ind) {
  tbl_subset <- tbl[ind, ]
  mdl <-
    gamm4(
      formula = Data ~ Age + I(Age ^ 2) + I(Age ^ 3),
      random = ~ (1 | Subject),
      data = tbl_subset,
      REML = TRUE
    )
  new_data <-
    with(tbl_subset, expand.grid(
      Age = seq(min(Age), max(Age), length = 10000),
      Subject = as.character(tbl_subset$Subject[1])
    ))
  pp <-
    predict(mdl$gam,
            newdata = new_data,
            exclude = "(1|Subject)",
            type = "response")
  iinf <- which.min(diff(pp))
  if (length(iinf) > 0 &&
      iinf[1] != 1 &&
      iinf[1] != length(pp) - 1) {
    return(new_data$Age[iinf])
  } else {
    return(NA)
  }
}

# Iterate over the frequency bands, and store features where first row is the mean across all the boostrap iterations,
# second row is the lower confidence interval, third row is the upper confidence interval, and 4th row is the percentage
# of times the feature is present in the bootstraps
slope_M <- matrix(0, 4, 1)
local_min_M <- matrix(0, 4, 1)
local_max_M <- matrix(0, 4, 1)
inflection_M <- matrix(0, 4, 1)
slope_F <- matrix(0, 4, 1)
local_min_F <- matrix(0, 4, 1)
local_max_F <- matrix(0, 4, 1)
inflection_F <- matrix(0, 4, 1)

# Extract the data
sampling_pct <- 0.632
n <- 10000
tbl_sexF <- data
tbl_sexF <- subset(tbl_sexF, Sex == 'F')
tbl_sexM <- data
tbl_sexM <- subset(tbl_sexM, Sex == 'M')

# Females
if (opt_mdlF == 2) {
  # If linear, compute slope
  feature <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_F = sample(1:nrow(tbl_sexF), round(sampling_pct * nrow(tbl_sexF)))
    return(get_slope(tbl_sexF, ind_F))
  }
  feature <- matrix(feature, nrow = n, byrow = TRUE)
  slope_F[1,] <- mean(feature, na.rm = TRUE)
  q <- quantile(feature, probs = c(0.025, 0.975), na.rm = TRUE)
  slope_F[2,] <- q[1]
  slope_F[3,] <- q[2]
  slope_F[4,] <- sum(!is.na(feature)) / n
  
} else if (opt_mdlF == 3) {
  # If quadratic, compute local max and min (will only need to report one)
  feature_max <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = 'gamm4'
  ) %dopar% {
    ind_F = sample(1:nrow(tbl_sexF), round(sampling_pct * nrow(tbl_sexF)))
    return(get_max(tbl_sexF, ind_F))
  }
  local_max_F[1,] <- mean(feature_max, na.rm = TRUE)
  q <- quantile(feature_max,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_max_F[2,] <- q[1]
  local_max_F[3,] <- q[2]
  local_max_F[4,] <- sum(!is.na(feature_max)) / n
  
  feature_min <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = 'gamm4'
  ) %dopar% {
    ind_F = sample(1:nrow(tbl_sexF), round(sampling_pct * nrow(tbl_sexF)))
    return(get_min(tbl_sexF, ind_F))
  }
  local_min_F[1,] <- mean(feature_min, na.rm = TRUE)
  q <- quantile(feature_min,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_min_F[2,] <- q[1]
  local_min_F[3,] <- q[2]
  local_min_F[4,] <- sum(!is.na(feature_min)) / n
  
} else if (opt_mdlF == 4) {
  # If cubic, compute local max and min and inflection point
  feature_max <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_F = sample(1:nrow(tbl_sexF), round(sampling_pct * nrow(tbl_sexF)))
    return(get_max(tbl_sexF, ind_F))
  }
  local_max_F[1,] <- mean(feature_max, na.rm = TRUE)
  q <- quantile(feature_max,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_max_F[2,] <- q[1]
  local_max_F[3,] <- q[2]
  local_max_F[4,] <- sum(!is.na(feature_max)) / n
  
  feature_min <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_F = sample(1:nrow(tbl_sexF), round(sampling_pct * nrow(tbl_sexF)))
    return(get_min(tbl_sexF, ind_F))
  }
  local_min_F[1,] <- mean(feature_min, na.rm = TRUE)
  q <- quantile(feature_min,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_min_F[2,] <- q[1]
  local_min_F[3,] <- q[2]
  local_min_F[4,] <- sum(!is.na(feature_min)) / n
  
  feature_inf <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_F = sample(1:nrow(tbl_sexF), round(sampling_pct * nrow(tbl_sexF)))
    return(get_inf(tbl_sexF, ind_F))
  }
  inflection_F[1,] <- mean(feature_inf, na.rm = TRUE)
  q <- quantile(feature_inf,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  inflection_F[2,] <- q[1]
  inflection_F[3,] <- q[2]
  inflection_F[4,] <- sum(!is.na(feature_inf)) / n
  
}

# Males
if (opt_mdlM == 2) {
  # If linear, compute slope
  feature <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_M = sample(1:nrow(tbl_sexM), round(sampling_pct * nrow(tbl_sexM)))
    return(get_slope(tbl_sexM, ind_M))
  }
  feature <- matrix(feature, nrow = n, byrow = TRUE)
  slope_M[1,] <- mean(feature, na.rm = TRUE)
  q <- quantile(feature, probs = c(0.025, 0.975), na.rm = TRUE)
  slope_M[2,] <- q[1]
  slope_M[3,] <- q[2]
  slope_M[4,] <- sum(!is.na(feature)) / n
  
} else if (opt_mdlM == 3) {
  # If quadratic, compute local max and min (will only need to report one)
  feature_max <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_M = sample(1:nrow(tbl_sexM), round(sampling_pct * nrow(tbl_sexM)))
    return(get_max(tbl_sexM, ind_M))
  }
  local_max_M[1,] <- mean(feature_max, na.rm = TRUE)
  q <- quantile(feature_max,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_max_M[2,] <- q[1]
  local_max_M[3,] <- q[2]
  local_max_M[4,] <- sum(!is.na(feature_max)) / n
  
  feature_min <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_M = sample(1:nrow(tbl_sexM), round(sampling_pct * nrow(tbl_sexM)))
    return(get_min(tbl_sexM, ind_M))
  }
  local_min_M[1,] <- mean(feature_min, na.rm = TRUE)
  q <- quantile(feature_min,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_min_M[2,] <- q[1]
  local_min_M[3,] <- q[2]
  local_min_M[4,] <- sum(!is.na(feature_min)) / n
  
} else if (opt_mdlM == 4) {
  # If cubic, compute local max and min and inflection point
  feature_max <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = c('gamm4', 'zoo')
  ) %dopar% {
    ind_M = sample(1:nrow(tbl_sexM), round(sampling_pct * nrow(tbl_sexM)))
    return(get_max(tbl_sexM, ind_M))
  }
  local_max_M[1,] <- mean(feature_max, na.rm = TRUE)
  q <- quantile(feature_max,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_max_M[2,] <- q[1]
  local_max_M[3,] <- q[2]
  local_max_M[4,] <- sum(!is.na(feature_max)) / n
  
  feature_min <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = 'gamm4'
  ) %dopar% {
    ind_M = sample(1:nrow(tbl_sexM), round(sampling_pct * nrow(tbl_sexM)))
    return(get_min(tbl_sexM, ind_M))
  }
  local_min_M[1,] <- mean(feature_min, na.rm = TRUE)
  q <- quantile(feature_min,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  local_min_M[2,] <- q[1]
  local_min_M[3,] <- q[2]
  local_min_M[4,] <- sum(!is.na(feature_min)) / n
  
  feature_inf <- foreach(
    i = seq_len(n),
    .combine = 'c',
    .multicombine = TRUE,
    .packages = 'gamm4'
  ) %dopar% {
    ind_M = sample(1:nrow(tbl_sexM), round(sampling_pct * nrow(tbl_sexM)))
    return(get_inf(tbl_sexM, ind_M))
  }
  inflection_M[1,] <- mean(feature_inf, na.rm = TRUE)
  q <- quantile(feature_inf,
                probs = c(0.025, 0.975),
                na.rm = TRUE)
  inflection_M[2,] <- q[1]
  inflection_M[3,] <- q[2]
  inflection_M[4,] <- sum(!is.na(feature_inf)) / n
  
}
