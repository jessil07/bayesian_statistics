# File name: 1124_BA.R

# Written/ run on: RStudio Desktop 
# R version 4.3.3 (2024-02-29) -- "Angel Food Cake"

# Level M Bayesians Project

# Task 1 - Exploratory Data Analysis 
# Task 2 - Logistic Regression
# Jessie, September 2024 

################################################################################.
# Packages and functions ----
################################################################################.
library(dplyr) # pipes  
library(tidyr) # tidy data frames 
library(rlang)
library(MASS) # multivariate normal 
library(coda) # mcmc
library(ggplot2) # plots 
library(ggpubr) # to arrange plots 
library(sjPlot) # log odds plot
library(paletteer) # colour palette 

## functions 
my_colours <- paletteer::paletteer_d("lisa::AndyWarhol")

################################################################################.
# The data ----
################################################################################.
# read in data 
data <- read.csv("project/foodpoisoning.csv") %>%
  mutate_if(~ is.numeric(.) && all(unique(.) %in% c(0, 1)), factor)
# # change naming conventions 
# mutate(sex = ifelse(sex == 1, "male", "female"), 
#        beefcurry = ifelse(beefcurry == 1, "yes", "no"), 
#        water = ifelse(water == 1, "yes", "no"), 
#        case = ifelse(case == 1, "yes", "no")) %>%
# # convert to characters 
# mutate_if(sapply(., is.character), as.factor)

################################################################################.
# Exploratory Data Analysis ----
################################################################################.
# check for missing values 
data %>%
  summarise_all(~sum(is.na(.))) # there are no missing values 

# number of cases
n_fp <- data %>%
  filter(case == 1) %>%
  summarise(n = n(), n/nrow(data))

# number of non-cases
n_nfp <- data %>%
  filter(case == 0) %>%
  summarise(n = n(), n/nrow(data))

# summary statistics for continuous variables
numerical_summary <- data %>%
  dplyr::select(age, eclair) %>%
  summarise_each(funs(min = min, 
                      q25 = quantile(., 0.25), 
                      median = median, 
                      q75 = quantile(., 0.75), 
                      max = max, 
                      mean = mean, 
                      sd = sd))

# convert to tidy format 
numerical_summary_tidy <- numerical_summary %>%
  # turn columns names into entries in statistics column 
  # corresponding value is in value column
  gather(statistic, value) %>%
  # seperate the statistic entry into two columns 
  # one indicating the variable of interest, the other the statistic of interest
  separate(statistic, into = c("var", "stat"), sep = "_") %>%
  # statistic becomes the column names, the rows are each variable
  spread(stat, value) %>%
  dplyr::select(var, min, q25, median, q75, max, mean, sd) # reorder columns

# distribution of continuous variables by different case level 
age_box <- ggplot(data, aes(case, age)) +
  geom_boxplot(aes(fill = case)) +
  scale_fill_manual(values = my_colours[2:3]) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Did the subject suffer from food poisoning?", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12))

age_hist <- ggplot(data, aes(x = age, fill = case, colour = case)) +
  geom_histogram(position = "stack", alpha = 0.6, bins = 30) +
  scale_fill_manual(labels = c("No food poisoning", "Food poisoning"), 
                    values = my_colours[2:3]) +
  scale_colour_manual(labels = c("No food poisoning", "Food poisoning"), 
                      values = my_colours[2:3]) +
  labs(x = "Age", y = "Count", colour = "Case", fill = "Case") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12))

ggarrange(age_box, age_hist)
ggsave("project/output/eda/eda_age.png", width = 16, height = 8, units = "cm", bg = "white")

# distribution of continuous variables by different case level 
eclair_box <- ggplot(data, aes(case, eclair)) +
  geom_boxplot(aes(fill = case)) +
  scale_fill_manual(values = my_colours[2:3]) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Did the subject suffer from food poisoning?", y = "Number of eclairs eaten") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12))

eclair_hist <- ggplot(data, aes(x = eclair, fill = case, colour = case)) +
  geom_histogram(position = "stack", alpha = 0.6, bins = 13) +
  scale_fill_manual(labels = c("No food poisoning", "Food poisoning"), 
                    values = my_colours[2:3]) +
  scale_colour_manual(labels = c("No food poisoning", "Food poisoning"), 
                      values = my_colours[2:3]) +
  labs(x = "Number of eclairs eaten", y = "Count", colour = "Case", fill = "Case") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12))

ggarrange(eclair_box, eclair_hist)
ggsave("project/output/eda/eda_eclair.png", width = 16, height = 8, units = "cm", bg = "white")

# bar plots of factor variables 
# get factor variables from data set
factor_columns <- data[, sapply(data, is.factor)]
factor_names <- setdiff(colnames(factor_columns), "case")

# create a list to store proportions 
list_with_props <- list()

# calculate proportions of cases within each factor 
for (i in 1:length(factor_names)) {
  
  foi <- factor_names[i]
  
  # calculate proportions
  data_with_props <- data %>%
    group_by(case, !!sym(foi)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(case) %>%
    mutate(prop = count / sum(count) * 100)
  
  # save resuts 
  list_with_props[[foi]] <- data_with_props
}

# plot proportions
sex_bar <- ggplot(list_with_props$sex, aes(x = sex, y = prop, fill = sex)) +
  facet_wrap(~case, 
             labeller = as_labeller(c('0' = "No food poisoning", '1' = "Food poisoning"))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.1) +
  scale_fill_manual(values = my_colours[2:3]) +
  scale_x_discrete(labels = c("Female", "Male")) +
  labs(y = "Percentage of cases (%)", x = "", fill = "Sex") +
  theme_minimal() + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12)) +
  ylim(c(0, 100))

curry_bar <- ggplot(list_with_props$beefcurry, aes(x = beefcurry, y = prop, fill = beefcurry)) +
  facet_wrap(~case, 
             labeller = as_labeller(c('0' = "No food poisoning", '1' = "Food poisoning"))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.1) +
  scale_fill_manual(values = my_colours[2:3]) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(y = "Percentage of cases (%)", x = "Did the subject have beef curry?") +
  theme_minimal() + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20)) 

water_bar <- ggplot(list_with_props$water, aes(x = water, y = prop, fill = water)) +
  facet_wrap(~case, 
             labeller = as_labeller(c('0' = "No food poisoning", '1' = "Food poisoning"))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.1) +
  scale_fill_manual(values = my_colours[2:3]) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(y = "Percentage of cases (%)", x = "Did the subject have water?") +
  theme_minimal() + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 10),  # increase facet label size
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20)) 

ggarrange(sex_bar, curry_bar, water_bar, ncol = 1)
ggsave("project/output/eda/eda_sex.png", plot = sex_bar, width = 16, height = 8, units = "cm", bg = "white")
ggsave("project/output/eda/eda_curry.png", plot = curry_bar, width = 16, height = 8, units = "cm", bg = "white")
ggsave("project/output/eda/eda_water.png", plot = water_bar, width = 16, height = 8, units = "cm", bg = "white")

################################################################################.
# Logistic Regression ----
################################################################################.
# need to check for correlation between numerical variables  
cor(data$age, data$eclair) # this seems fine 

### model selection ----
# need to select predictors

# without standardising age
# start with full main effects model
m1 <- glm(case~age + beefcurry + eclair + sex + water, data=data, family= binomial(link="logit"))
summary(m1) # as expected from the eda, beef curry and water are not significant predictors

# stepwise model selection using aic
step(m1)

# remove beef curry based on above
m2 <- glm(case~age + eclair + sex + water, data=data, family= binomial(link="logit"))
summary(m2) # water is just significant

# calculate deviance explained
m2_deviance_explained <- (m2$null.deviance - m2$deviance) / m2$null.deviance * 100

# refit above model without water and consider some comparison metrics
m3 <- glm(case~sex + age + eclair, data=data, family= binomial(link="logit"))
summary(m3)

# calculate deviance explained
m3_deviance_explained <- (m3$null.deviance - m3$deviance) / m3$null.deviance * 100

# compare models
model_comparison <- cbind(model = c("model with water", "model without water"),
                          aic = c(AIC(m2), AIC(m3)),
                          bic = c(BIC(m2), BIC(m3)),
                          deviance.expl = c(m2_deviance_explained, m3_deviance_explained))
model_comparison # the complex model outperforms the simpler model in terms of aic and deviance explained
# the simpler model is preferred when considering the bic

# conduct a chi-square test
anova(m3, m2, test = "Chisq")
# the simpler model significantly improves the fit over the simpler model
# move forward with more complex model, i.e., model with water predictor

### assumptions ----
# plot the raw residuals against the fitted values to check the independence of residuals assumption.
ggplot(data.frame(fitted=predict(m2, type = "response"),
                  residuals=residuals(m2, type = "response")),
       aes(x = fitted,y = residuals)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Fitted values", y = "Raw residuals")

plot(m2)

# assumptions do not hold... 

### interpretation ----
summary(m2)

plot_model(m2, show.values = FALSE,
           axis.labels = c("Age", "Eclair", "Sex = Male", "Water = Yes")) +
  # adjust text label 
  geom_text(aes(label = round(..y.., 2)), nudge_x = 0.3) + 
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  labs(title = "")

ggsave("project/output/logodds.png", plot = last_plot(), width = 16, height = 8, units = "cm", bg = "white")

# standardise age 
# data <- data %>%
#   # standardise age 
#   mutate(std.age = scale(age))
#
### model selection ----
# need to select predictors 
#
# # start with full main effects model
# m1 <- glm(case~sex + std.age + beefcurry + eclair + water, data=data, family= binomial(link="logit"))
# summary(m1) # as expected from the eda, beef curry and water are not significant predictors 
# 
# # stepwise model selection using aic
# step(m1) 
# 
# # remove beef curry based on above 
# m2 <- glm(case~sex + std.age + eclair + water, data=data, family= binomial(link="logit"))
# summary(m2) # water is just significant 
# 
# # calculate deviance explained
# m2_deviance_explained <- (m2$null.deviance - m2$deviance) / m2$null.deviance * 100
# 
# # refit above model without water and consider some comparison metrics 
# m3 <- glm(case~sex + std.age + eclair, data=data, family= binomial(link="logit"))
# summary(m3)
# 
# # calculate deviance explained
# m3_deviance_explained <- (m3$null.deviance - m3$deviance) / m3$null.deviance * 100
# 
# # compare models 
# model_comparison <- cbind(model = c("model with water", "model without water"), 
#                           aic = c(AIC(m2), AIC(m3)), 
#                           bic = c(BIC(m2), BIC(m3)), 
#                           deviance.expl = c(m2_deviance_explained, m3_deviance_explained))
# model_comparison # the complex model outperforms the simpler model in terms of aic and deviance explained
# # the simpler model is preferred when considering the bic  
# 
# # conduct a chi-square test 
# anova(m3, m2, test = "Chisq")
# # the simpler model significantly improves the fit over the simpler model 
# # move forward with more complex model, i.e., model with water predictor 
# 
# ### assumptions 
# # plot the raw residuals against the fitted values to check the independence of residuals assumption.
# ggplot(data.frame(fitted=predict(m2, type = "response"),
#                   residuals=residuals(m2, type = "response")),
#        aes(x = fitted,y = residuals)) +
#   geom_point() +
#   geom_smooth() +
#   labs(x = "Fitted values", y = "Raw residuals")
# 
# ### interpretation 
# summary(m2)
# 
# plot_model(m2, show.values = TRUE, 
#            axis.labels = c("Sex = Male", "Eclair", "Age", "Water = Yes")) +
#   theme_minimal() +
#   theme(legend.position = "none", 
#         strip.text = element_text(size = 10),  
#         axis.text = element_text(size = 10), 
#         axis.title = element_text(size = 12)) +
#   labs(title = "Effect on Odds of Food Poisoning")
# # age reduces the odds 

################################################################################.
# Metropolis Hastings ----
################################################################################.
# set seed 
set.seed(11271432)

### MH algorithm ----
# function defining the inverse logit function
inv.logit <- function(x) {
  1 / (1 + exp(-x))
}

# function to evaluate the log conditional posterior density of beta.input
beta.post <- function(beta.input, X.data, y.data){
  # evaluate posterior for beta 
  # X.data is the design matrix, including intercept 
  # y.data is the (0/1) vector indicating if the outcome of interest 
  # occurred (food poisoning)
  
  # check that vector of parameters has length equal to number of columns 
  # in design matrix 
  if(length(beta.input) != ncol(X.data)){
    stop("The number of parameters must match the number of columns 
    in the design matrix")
  }
  
  # obtain linear predictor 
  lp <- X.data %*% beta.input
  
  # compute the log posterior—log of equation (2.2)
  # use the log to improve computational efficiency 
  # add 1e-9 to avoid log(0)
  log.posterior <- sum(y.data * log(inv.logit(lp) + 1e-9) + 
                         (1 - y.data) * log(1 - inv.logit(lp) + 1e-9))
  
  # output 
  return(log.posterior)
}

# function for metropolis sampler 
beta.sampler <- function(X.data, y.data, beta.phat, sigma2, n.sims = 100000){
  # beta.phat is the vector containing the parameter estimates from logistic reg
  # sigma2 is the variance of the proposal density, i.e., N(beta.phat, sigma2I)
  # n.sims is the number of times to run the sampler, adjust as necessary 
  
  # calculate total number of accepted samples 
  acceptance.count <- 0
  
  # valid variance input
  if (sigma2 <= 0) stop("Proposal variance sigma2 must be positive")
  
  # create matrix to store beta samples from MH 
  # each row will be an iteration, each column corresponds to a parameter 
  beta.draws <- matrix(NA, nrow = n.sims+1, ncol = length(beta.phat))
  beta.draws[1, ] <- beta.phat # starting values 
  
  # list to store debugging information
  debug_info <- list(
    beta_cur = vector("list", n.sims),
    beta_star = vector("list", n.sims),
    log_R = vector("numeric", n.sims),
    R = vector("numeric", n.sims), 
    acceptance.count = vector("numeric", n.sims)
  )
  
  # run the algorithm n.sims times, i.e., get n.sims samples 
  # start at two as first row in beta.draw is the vector of starting value
  for (i in 2:(n.sims+1)) {
    
    # set the current sample  
    beta.cur <- beta.draws[i - 1, ] 
    
    # propose new sample from jump density beta.star 
    beta.star <-  mvrnorm(n = 1, mu = beta.cur, 
                          Sigma = sigma2*diag(length(beta.cur)))
    
    # calculate acceptance ratio 
    log.R <- beta.post(beta.star, X.data, y.data) - 
      beta.post(beta.cur, X.data, y.data)
    # convert back to original scale using expo transformation
    R <- exp(log.R)
    
    # accept-reject stage
    # generate uniform distribution
    if (runif(1) <= min(1, R)) {
      # accept new sample
      beta.new <- beta.star
      accept.update <- 1
    } else {
      # reject new sample and recycle current sample
      beta.new <- beta.cur
      accept.update <- 0
    }
    # store the new sample
    beta.draws[i, ] <- beta.new
    acceptance.count <- acceptance.count + accept.update
    
    # keep debugging information
    debug_info$beta_cur[[i]] <- beta.cur
    debug_info$beta_star[[i]] <- beta.star
    debug_info$log_R[i] <- log.R
    debug_info$R[i] <- R
    debug_info$acceptance.count[i] <- acceptance.count
  }
  
  # remove starting values from samples matrix 
  beta.draws <- beta.draws[-1, ]
  
  # Return the samples, debugging information, and total 
  # number of accepted samples
  return(list(beta_draws = beta.draws, 
              debug_info = debug_info, 
              acceptance.count = acceptance.count))
}

# use parameter estimates from logistic reg 
# choose the full model, i.e., model with all predictors included so we have an 
# estimate for each of our parameters
m1 <- glm(case~ age + beefcurry + eclair +sex + water, data=data, family= binomial(link="logit"))
my.beta.phat <- coef(m1) # extract model coefficients 

# need to convert data to a format we can work with 
data <- read.csv("project/foodpoisoning.csv") 
my.X <- as.matrix(cbind(1, scale(data$age), data$beefcurry, 
                        scale(data$eclair), data$sex, data$water)) # design matrix 
my.y <- data$case # get vector of response 

# MH draws
# set starting choice for sigma
sigma2.start <- round(var(log(my.y + 0.5)), 1) # (slide 8, lecture 8)

# run sampler 
beta.sampler.output <- beta.sampler(X.data = my.X, y.data = my.y, 
                                    my.beta.phat, sigma2 = sigma2.start) 
beta.draws <- beta.sampler.output$beta_draws # get draws 

# convert to mcmc object, 10% burn n 
post.beta <- mcmc(beta.draws, start = 10000, end = 100000) 
plot(post.beta, col = "#F588AFFF") # create plot 

effectiveSize(post.beta)

### different values of sigma^2 ----
# different sigma^2
diff.sigma2 <- c(sigma2.start, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 10)

# create list to store mcmc objects 
all.post.beta <- list()

# loop for info
for (i in 1:length(diff.sigma2)) {
  
  # select sigma2 of interest 
  sigma2.oi <- diff.sigma2[i]
  
  # measure runtime for beta.sampler()
  runtime <- system.time({
    beta.sampler.output <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = sigma2.oi)
  })
  
  # get draws 
  beta.draws <- beta.sampler.output$beta_draws
  
  # convert to mcmc object 
  post.beta <- mcmc(beta.draws, start = 10000, end = 100000)
  
  # print effect sample size and number of accepted samples
  cat(sprintf(
    "Variance: %f | Effective Sizes: %s | Accepted Samples: %d\n",
    sigma2.oi, paste(round(effectiveSize(post.beta), 4), collapse = ", "), beta.sampler.output$acceptance.count))
  
  # print runtime information
  cat(sprintf("Variance: %f | Runtime: %.2f seconds\n", sigma2.oi, runtime["elapsed"]))
  
  print(summary(post.beta))

  # store draws in list
  all.post.beta[[i]] <- post.beta
}


for (i in 1:length(diff.sigma2)) {
  
  # get beta samples 
  post.beta <- all.post.beta[[i]]
  
  # column names 
  colnames(post.beta) <- c("intercept", "age", "beefcurry", "eclair", "sex", "water")
  
  # select sigma2 of interest 
  sigma2.oi <- diff.sigma2[i]
  
  # convert sigma2.oi to a format without dots
  sigma2.oi.cleaned <- gsub("\\.", "", as.character(sigma2.oi))  # remove dots
  
  # save first plot
  first_name <- paste0("project/output/traceplots/", sigma2.oi.cleaned, "_first_tp.png")
  png(first_name, width = 16, height = 13, units = "cm", res = 300)  # open device
  par(mar = c(3, 3, 1, 1))  # set margins
  plot(post.beta[, 1:3], col = "#F588AFFF")  
  dev.off()  # close device
  
  # save second plot
  second_name <- paste0("project/output/traceplots/", sigma2.oi.cleaned, "_second_tp.png")
  plot(post.beta, col = "#F588AFFF")
  png(second_name, width = 16, height = 13, units = "cm", res = 300)
  par(mar = c(3, 3, 1, 1))  
  plot(post.beta, col = "#F588AFFF")
  dev.off()
  
  # save acf plot 
  acf_name <- paste0("project/output/acf/", sigma2.oi.cleaned, "_acf.png")
  png(acf_name, width = 16, height = 13, units = "cm", res = 300)
  par(mar = c(4, 4, 2, 1))
  autocorr.plot(post.beta)
  dev.off()
}


# ### different values of sigma^2 ----
# # using default sigma^2 = 0.1
# post.beta <- mcmc(beta.draws, start = 1000, end = 100000) # convert to mcmc object, burn-in
# # png("project/output/0100_sigma/0100_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  # reduce margins
# plot(post.beta)
# autocorr.plot(post.beta)
# effectiveSize(post.beta)
# summary(post.beta)
# 
# # # save plots 
# # png("project/output/0100_sigma/0100_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1)) 
# # plot(post.beta)
# dev.off()
# # png("project/output/0100_sigma/0100_acf.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  
# # autocorr.plot(post.beta)
# # dev.off()
# 
# beta.sampler.output <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 5)
# post.beta <- mcmc(beta.sampler.output$beta_draws, start =1000, end = 100000) 
# plot(post.beta)
# 
# # different sigma^2
# diff.sigma2 <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10)
# 
# # convergence diagnosis plots
# beta.sampler.output.smallest <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 0.001)
# beta.draws.smallest <- beta.sampler.output.smallest$beta_draws
# post.beta.smallest <- mcmc(beta.draws.smallest, start = 5000, end = 50000)
# plot(post.beta.smallest)
# autocorr.plot(post.beta.smallest)
# 
# # # save plots
# # png("project/output/0001_sigma/0001_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  # reduce margins
# # plot(post.beta.smallest)
# # png("project/output/0001_sigma/0001_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1)) 
# # plot(post.beta.smallest)
# # dev.off()
# # png("project/output/0001_sigma/0001_acf.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  
# # autocorr.plot(post.beta.smallest)
# # dev.off()
# 
# beta.sampler.output.smaller <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 0.01)
# beta.draws.smaller <- beta.sampler.output.smaller$beta_draws
# post.beta.smaller <- mcmc(beta.draws.smaller, start = 5000, end = 50000)
# plot(post.beta.smaller)
# autocorr.plot(post.beta.smaller)
# 
# # # save plots
# # png("project/output/0010_sigma/0010_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  # reduce margins
# # plot(post.beta.smaller)
# # png("project/output/0010_sigma/0010_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # plot(post.beta.smaller)
# # dev.off()
# # png("project/output/0010_sigma/0010_acf.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # autocorr.plot(post.beta.smaller)
# # dev.off()
# 
# beta.sampler.output.small <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 0.05)
# beta.draws.small <- beta.sampler.output.small$beta_draws
# post.beta.small <- mcmc(beta.draws.small, start = 5000, end = 50000)
# plot(post.beta.small)
# autocorr.plot(post.beta.small)
# 
# # # save plots
# # png("project/output/0050_sigma/0050_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  # reduce margins
# # plot(post.beta.small)
# # png("project/output/0050_sigma/0050_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # plot(post.beta.small)
# # dev.off()
# # png("project/output/0050_sigma/0050_acf.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # autocorr.plot(post.beta.small)
# # dev.off()
# 
# beta.sampler.output.large <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 0.5)
# beta.draws.large <- beta.sampler.output.large$beta_draws
# post.beta.large <- mcmc(beta.draws.large, start = 5000, end = 50000)
# plot(post.beta.large)
# autocorr.plot(post.beta.large)
# 
# # save plots
# png("project/output/0500_sigma/0500_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# par(mar = c(4, 4, 2, 1))  # reduce margins
# plot(post.beta.large)
# png("project/output/0500_sigma/0500_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# par(mar = c(4, 4, 2, 1))
# plot(post.beta.large)
# dev.off()
# png("project/output/0500_sigma/0500_acf.png", width = 16, height = 8, units = "cm", res = 300)
# par(mar = c(4, 4, 2, 1))
# autocorr.plot(post.beta.large)
# dev.off()
# 
# beta.sampler.output.larger <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 1)
# beta.draws.larger <- beta.sampler.output.larger$beta_draws
# post.beta.larger <- mcmc(beta.draws.larger, start = 5000, end = 50000)
# plot(post.beta.larger)
# autocorr.plot(post.beta.larger)
# 
# # # save plots
# # png("project/output/1000_sigma/1000_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  # reduce margins
# # plot(post.beta.larger)
# # png("project/output/1000_sigma/1000_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # plot(post.beta.larger)
# # dev.off()
# # png("project/output/1000_sigma/1000_acf.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # autocorr.plot(post.beta.larger)
# # dev.off()
# 
# beta.sampler.output.largest <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 5)
# beta.draws.largest <- beta.sampler.output.largest$beta_draws
# post.beta.largest <- mcmc(beta.draws.largest, start = 5000, end = 50000)
# plot(post.beta.largest)
# autocorr.plot(post.beta.largest)
# 
# # # save plots
# # png("project/output/5000_sigma/5000_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))  # reduce margins
# # plot(post.beta.largest)
# # png("project/output/5000_sigma/5000_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # plot(post.beta.largest)
# # dev.off()
# # png("project/output/5000_sigma/5000_acf.png", width = 16, height = 8, units = "cm", res = 300)
# # par(mar = c(4, 4, 2, 1))
# # autocorr.plot(post.beta.largest)
# # dev.off()
# 
# beta.sampler.output.extreme <- beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = 10)
# beta.draws.extreme <- beta.sampler.output.extreme$beta_draws
# post.beta.extreme <- mcmc(beta.draws.extreme, start = 5000, end = 50000)
# plot(post.beta.extreme)
# autocorr.plot(post.beta.extreme)
# 
# # save plots
# png("project/output/10000_sigma/10000_beta_mix1.png", width = 16, height = 8, units = "cm", res = 300)
# par(mar = c(4, 4, 2, 1))  # reduce margins
# plot(post.beta.extreme)
# png("project/output/10000_sigma/10000_beta_mix2.png", width = 16, height = 8, units = "cm", res = 300)
# par(mar = c(4, 4, 2, 1))
# plot(post.beta.extreme)
# dev.off()
# png("project/output/10000_sigma/10000_acf.png", width = 16, height = 8, units = "cm", res = 300)
# par(mar = c(4, 4, 2, 1))
# autocorr.plot(post.beta.extreme)
# dev.off()
# 
# # number of accepted samples for each different sigma^2
# accepted.count.sigma2 <- cbind(sigma = diff.sigma2, 
#                                count = c(beta.sampler.output.smallest$acceptance.count, 
#                                          beta.sampler.output.smaller$acceptance.count, 
#                                          beta.sampler.output.small$acceptance.count, 
#                                          beta.sampler.output$acceptance.count, 
#                                          beta.sampler.output.large$acceptance.count, 
#                                          beta.sampler.output.larger$acceptance.count,
#                                          beta.sampler.output.largest$acceptance.count,
#                                          beta.sampler.output.extreme$acceptance.count))
# 
# # effective sample size 
# # matrix to store ess for each different sigma^2
# ess.sigma2 <- cbind(sigma = diff.sigma2, 
#                     ess = rbind(effectiveSize(post.beta.smallest), 
#                             effectiveSize(post.beta.smaller), 
#                             effectiveSize(post.beta.small), 
#                             effectiveSize(post.beta), 
#                             effectiveSize(post.beta.large), 
#                             effectiveSize(post.beta.larger), 
#                             effectiveSize(post.beta.largest), 
#                             effectiveSize(post.beta.extreme))) %>%
#   as_tibble() %>%
#   mutate_all(funs(round(., 4)))
# 
# # run times 
# time.taken <- matrix(NA, nrow = length(diff.sigma2), ncol = 1)
# 
# for (i in 1:length(diff.sigma2)) {
#   # select a sigma^2 of interest 
#   sigma.oi <- diff.sigma2[i]
#   
#   # get system times 
#   times <- system.time(beta.sampler(X.data = my.X, y.data = my.y, my.beta.phat, sigma2 = sigma.oi))
#   # get time it took to execute the above  
#   time.elapsed <- as.vector(times)[3]
#   
#   # store  
#   time.taken[i, 1] <- time.elapsed
# }
# 

