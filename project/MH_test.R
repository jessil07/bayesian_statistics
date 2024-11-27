# need to convert data to a format we can work with 
data <- read.csv("project/foodpoisoning.csv") 
X.data <- as.matrix(cbind(1, data$age, data$beefcurry, data$eclair, data$sex, data$water)) # design matrix 
y.data <- data$case # get vector of response 

m1 <- glm(case~ age + beefcurry + eclair +sex + water, data=data, family= binomial(link="logit"))
beta.phat <- coef(m1) # extract model coefficients 


# function defining the inverse logit function
inv.logit <- function(x) {
  1 / (1 + exp(-x))
}


# function to evaluate the log conditional posterior density of beta.input
log.beta.post <- function(beta, X.data, y.data){

  lp <- X.data %*% beta
  
  log.posterior <- sum(y.data * log(inv.logit(lp) + 1e-9) + 
                         (1 - y.data) * log(1 - inv.logit(lp) + 1e-9))
  
  return(log.posterior)
}

beta.update <- function(beta.cur, sigma2, X.data, y.data) {
  
  beta.star <-  mvrnorm(n = 1, mu = beta.cur, 
                        Sigma = sigma2*diag(length(beta.cur)))
  
  log.R <- log.beta.post(beta.star, X.data, y.data) -
    log.beta.post(beta.cur, X.data, y.data)
  
  if (runif(1) <= min(1, exp(log.R))) {
    
    beta.new <- beta.star
    
  } else {
    
    beta.new <- beta.cur
    
  }
  return(beta.new)
}

metro <- function(n.sims, beta.start, sigma2, X.data, y.data){
  beta.draws <- matrix(NA, nrow = n.sims, ncol = length(beta.start))
  beta.cur <- beta.start
  
  for (i in 1:n.sims) {

    beta.new <- beta.update(beta.cur = beta.cur, sigma2=sigma2, X.data, y.data)
    
    # store result
    beta.draws[i, ] <- beta.new
    
    # update 
    beta.cur <- beta.new 
    
  }
  
  return(beta.draws)
  
}

test <- metro(n.sims = 100000, beta.start = beta.phat, sigma2 = 0.001, X.data=X.data, y.data=y.data)
testmc <- mcmc(test)
plot(testmc)

