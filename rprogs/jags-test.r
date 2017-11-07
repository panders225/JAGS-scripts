

# start with the data 
myData <- read.csv("/Users/Philip/books/bayes_stat/kruschke-doing_bayesian_data_analysis/2e/z15N50.csv")
y <- myData$y
Ntotal <- length(y)
dataList <- list(
  y=y
  , Ntotal=Ntotal
)


# specify the model
modelString <- "
model {
  for ( i in 1:Ntotal) {
  # likelihood function
  y[i] ~ dbern(theta) 
  }
  # prior
  theta ~ dbeta(1, 1)
}
"
writeLines(modelString, con="/Users/Philip/books/bayes_stat/JAGS-scripts/txt_files/TEMPmodel.txt")

# use the MLE as the starting point for our MCMC chains
# generate a function for this exercise

initsList <- function() {
  resampledY <- sample(y, replace=T)
  thetaInit <- sum(resampledY)/length(resampledY) # extract bernoulli MLE
  thetaInit <- 0.001 + 0.998*thetaInit
  return(list(theta=thetaInit))
    }

# generate chains
# create the shell
jagsModel <- jags.model(
           file="/Users/Philip/books/bayes_stat/JAGS-scripts/txt_files/TEMPmodel.txt"
           , data=dataList
           , inits=initsList
           , n.chains=3
           , n.adapt=500
            )

# specify the burn-in
update(jagsModel, n.iter=500)

# specify chain length and the posterior estimate we want to hold on to
codaSamples <- coda.samples(jagsModel, variable.names=c("theta")
                            , n.iter=3334)

source("/Users/Philip/books/bayes_stat/kruschke-doing_bayesian_data_analysis/2e/DBDA2E-utilities.R")

diagMCMC(codaObject=codaSamples, parName="theta")

plotPost(codaSamples[, "theta"], main="theta", xlab=bquote(theta))


