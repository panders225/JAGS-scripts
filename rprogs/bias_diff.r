
# jags example

setwd("/Users/Philip/books/bayes_stat/kruschke-doing_bayesian_data_analysis/2e/")
source("/Users/Philip/books/bayes_stat/kruschke-doing_bayesian_data_analysis/2e/Jags-Ydich-XnomSsubj-MbernBeta.R")

dset <- read.csv("/Users/Philip/books/bayes_stat/kruschke-doing_bayesian_data_analysis/2e/z6N8z2N7.csv")
y <- dset$y
s <- as.numeric(dset$s)

Ntotal <- length(y)
Nsubj <- length(unique(s))

d_list <- list(
  y=y
  , s=s
  , Ntotal=Ntotal
  , Nsubj=Nsubj
)

model {
  for ( in in 1:Ntotal) {
    y[i] ~ dbern(theta[s[i]])
  }
  for ( s in 1:Nsubj){
    theta[s] ~ dbeta(2,2)
  }
}

mcmcCoda <- genMCMC(data=dset, numSavedSteps=10000)
diagMCMC(mcmcCoda, parName="theta[1]")

smryMCMC(mcmcCoda, compVal = NULL, compValDiff = 0.0)




