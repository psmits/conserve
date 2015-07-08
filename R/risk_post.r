library(rstan)
library(plyr)
library(ggplot2)
library(scales)
library(reshape2)
library(grid)
source('../R/waic.r')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 5),
             axis.title = element_text(size = 10),
             legend.text = element_text(size = 15),
             legend.title = element_text(size = 12),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 10))

# expectation graph
ordered.expected <- function(x, cuts) {
  k <- length(cuts) + 1
  out <- 0
  for(ii in seq(k)) {
    if(ii == 1) {
      out <- out + ii * invlogit(x - cuts[ii])
    } else if(ii > 1 && ii < k) {
      out <- out + ii * invlogit(x - cuts[ii - 1]) - invlogit(x - cuts[ii])
    } else if(ii == k) {
      out <- out + ii * invlogit(x - cuts[ii - 1]) - 0
    }
  }
  out
}
ordered.prob <- function(k, x, cuts) {
  out <- c()
  for(ii in seq(length(cuts) + 1)) {
    if(ii == 1) {
      out[ii] <- invlogit(x - cuts[ii])
    } else if(ii > 1 && ii < (length(cuts) + 1)) {
      out[ii] <- invlogit(x - cuts[ii - 1]) - invlogit(x - cuts[ii])
    } else if(ii == (length(cuts) + 1)) {
      out[ii] <- invlogit(x - cuts[ii - 1]) - 0
    }
  }
  out
}


# data
source('../R/risk_model.r')

fit <- list.files('../data/mcmc_out', 
                  pattern = 'status_base', 
                  full.names = TRUE)
fit.base <- read_stan_csv(fit)
post.base <- extract(fit.base, permuted = TRUE)


sim.table <- adply(post.base$y_new, 1, table)[, -1]
obs.table <- table(data$y)
post.test <- c()
for(ii in seq(length(obs.table))) {
  post.test[ii] <- sum(sim.table[, ii] < obs.table[ii]) / nrow(sim.table)
}

cut.quant <- adply(post.base$c, 2, function(x) quantile(x, c(0.1, 0.5, 0.9)))
