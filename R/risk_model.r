library(ape)
library(geiger)
library(stringr)
library(rstan)
library(arm)

# bring in IUCN red list
iucn <- read.csv('../data/export-61549.csv', stringsAsFactors = FALSE)
iucn <- iucn[iucn$Red.List.status != 'DD', ]
iucn$binom <- paste0(iucn$Genus, '_', iucn$Species)

# bring in phylogeny
tree <- read.nexus('../data/bininda_emonds.nex')

# make IUCN and phylogeny match
sect <- name.check(phy = tree[[1]], data.names = iucn$binom)
tree <- ape::drop.tip(tree[[1]], sect$tree_not_data)
iucn <- iucn[!(iucn$binom %in% sect$data_not_tree), ]

# bring in body size
pantheria <- read.delim('../data/pantheria.txt', stringsAsFactors = FALSE, sep = '\t')
pantheria$binom <- paste0(pantheria$MSW05_Genus, '_', pantheria$MSW05_Species)
mass <- 'X5.1_AdultBodyMass_g'
troph <- 'X6.2_TrophicLevel'
terr <- 'X12.2_Terrestriality'
home <- 'X22.1_HomeRange_km2'
pantheria <- pantheria[, c('binom', mass, troph, terr, home)]
names(pantheria) <- c('binom', 'mass', 'troph', 'terr', 'home')

raia <- read.csv('../data/raia_geb_mass.csv', stringsAsFactors = FALSE)
brook <- read.csv('../data/brook_bowman_2004.csv', stringsAsFactors = FALSE)

raia <- raia[!(raia$Species %in% brook$Name), ]
raia <- raia[, c('Species', 'log.mass')]
raia[, 2] <- exp(raia[, 2])
names(raia) <- c('binom', 'mass')

brook <- brook[, c('Name', 'Log10.mass..g.')]
brook$Log10.mass..g. <- 10^brook$Log10.mass..g.
names(brook) <- c('binom', 'mass')

other <- rbind(raia, brook)
other$binom <- str_replace(other$binom, '\\s', '_')
other$binom <- str_replace(other$binom, '\\s', '')

# bring it all together
other <- other[other$binom %in% pantheria$binom, ]
ord <- match(other$binom, pantheria$binom)
pantheria[ord, 'mass'] <- other$mass

pantheria <- pantheria[pantheria[, 2] > 0 & 
                       pantheria[, 3] > 0 & 
                       pantheria[, 4] > 0 &
                       pantheria[, 5] > 0, ]

# now cut the tree and iucn by this info
sect <- name.check(phy = tree, data.names = pantheria$binom)
tree <- ape::drop.tip(tree, sect$tree_not_data)
iucn <- iucn[!(iucn$binom %in% sect$tree_not_data), ]
pantheria <- pantheria[!(pantheria$binom %in% sect$data_not_tree), ]

# get the good data frame together
iucn <- cbind(iucn[, c(1:8, 18, 24)], pantheria[, -1])
sect <- 
tree <- drop.tip(tree, iucn$binom[iucn$Red.List.status %in% c('EW', 'EX')])
iucn <- iucn[!(iucn$Red.List.status %in% c('EW', 'EX')), ]

mass <- rescale(log(iucn$mass))
# herbivore is intercept
trop <- model.matrix( ~ as.character(iucn$troph) - 1)[, -1]
# ground is intercept
terr <- model.matrix( ~ as.character(iucn$terr) - 1)[, -1]
home <- rescale(log(iucn$home))

covariates <- cbind(mass, 
                    trop, 
                    terr, 
                    home,
                    home)

# response
risk.order <- c('LC', 'NT', 'VU', 'EN', 'CR') # from lowest risk to highest risk
response <- as.numeric(factor(iucn$Red.List.status, levels = risk.order))

# add in the phylogeny
vcv <- vcv(tree) / max(node.depth.edgelength(tree))
data <- list(N = length(response),
             K = length(risk.order),
             D = ncol(covariates),
             y = response,
             x = covariates,
             vcv = vcv)

# out put
with(data, {stan_rdump(list = c('N', 'K', 'D', 'y', 'x', 'vcv'),
                       file = '../data/data_dump/risk_info.data.R')})
