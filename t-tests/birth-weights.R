# Motivaing question:
# Is there an observable difference in baby weight for smokers / non-smokers?

# Install/library the `openintro package
# install.packages('openintro')
library(openintro)
library(dplyr)

# You should now be able to `View` the `births` dataset

View(births)
# ?births

# What are the mean birth weights for smokers/non-smokers
means <- births %>% group_by(smoke) %>% summarise(mean.weight = mean(weight))

# Store smokers/non-smokers in separate variables for easier access
smokers <- births %>% filter(smoke == 'smoker') %>% select(weight)
nonSmokers <- births %>% filter(smoke != 'smoker') %>% select(weight)

# Make an overlapping histogram of the birth weights of smokers/non-smokers
hist(nonSmokers$weight, col=rgb(0,0,1,.2))
hist(smokers$weight, add=TRUE,col=rgb(0,1,0,.2))

# Pre-t-test conceptaul question: is this data paired?
diff.means <- mean(nonSmokers$weight) - mean(smokers$weight)
se <- sqrt(
  sd(nonSmokers$weight)^2/nrow(nonSmokers) + 
  sd(smokers$weight)^2/nrow(smokers)
)

t.score <- diff.means/se
t <- 2.009

ci.lower <- diff.means - t * se
ci.upper <- diff.means + t * se

t.test(nonSmokers, smokers)

# Let's calculate the t value and confidence intervals manually


# Compute CIs using t threshold


# Implement a t-test to confirm our results




