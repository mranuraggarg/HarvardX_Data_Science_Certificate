library(gridExtra)
library(tidyverse)

X_hat <- replicate(100, {
    smpl <- c(sample(c(0, 1), prob=c(0.55, 0.45), replace=TRUE, size=1000))
    mean(smpl)
})

p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) %>%
    + geom_histogram(binwidth=0.005, color="black")

p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat)) %>% +
    stat_qq(dparams = list(mean=mean(X_hat), sd=sd(X_hat))) +
    geom_abline() +
    ylab("X_hat") +
    xlab("Theoritical normal")

grid.arrange(p1, p2, nrow=1)
