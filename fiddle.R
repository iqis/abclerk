require(pwr)


n1 <- 80000
n2 <- 80000

x1 <- 1600
x2 <- 1700

p1 <-  x1/n1
p2 <-  x2/n2

delta <- (p2-p1)/p1

# effect size
h <-  ES.h(p1 = p2, p2 = p1)

test <- pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05, alternative = "greater")

power <- test$power




