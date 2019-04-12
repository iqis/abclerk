require(pwr)
require(ggplot2)
require(scales)

n <- seq(250, 100000, by = 250)
p1 <- 0.2
uplift <- 0.06
p2 <- p1*(1 + uplift)

desired_power <- 0.8
sig <- 0.05

power <- sapply(
        n,
        function(.) pwr.2p.test(h = ES.h(p1 = p2, p2 = p1),
                                n = .,
                                sig.level = sig)$power
)

d <- data.frame(n, power)



necessary_n <- with(d,
     n[which(power > desired_power)[1]]
     )

plot_01 <- ggplot(d, aes(x = n, y = power)) +
        theme_minimal()+
        geom_line(size = 1.5, color = "deepskyblue3") +
        scale_y_continuous(name = NULL,
                           labels = percent
        ) +
        xlab("Visits") +
        geom_point(aes(x = necessary_n, y = desired_power),
                   color = "red", size = 2) +
        geom_label(aes(x = necessary_n, y = desired_power),
                   label  = paste0("Power: ", percent(desired_power), "\n",
                                   "Visits:", necessary_n
                        ),
                   position = position_nudge(x = 0.15 * max(n),
                                             y = -0.07 * max(power)
                   ))


conclusion <- paste("You need", necessary_n, "visitors to make sure that your A/B test has a", percent(desired_power), "probability to confirm if Plan B will produce a", percent(uplift), "uplift from Plan A's", percent(p1), "conversion rate to", percent(p2), ", due to less than", percent(sig), "random chance.")


# Echo
print(plot_01)
cat(conclusion)
