require(magrittr)
require(pwr)
require(ggplot2)

nA <- 8000
nB <- 8020
xA <- 1600
xB <- 1700

n_factor <- seq(0.01, 2.5, by = 0.01)

p_value <- sapply(n_factor,
        function(.) {

                prop.test(
                        x = c(xB * ., xA * .),
                        n = c(nB * ., nA * .),
                        alternative = "greater",
                        conf.level = 0.95
                        )$`p.value`
                }
)

power <- sapply(n_factor,
              function(.) {
                        pwr.2p2n.test(ES.h(xB*./nB*., xA*./nA*.),
                                      n1 = nB*.,
                                      n2 = nA*.,
                                      sig.level = 0.05,
                                      alternative = "greater"
                                      )$power
              }
)


plot(n_factor, p_value,
     ylim = rev(range(0.0001, 0.15)),
     type = "l")
plot(n_factor, power,
     type = "l",
     ylim = range(0,1))

plot(power,p_value, ylim = rev(range(p_value)), type = "l")

d <- data.frame(
        n_factor = n_factor,
        power = power,
        p_value = p_value
        )
d %>% ggplot(aes(x = n_factor)) +
        theme_minimal() +
        geom_line(aes(y = power, color = "Power"),
                  size = 1.5) +
        geom_line(aes(y = 1 - p_value, color = "Confidence"),
                  size = 1.5) +
        xlab(NULL) +
        ylab(NULL) +
        scale_y_continuous(limits = c(0.75,1)) +
        theme(legend.title = element_blank(),
              legend.position = "left") +
        geom_vline(xintercept = 1, size = 1) +
        geom_hline(yintercept = 0.8, size = 1) +
        geom_hline(yintercept = .95, size = 1)


n_satisfy <- with(d,{
                c(power = n_factor[which(power > 0.8)[1]],
                p_value = n_factor[which(p_value < 0.05)[1]])
        })

(n_satisfy - 1) * nA
