library(tidyverse)

# c("#e3944f", "#297373", "#9a281d")
set.seed(1)
tibble(obsv=runif(1000) %>% sort(),
       pred=obsv + rnorm(1000, sd=0.33) * dnorm(seq(-2, 2, length.out=1000))) %>%
  ggplot() +
  aes(x=obsv, y=pred) +
  geom_point(alpha=0.5, size=0.5) +
  geom_abline(size=1, linetype="dashed", colour="#e3944f") +
  scale_y_continuous(name="Predicted", limits=0:1) +
  scale_x_continuous(name="Observed", limits=0:1) +
  coord_equal() +
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        text=element_text(size=20, family="Alegreya Sans"))
ggsave("../figures/example_fit.svg", width=4, height=4)

set.seed(8)
tibble(Feature=LETTERS[1:7],
       Importance=rlnorm(7)) %>%
  ggplot() +
  aes(x=Feature, y=Importance) +
  geom_bar(stat="Identity", fill="#9a281d") +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        text=element_text(size=20, family="Alegreya Sans"))
ggsave("../figures/example_imp_alt.svg", width=4, height=4)

set.seed(13)
tibble(Feature=LETTERS[1:7],
       Importance=rlnorm(7) %>% sort(decreasing=TRUE)) %>%
  ggplot() +
  aes(x=Feature, y=Importance) +
  geom_bar(stat="Identity", fill="#297373") +
  scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        text=element_text(size=20, family="Alegreya Sans"))
ggsave("../figures/example_imp.svg", width=4, height=4)
