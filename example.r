library(rstan)
library(tidyverse)

# data ----
df <- readr::read_tsv("example-data.tsv")
y_obs <- df$time[df$status == 1]
y_cens <- df$time[df$status == 0]
data <- list(
  y_obs = y_obs,
  y_cens = y_cens,
  N_obs = length(y_obs),
  N_cens = length(y_cens)
)

# model ----
m <- rstan::stan_model("stan/weibull.stan")  # ~30 sec.
fit <- rstan::sampling(m, data = data)


weibull_survival <- function(time, alpha, sigma) {
  exp(-(time / sigma)^alpha)
}
survival_times <- seq(0, 50, 0.5)
posterior_samples <- rstan::extract(fit)
posterior <- map_df(
  survival_times,
  function(time) {
    surv = weibull_survival(time = time,
                            alpha = posterior_samples$alpha,
                            sigma = posterior_samples$sigma)
    
    tibble(time = time, surv = list(surv))
  }
) 

posterior %>% 
  mutate(
    x = map(surv, ~ quantile(.x, c(.025,.5, .975 )))
  ) %>% 
  unnest_wider(x) %>% 
  ggplot(aes(time, `50%`, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_ribbon(alpha = .4) +
  geom_line() +
  labs(x = "Survival time", y = "Survival probability") +
  theme_bw(base_size = 15)
