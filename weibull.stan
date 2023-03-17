data {
    int<lower=0> N_obs;
    int<lower=0> N_cens;
    vector[N_obs] y_obs;
    vector[N_cens] y_cens;
}

parameters {
    real<lower=0> alpha;
    real<lower=0> sigma;
}

model {
    target += weibull_lpdf(y_obs | alpha, sigma);
    target += weibull_lccdf(y_cens | alpha, sigma);
    alpha ~ exponential(1);
    sigma ~ exponential(1);
}
