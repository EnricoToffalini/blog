# ============================================================
# Continuous norming (illustrativo) vs binning (0.5 anni)
# - outcome: accuracy 0..k (discreto bounded)
# - generatore: crescita log-like + random intercept soggetto (eterogeneità)
# - continuous: GLMM binomiale (glmer) con spline su età
#   CI95% analitico del valore atteso = k * E_u[ plogis( X(age)%*%beta + u ) ]
#   (cioè MEDIA MARGINALE, comparabile con le medie per bin)
# - P5: soglia discreta via mid-P:
#   * blu: P5 della mixture binomiale-logitnorm (approssimata Monte Carlo, continua su età)
#   * nero: P5 per bin (mid-P), con band ottenuta da CI su p del binom.test (illustrativo)
# - rosso: vero processo generativo (marginale su u) per MEDIA e P5
# ============================================================

set.seed(0)

library(ggplot2)
library(splines)
library(lme4)

# -------------------------
# 0) Simulazione dati (task matematica)
# -------------------------
N <- 600
k <- 20

d <- data.frame(
  id  = factor(seq_len(N)),
  age = runif(N, 7, 13)
)

# crescita "log-like"
c0 <- 6
A <- log(d$age - c0) / log(13 - c0)
A <- pmax(0, pmin(1, A))

# random intercept soggetto
sd_u <- 0.8
u <- rnorm(N, 0, sd_u)

# linear predictor del processo generativo
true_intercept = -1.2
true_slope = 2.5
eta_true <- (true_intercept + true_slope * A) + u
p_true <- plogis(eta_true)

d$accuracy <- rbinom(N, size = k, prob = p_true)

stopifnot(all(d$accuracy >= 0 & d$accuracy <= k))

# -------------------------
# 1) Helper: cutoff mid-P dato pmf su 0..k
# -------------------------
midP_cutoff_from_pmf <- function(pmf, alpha) {
  cdf_le <- cumsum(pmf)
  cdf_lt <- c(0, cdf_le[-length(cdf_le)])
  p_mid  <- cdf_lt + 0.5 * pmf
  j <- which(p_mid >= alpha)[1]
  if (is.na(j)) return(length(pmf) - 1L)
  j - 1L
}

# cutoff mid-P per Binom(k, p)
midP_cutoff_binom <- function(p, k, alpha = 0.05) {
  x <- 0:k
  pmf <- dbinom(x, size = k, prob = p)
  midP_cutoff_from_pmf(pmf, alpha)
}

# -------------------------
# 2) Binning (0.5 anni): nero
# -------------------------
breaks <- seq(7, 13, by = 0.5)
d$age_bin <- cut(d$age, breaks = breaks, include.lowest = TRUE, right = TRUE)

age_mid <- (breaks[-1] + breaks[-length(breaks)]) / 2
fmt <- function(x) sub("\\.0$", "", format(x, trim = TRUE))
bin_lab <- paste0("(", fmt(breaks[-length(breaks)]), ",", fmt(breaks[-1]), "]")

bin_list <- split(d$accuracy, d$age_bin)

# media per bin + CI95% (approssimazione normale)
bin_mean <- data.frame(
  age_mid = age_mid,
  age_lab = bin_lab,
  n = as.integer(sapply(bin_list, length)),
  mean = as.numeric(sapply(bin_list, mean)),
  sd = as.numeric(sapply(bin_list, sd))
)
bin_mean$se <- bin_mean$sd / sqrt(bin_mean$n)
bin_mean$lo <- pmax(0, bin_mean$mean - 1.96 * bin_mean$se)
bin_mean$hi <- pmin(k, bin_mean$mean + 1.96 * bin_mean$se)

# P5 per bin (mid-P) + band (da CI su p con binom.test, illustrativo)
bin_p5 <- data.frame(
  age_mid = age_mid,
  age_lab = bin_lab,
  p5    = NA_integer_
)

for (j in seq_along(bin_list)) {
  bin_p5$p5[j] = quantile(bin_list[[j]],0.05)
}

# -------------------------
# 3) Continuous norming: GLMM binomiale con random intercept
# -------------------------
dfs = 4
fit <- glmer(
  cbind(accuracy, k - accuracy) ~ ns(age, df = dfs) + (1 | id),
  family = binomial,
  data = d,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

age_grid <- seq(7, 13, by = 0.1)
newg <- data.frame(age = age_grid)

# ------------------------------------------------------------
# CI95% ANALITICO della MEDIA stimata (MARGINALE su random intercept)
# - ottengo eta_hat e se_eta sui FIXED
# - poi integro su u ~ N(0, sd_re^2) (Monte Carlo fisso)
#   mean(age) = k * E_u[plogis(eta_hat + u)]
# ------------------------------------------------------------
Xg <- model.matrix(~ ns(age, df = dfs), data = newg)

beta <- fixef(fit)
V <- as.matrix(vcov(fit))

eta_hat <- as.numeric(Xg %*% beta)
se_eta <- sqrt(pmax(0, diag(Xg %*% V %*% t(Xg))))

sd_re <- as.numeric(attr(VarCorr(fit)$id, "stddev"))

set.seed(777)
n_mc <- 5000
u_mc <- rnorm(n_mc, 0, sd_re)

p_marg <- vapply(eta_hat, function(e) mean(plogis(e + u_mc)), numeric(1))
p_marg_lo <- vapply(eta_hat - 1.96 * se_eta, function(e) mean(plogis(e + u_mc)), numeric(1))
p_marg_hi <- vapply(eta_hat + 1.96 * se_eta, function(e) mean(plogis(e + u_mc)), numeric(1))

grid_mean <- data.frame(
  age  = age_grid,
  mean = k * p_marg,
  lo   = k * p_marg_lo,
  hi   = k * p_marg_hi
)

# -------------------------
# 4) Continuous P5 (mid-P) della mixture logit-normal (Monte Carlo)
#    + band "illustrativo" propagando (eta_hat ± 1.96 se_eta)
# -------------------------
mix_pmf_from_eta <- function(eta0, u_mc, k) {
  p_u <- plogis(eta0 + u_mc)
  y_mc <- rbinom(length(u_mc), size = k, prob = p_u)
  tabulate(y_mc + 1L, nbins = k + 1L) / length(u_mc)
}

p5_hat <- integer(length(age_grid))
p5_lo  <- integer(length(age_grid))
p5_hi  <- integer(length(age_grid))

for (i in seq_along(age_grid)) {
  pmf_c <- mix_pmf_from_eta(eta_hat[i], u_mc, k)
  pmf_l <- mix_pmf_from_eta(eta_hat[i] - 1.96 * se_eta[i], u_mc, k)
  pmf_h <- mix_pmf_from_eta(eta_hat[i] + 1.96 * se_eta[i], u_mc, k)
  
  p5_hat[i] <- midP_cutoff_from_pmf(pmf_c, alpha = 0.05)
  p5_lo[i]  <- midP_cutoff_from_pmf(pmf_l, alpha = 0.05)
  p5_hi[i]  <- midP_cutoff_from_pmf(pmf_h, alpha = 0.05)
}

grid_p5 <- data.frame(
  age = age_grid,
  p5 = p5_hat,
  lo = p5_lo,
  hi = p5_hi
)

# oggetto che ti serve tra poco
p5_continuous <- grid_p5[, c("age", "p5")]

# -------------------------
# 5) Curva rossa: VERO processo generativo (marginale su u con sd_u)
# -------------------------
set.seed(999)
n_mc_true <- 6000
u_true <- rnorm(n_mc_true, 0, sd_u)

# ricalcolo A su griglia, identico al generatore
A_g <- log(age_grid - c0) / log(13 - c0)
A_g <- pmax(0, pmin(1, A_g))
eta0_true <- (true_intercept + true_slope * A_g)

# media vera marginale
p_true_mean <- vapply(eta0_true, function(e0) mean(plogis(e0 + u_true)), numeric(1))
true_mean_df <- data.frame(
  age = age_grid,
  mean_true = k * p_true_mean
)

# P5 vero (mid-P) della mixture generativa
p5_true <- integer(length(age_grid))
for (i in seq_along(age_grid)) {
  pmf <- mix_pmf_from_eta(eta0_true[i], u_true, k)
  p5_true[i] <- midP_cutoff_from_pmf(pmf, alpha = 0.05)
}
true_p5_df <- data.frame(
  age = age_grid,
  p5_true = p5_true
)

# -------------------------
# parametri grafici
lw = 1.2
bs = 24

# -------------------------
# 6) Grafico 1: media (continuous blu) vs binning nero + verità rossa
# -------------------------
ggplot(d, aes(x = age, y = accuracy)) +
  geom_point(alpha = 0.2, size = 2) +
  
  # nero: binning 0.5 anni
  geom_errorbar(
    data = bin_mean,
    aes(x = age_mid, ymin = lo, ymax = hi, color = "_Fasce età (0.5 anni)"),
    inherit.aes = FALSE, width = 0.18, linewidth = lw*0.8
  ) +
  geom_point(
    data = bin_mean,
    aes(x = age_mid, y = mean, color = "_Fasce età (0.5 anni)"),
    inherit.aes = FALSE, size = 5
  ) +
  geom_line(
    data = bin_mean,
    aes(x = age_mid, y = mean, color = "_Fasce età (0.5 anni)"),
    inherit.aes = FALSE, linewidth = lw*0.9
  ) +
  
  # blu: continuous (IC95% della media MARGINALE stimata)
  geom_ribbon(
    data = grid_mean,
    aes(x = age, ymin = lo, ymax = hi),
    inherit.aes = FALSE, alpha = 0.25, fill = "blue"
  ) +
  geom_line(
    data = grid_mean,
    aes(x = age, y = mean, color = "Continuous norming"),
    inherit.aes = FALSE, linewidth = lw*1.5
  ) +
  
  # rosso: vero processo generativo (media marginale)
  geom_line(
    data = true_mean_df,
    aes(x = age, y = mean_true, color = "Vero processo generativo"),
    inherit.aes = FALSE, linewidth = lw
  ) +
  
  scale_x_continuous(breaks = age_mid, labels = bin_lab, limits = c(7, 13)) +
  labs(x = "Età (anni)", y = "Valore atteso (medio)", color = NULL, fill = NULL) +
  
  # mappa colori "a mano" per avere nero/blu/rosso coerenti
  scale_color_manual(values = c(
    "_Fasce età (0.5 anni)"   = "black",
    "Continuous norming"    = "blue",
    "Vero processo generativo"     = "red"
  )) +
  
  # una sola legenda (evita duplicati e rende l'ordine prevedibile)
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 1)),
    fill  = guide_legend(override.aes = list(alpha = 0.25))
  ) +
  
  theme_bw(base_size = bs) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )


# -------------------------
# 7) Grafico 2: P5 (mid-P), continuous blu vs binning nero + verità rossa
# -------------------------
ggplot() +
  geom_point(data = d, aes(x = age, y = accuracy), alpha = 0.2, size = 2) +
  
  # nero: P5 per bin
  geom_point(
    data = bin_p5,
    aes(x = age_mid, y = p5, color = "_Fasce età (0.5 anni)"),
    inherit.aes = FALSE, size = 5
  ) +
  geom_line(
    data = bin_p5,
    aes(x = age_mid, y = p5, color = "_Fasce età (0.5 anni)"),
    inherit.aes = FALSE, linewidth = lw*0.8
  ) +
  
  # blu: P5 continuo mixture
  geom_line(
    data = grid_p5,
    aes(x = age, y = p5, color = "Continuous norming"),
    inherit.aes = FALSE, linewidth = lw*2
  ) +
  
  # rosso: vero P5 mixture generativa
  geom_line(
    data = true_p5_df,
    aes(x = age, y = p5_true, color = "Vero processo generativo"),
    inherit.aes = FALSE, linewidth = lw
  ) +
  
  scale_x_continuous(breaks = age_mid, labels = bin_lab, limits = c(7, 13)) +
  scale_y_continuous(limits = c(0, k)) +
  labs(x = "Età (anni)", y = "Stima del 5° percentile", color = NULL) +
  
  scale_color_manual(values = c(
    "_Fasce età (0.5 anni)"            = "black",
    "Continuous norming"     = "blue",
    "Vero processo generativo"      = "red"
  )) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  
  theme_bw(base_size = bs) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

