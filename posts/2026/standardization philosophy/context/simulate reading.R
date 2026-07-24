# ------------------------------------------------------------
# Mini-batteria di decodifica: simulazione + plot essenziali
# - Task 1: lettura brano, output = velocita (sillabe/secondo), continuo, higher=better
# - Task 2: decisione lessicale time-limited, output = corrette su k, discreto, higher=better
# - Confondente: eta continua (6-13 anni)
#
# Nota di plausibilita (ordine di grandezza, uso didattico):
# taro la simulazione in modo che la mediana sia circa:
# - 6 anni: ~1.7-2.0 sill/sec
# - 13 anni: ~3.8-4.3 sill/sec
# con variabilita individuale realistica e senza "pile-up" al massimo.
# ------------------------------------------------------------

set.seed(1)

simulate_reading_minibattery <- function(N = 900,
                                         age_min = 6, age_max = 13,
                                         k_ld = 60,
                                         speed_bounds = c(0.6, 5.0)) {
  stopifnot(N > 10, age_min < age_max, k_ld >= 10)
  stopifnot(length(speed_bounds) == 2, speed_bounds[1] < speed_bounds[2])
  
  # Eta continua (anni), con densita un po' maggiore verso i piu piccoli
  age_y <- runif(N, age_min, age_max)
  
  # Tratto latente (non osservabile), utile solo per check didattici
  theta <- stats::rnorm(N, mean = 0, sd = 1)
  
  # Sviluppo con plateau: incremento rapido da piccoli, poi rallenta
  dev <- log(pmax(age_y - 5, 0.1))
  
  # -------------------------
  # Task 1: velocita lettura
  # -------------------------
  # Distribuzione Gamma (positiva, asimmetrica), con media dipendente da eta e theta
  mu_speed <- exp(log(1.15) + 0.60 * dev + 0.22 * theta)
  
  shape_speed <- 18
  speed_sps <- stats::rgamma(N, shape = shape_speed, scale = mu_speed / shape_speed)
  
  # Evito clamp secco che crea pile-up: faccio resampling dei valori fuori range
  lo <- speed_bounds[1]
  hi <- speed_bounds[2]
  
  idx_bad <- which(speed_sps < lo | speed_sps > hi)
  iter <- 0
  while (length(idx_bad) > 0 && iter < 50) {
    speed_sps[idx_bad] <- stats::rgamma(
      length(idx_bad),
      shape = shape_speed,
      scale = mu_speed[idx_bad] / shape_speed
    )
    idx_bad <- which(speed_sps < lo | speed_sps > hi)
    iter <- iter + 1
  }
  # fallback (quasi mai): clamp dolce
  speed_sps <- pmin(pmax(speed_sps, lo), hi)
  
  # --------------------------------
  # Task 2: decisione lessicale
  # --------------------------------
  u_ld <- stats::rnorm(N, mean = 0, sd = 0.55)
  
  eta_ld <- -0.55 + 0.75 * dev + 0.45 * theta + u_ld
  p_ld <- stats::plogis(eta_ld)
  
  ld_correct <- stats::rbinom(N, size = k_ld, prob = p_ld)
  
  data.frame(
    id = seq_len(N),
    age_y = age_y,
    speed_sps = speed_sps,
    ld_correct = ld_correct,
    k_ld = k_ld,
    theta = theta
  )
}

dat <- simulate_reading_minibattery()

# ------------------------------------------------------------
# Controlli rapidi di plausibilita (stampati)
# ------------------------------------------------------------
dat$age_int <- floor(dat$age_y)

# Funzione che ritorna un named vector (cosi aggregate produce una matrice, non una lista ambigua)
summ_vec <- function(x) {
  c(
    mean = mean(x),
    median = stats::median(x),
    sd = stats::sd(x),
    p10 = as.numeric(stats::quantile(x, .10, type = 8)),
    p90 = as.numeric(stats::quantile(x, .90, type = 8))
  )
}

speed_by_age <- aggregate(speed_sps ~ age_int, dat, FUN = summ_vec)

# Qui speed_by_age$speed_sps e' tipicamente una matrice con colonne mean/median/sd/p10/p90.
# La gestione sotto e' robusta sia se e' matrice sia se (in casi strani) fosse una lista.
if (is.matrix(speed_by_age$speed_sps)) {
  speed_by_age_out <- data.frame(
    age = speed_by_age$age_int,
    speed_by_age$speed_sps,
    row.names = NULL
  )
} else if (is.list(speed_by_age$speed_sps)) {
  speed_by_age_out <- data.frame(
    age = speed_by_age$age_int,
    do.call(rbind, speed_by_age$speed_sps),
    row.names = NULL
  )
} else {
  stop("Struttura inattesa di speed_by_age$speed_sps")
}

print(speed_by_age_out)

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------
library(ggplot2)

# Per evitare plot troppo densi, campiono punti per lo scatter
set.seed(123)
idx_scatter <- sample(seq_len(nrow(dat)), size = min(500, nrow(dat)))

# 1) Andamenti vs eta (scatter + loess)
p_speed_age <- ggplot(dat[idx_scatter, ], aes(age_y, speed_sps)) +
  geom_point(alpha = 0.30, size = 1) +
  geom_smooth(data = dat, se = FALSE, method = "loess", span = 0.9) +
  coord_cartesian(ylim = c(0.6, 5.0)) +
  labs(
    x = "Eta (anni)",
    y = "Velocita lettura (sillabe/secondo)",
    title = "Task 1: velocita di lettura vs eta"
  )

p_ld_age <- ggplot(dat[idx_scatter, ], aes(age_y, ld_correct)) +
  geom_point(alpha = 0.30, size = 1) +
  geom_smooth(data = dat, se = FALSE, method = "loess", span = 0.9) +
  labs(
    x = "Eta (anni)",
    y = paste0("Decisione lessicale: corrette (su ", unique(dat$k_ld), ")"),
    title = "Task 2: corrette vs eta"
  )

# 2) Distribuzioni marginali (istogrammi)
p_hist_speed <- ggplot(dat, aes(speed_sps)) +
  geom_histogram(bins = 35) +
  coord_cartesian(xlim = c(0.6, 5.0)) +
  labs(
    x = "Velocita lettura (sillabe/secondo)",
    y = "N",
    title = "Distribuzione: velocita lettura"
  )

p_hist_ld <- ggplot(dat, aes(ld_correct)) +
  geom_histogram(bins = 30) +
  labs(
    x = "Corrette (su k)",
    y = "N",
    title = "Distribuzione: decisione lessicale"
  )

# 3) Curve dei quantili empirici per eta (binning + quantili)
age_step <- 0.5
dat$age_bin <- round(dat$age_y / age_step) * age_step
probs <- c(.10, .25, .50, .75, .90)

quantile_by_agebin <- function(x, g, probs = c(.10, .25, .50, .75, .90)) {
  m <- do.call(
    rbind,
    lapply(split(x, g), function(v) {
      as.numeric(stats::quantile(v, probs = probs, na.rm = TRUE, type = 8))
    })
  )
  out <- data.frame(
    age_bin = as.numeric(rownames(m)),
    q10 = m[, 1],
    q25 = m[, 2],
    q50 = m[, 3],
    q75 = m[, 4],
    q90 = m[, 5],
    row.names = NULL
  )
  out[order(out$age_bin), ]
}

# Quantili per velocita
qs_speed <- quantile_by_agebin(dat$speed_sps, dat$age_bin, probs = probs)
qs_speed_long <- reshape(
  qs_speed,
  varying = c("q10", "q25", "q50", "q75", "q90"),
  v.names = "value",
  timevar = "quantile",
  times = c("10", "25", "50", "75", "90"),
  direction = "long"
)

p_q_speed <- ggplot(qs_speed_long, aes(age_bin, value, group = quantile)) +
  geom_line() +
  coord_cartesian(ylim = c(0.6, 5.0)) +
  labs(
    x = "Eta (anni, binning 0.5)",
    y = "Velocita (sillabe/secondo)",
    title = "Velocita lettura: curve dei quantili empirici (per eta)"
  )

# Quantili per corrette
qs_ld <- quantile_by_agebin(dat$ld_correct, dat$age_bin, probs = probs)
qs_ld_long <- reshape(
  qs_ld,
  varying = c("q10", "q25", "q50", "q75", "q90"),
  v.names = "value",
  timevar = "quantile",
  times = c("10", "25", "50", "75", "90"),
  direction = "long"
)

p_q_ld <- ggplot(qs_ld_long, aes(age_bin, value, group = quantile)) +
  geom_line() +
  labs(
    x = "Eta (anni, binning 0.5)",
    y = paste0("Corrette (su ", unique(dat$k_ld), ")"),
    title = "Decisione lessicale: curve dei quantili empirici (per eta)"
  )

# Stampa dei plot
p_speed_age
p_ld_age
p_hist_speed
p_hist_ld
p_q_speed
p_q_ld

######## EXPORT

dat$age = round(dat$age_y,2)
dat$syllsec = round(dat$speed_sps,2) 
dat$lessdec = dat$ld_correct
dat$lessdecTRIALS = dat$k_ld
write.csv(dat[,c("id","age","syllsec","lessdec","lessdecTRIALS")],"Mini-Deco_Sample.csv",row.names=F)

