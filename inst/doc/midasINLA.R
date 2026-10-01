## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(midasINLA)
library(ggplot2)
library(dplyr)
library(tidyr)

has_INLA <- requireNamespace("INLA", quietly = TRUE)
if (has_INLA) {
  INLA::inla.setOption(num.threads = 1)
}

## ----load-data, eval = has_INLA-----------------------------------------------
data("data_spatialpoisson_example")

## ----inspect-data, eval = has_INLA--------------------------------------------
names(data_spatialpoisson_example)

## ----response-data, eval = has_INLA-------------------------------------------
head(data_spatialpoisson_example[["data_y"]])

## ----covariate-data, eval = has_INLA------------------------------------------
head(data_spatialpoisson_example$data_x1)
head(data_spatialpoisson_example$data_x2)

## ----graph, eval = has_INLA---------------------------------------------------
g <- INLA::inla.read.graph(
  filename = system.file("map.adj", package = "midasINLA")
)

## ----prepare-x1, eval = has_INLA----------------------------------------------
Midas_x1 <- prepare_Minla_spatial(
  x = data_spatialpoisson_example$data_x1$x1,
  loc_x = data_spatialpoisson_example$data_x1$loc,
  constraint = "hyperbolic",
  K = 0:29,
  m = 30,
  svc = TRUE,
  svc_prior = "icar",
  g = g
)

## ----prepare-x2, eval = has_INLA----------------------------------------------
Midas_x2 <- prepare_Minla_spatial(
  x = data_spatialpoisson_example$data_x2$x2,
  loc_x = data_spatialpoisson_example$data_x2$loc,
  constraint = "gaussian",
  K = 0:45,
  m = 30,
  svc = FALSE
)

## ----prepare-response, eval = has_INLA----------------------------------------
response_data <- data_spatialpoisson_example[["data_y"]]

response_data$y_all <- response_data$y

response_data[which(response_data[["Time"]] %in% 183:192),"y"] <- NA

## ----fit-model-code, eval=FALSE-----------------------------------------------
# fit_res <- fit_Minla_spatial(
#   formula = y ~ 1,
#   data = response_data,
#   loc_var = "loc",
#   time_var = "Time",
#   family = "poisson",
#   hf_input = list(Midas_x1, Midas_x2),
#   inla_options = list(
#     verbose = FALSE,
#     control.predictor = list(
#       compute = TRUE,
#       link = 1
#     )
#   )
# )

## ----model-summary, eval = FALSE----------------------------------------------
# summary(fit_res[["res"]])

## ----load-results, include = FALSE--------------------------------------------
result_file <- system.file(
  "extdata",
  "vignette_results.rds",
  package = "midasINLA"
)

stopifnot(nzchar(result_file))

vignette_results <- readRDS(result_file)

beta_results <- vignette_results$beta_results
res_weights <- vignette_results$res_weights

## ----beta-results, eval = FALSE-----------------------------------------------
# beta_results <- compute_beta_spatial(
#   model = fit_res,
#   n_loc = 16
# )

## ----beta-summary-------------------------------------------------------------
beta_results$hf_index_1$summary.icar.beta

## ----total-beta-summary-------------------------------------------------------
beta_results$hf_index_1$summary.total.beta

## ----constant-beta-summary----------------------------------------------------
beta_results$hf_index_2$summary.beta

## ----weights, eval = FALSE----------------------------------------------------
# res_weights <- compute_weights(fit_res)

## ----weights-x1---------------------------------------------------------------
head(res_weights$hf_1)

## ----weights-x2---------------------------------------------------------------
head(res_weights$hf_2)

## ----plot-weights, fig.width = 7, fig.height = 4.5----------------------------
ggplot(res_weights$hf_1, aes(x = lag, y = mean)) +
  geom_errorbar(
    aes(
      ymin = q2.5,
      ymax = q97.5
    ),
    width = 0.2,
    colour = "grey30"
  ) +
  geom_point(
    aes(
      colour = "Posterior mean"
    ),
    size = 2
  ) +
  geom_point(
    aes(
      y = data_spatialpoisson_example$weights1,
      colour = "True value"
    ),
    size = 2
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Posterior mean" = "red",
      "True value" = "blue"
    )
  ) +
  labs(
    x = "Lag",
    y = "Lag weight"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

## ----plot-weights-x2, fig.width = 7, fig.height = 4.5-------------------------
ggplot(res_weights$hf_2, aes(x = lag, y = mean)) +
  geom_errorbar(
    aes(
      ymin = q2.5,
      ymax = q97.5
    ),
    width = 0.2,
    colour = "grey30"
  ) +
  geom_point(
    aes(
      colour = "Posterior mean"
    ),
    size = 2
  ) +
  geom_point(
    aes(
      y = data_spatialpoisson_example$weights2,
      colour = "True value"
    ),
    size = 2
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Posterior mean" = "red",
      "True value" = "blue"
    )
  ) +
  labs(
    x = "Lag",
    y = "Lag weight"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

## ----load-prediction-results, include = FALSE---------------------------------
prediction_results <- readRDS(
  system.file(
    "extdata",
    "vignette_prediction.rds",
    package = "midasINLA"
  )
)

pred_res <- prediction_results$pred_res
fit_res <- prediction_results$fit_res

## ----prediction, eval = FALSE-------------------------------------------------
# pred_res <- predict_midas(
#   model = fit_res,
#   family = "poisson",
#   Ntrials = NULL,
#   nsamples = 1000
# )

## ----prediction-summary-------------------------------------------------------
head(pred_res$computed_y$mean)
head(pred_res$computed_y$q2.5)
head(pred_res$computed_y$q97.5)

## ----prediction-plot, fig.width = 7, fig.height = 6---------------------------
plot_data <- data.frame(
  observed = fit_res$data_final$y_all,
  predicted = pred_res$computed_y$mean,
  lower = pred_res$computed_y$q2.5,
  upper = pred_res$computed_y$q97.5,
  loc = fit_res$data_final$loc,
  Time = fit_res$data_final$Time
)

plot_long <- plot_data |>
  dplyr::filter(loc %in% 1:4) |>
  tidyr::pivot_longer(
    cols = c(observed, predicted),
    names_to = "series",
    values_to = "value"
  )

# Determine the training/held-out boundary for each location
non_na <- !is.na(fit_res$data_final$y)

segment <- cumsum(
  non_na != dplyr::lag(non_na, default = TRUE)
)
segment[!non_na] <- NA

rel_idx <- ave(
  seq_along(non_na),
  segment,
  FUN = seq_along
)

first_na <- which(
  diff(c(FALSE, is.na(fit_res$data_final$y))) == 1
)

vlines <- data.frame(
  cut = rel_idx[first_na - 1] + 1,
  loc = seq_len(16)
) |>
  dplyr::filter(loc %in% 1:4)

ggplot(plot_long, aes(x = Time, y = value, colour = series)) +
  geom_ribbon(
    data = plot_data |>
      dplyr::filter(loc %in% 1:4),
    aes(
      x = Time,
      ymin = lower,
      ymax = upper
    ),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.2
  ) +
  geom_line() +
  geom_vline(
    data = vlines,
    aes(xintercept = cut),
    colour = "black",
    linetype = "dashed"
  ) +
  facet_wrap(
    ~loc,
    ncol = 2,
    labeller = labeller(
      loc = function(x) paste("Loc =", x)
    )
  ) +
  scale_colour_manual(
    values = c(
      "observed" = "blue",
      "predicted" = "red"
    )
  ) +
  labs(
    x = "Time",
    y = "Outcome",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

