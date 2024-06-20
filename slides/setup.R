# dev_pkgs <- c("countdown", "hadley/emo", "svglite")
# pak::pak(dev_pkgs)

#   ----------------------------------------------------------------------

hexes <- function(..., size = 64) {
  x <- c(...)
  x <- sort(unique(x), decreasing = TRUE)
  right <- (seq_along(x) - 1) * size

  res <- glue::glue(
    '![](hexes/<x>.png){.absolute top=-20 right=<right> width="<size>" height="<size * 1.16>"}',
    .open = "<", .close = ">"
  )

  paste0(res, collapse = " ")
}

knitr::opts_chunk$set(
  digits = 3,
  comment = "#>",
  dev = 'svglite'
)

# devtools::install_github("gadenbuie/countdown")
library(countdown)
library(ggplot2)
theme_set(theme_bw())
options(cli.width = 70, ggplot2.discrete.fill = c("#7e96d5", "#de6c4e"))

train_color <- "#1a162d"
test_color  <- "#cd4173"
data_color  <- "#767381"
assess_color <- "#84cae1"
splits_pal <- c(data_color, train_color, test_color)

# https://gist.github.com/topepo/778eff2590df87702e1c82c9ba09af7d
smooth_ph_linear_pred <- function(formula, data, deg_free = 5, grid_size = 500) {
  require(rlang)
  rlang::is_installed("survival")
  rlang::is_installed("ggplot2")
  rlang::is_installed("splines2")
  rlang::is_installed("cli")  
  require(ggplot2)

  # check 1 pred and continuous
  pred_sym <- rlang::f_rhs(formula)
  pred_var <- all.vars(pred_sym)
  if ( length(pred_var) != 1 ) {
    cli::abort("Only a single numeric predictor is supported.")
  }
  
  is_pred_num <- is.numeric(data[[pred_var]])
  if ( !is_pred_num ) {
    cli::abort("The predictor {.val pred_var} should be numeric.")
  }
  num_uniq <- length(unique(data[[pred_var]]))
  if ( num_uniq <= deg_free ) {
    cli::cli_abort("The predictor has {num_uniq} unique values; more are \\ 
                    needed to fit a spline with {deg_free} degrees of freedom.")
  }
  
  # make prop haz fit
  
  new_term <- rlang::call2("naturalSpline", .ns = "splines2", pred_sym, df = deg_free)

  model_form <- formula
  rlang::f_rhs(model_form) <- new_term
  ph_fit <- try(survival::coxph(model_form, data), silent = TRUE)
  if ( inherits(ph_fit, "try-error") ) {
    cli::cli_abort("The model fit failed with error {as.character(ph_fit)}.")
  }
  
  # plot grid

  pred_data <- sort(unique(rlang::eval_tidy(pred_sym, data)))
  
  # used for rug below
  pred_df <- data.frame(pred_data)
  names(pred_df) <- pred_var
  
  pred_rng <- range(pred_data)
  pred_grid <- seq(pred_rng[1], pred_rng[2], length.out = grid_size)
  pred_grid <- data.frame(x = pred_grid)
  names(pred_grid) <- pred_var
  
  pred_grid$linear_predictor <-
    predict(ph_fit,
            newdata = pred_grid,
            type = "lp",
            se.fit = FALSE) # TRUE appears to triggers a bug

  ggplot(pred_grid, aes(x = !!pred_sym)) +
    geom_line(aes(y = linear_predictor)) +
    geom_rug(data = pred_df) +
    labs(y = "Linear Predictor")
}