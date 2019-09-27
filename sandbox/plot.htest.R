setMethod(
  "plot.htest",
  signature = "htest",
  definition = function(t_test) {

    stopifnot(identical(t_test$method, "Welch Two Sample t-test"))

    require(ggplot2)

    ci <- t_test$conf.int
    est <- t_test$estimate

    df <- data.frame(data_name = t_test$data.name,
                     estimate  = abs(diff(est)),
                     upper_ci  = max(ci),
                     lower_ci  = min(ci))

    ggplot() +
      geom_pointrange(
        aes(x = data_name, y = estimate,
            ymin = lower_ci, ymax = upper_ci), data = df) +
      geom_hline(aes(yintercept = 0)) +
      labs(x = NULL, y = NULL) +
      theme_bw()
  }
)
