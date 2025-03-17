select_well_calibrated_model <- function(calibration_obj_list){
  calibration_obj_list |>
    imap(
      ~ .x$validation$metrics |>
        mutate(model = .y)
    ) |>
    reduce(rbind) |>
    mutate(lb = estimate - ci, ub = estimate + ci) |>
    filter(
      (term == "intercept" & lb <= 0 & ub >= 0)
      | (term == "slope" & lb <= 1 & ub >= 1)
    ) |>
    mutate(model = factor(model, unique(model))) |>
    group_by(model) |>
    summarize(n = n())|>
    filter(n == 2) |>
    pull(model) |>
    as.character()
}