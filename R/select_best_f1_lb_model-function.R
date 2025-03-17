select_best_f1_lb_model <- function(discrimination_obj_list){
  discrimination_obj_list |>
    imap(
      ~ .x$validation |>
        mutate(model = .y)
    ) |>
    reduce(rbind) |>
    filter(metric == "f1") |>
    filter(lb == max(lb)) |>
    slice(1) |>
    pull(model)
}