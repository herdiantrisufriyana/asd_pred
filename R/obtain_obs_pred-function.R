obtain_obs_pred <-
  function(
      prefix
      , data_dir
      , model_dir
      , set = c("train", "validation", "test", "independent_test")
      , task = "classification"
    ){
    `names<-`(set, set) |>
      lapply(
        \(x)
        read_csv(
            paste0(data_dir, prefix, "_", x, ".csv")
            , show_col_types = F
          ) |>
          select(id, outcome) |>
          cbind(
            read_csv(
              paste0(
                model_dir, prefix, "/", x, "_"
                , ifelse(task == "classification", "prob", "predictions")
                , ".csv"
              )
              , show_col_types = F
            )
          ) |>
          `colnames<-`(c("id", "obs", "pred")) |>
          mutate_at("obs", as.factor)
      )
  }