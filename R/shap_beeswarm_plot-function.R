shap_beeswarm_plot <-
  function(
    data, samp_size = 1, shap_iqr_width = 1.5, seed, transparency = 0.5
    , expected_value_name = "expected_value"
  ){
    
    set.seed(seed)
    data <-
      data |>
      group_by(feature, feature_value) |>
      filter(seq %in% sample(unique(seq), ceiling(samp_size * n()), FALSE)) |>
      ungroup()
    
    data <-
      data |>
      filter(feature != expected_value_name) |>
      group_by(feature) |>
      mutate(
        feature_value = rank(feature_value)
        , feature_value =
          (feature_value - min(feature_value))
        / (max(feature_value) - min(feature_value))
      ) |>
      ungroup() |>
      mutate(
        shap_iqr = quantile(shap_value, 0.75) - quantile(shap_value, 0.25)
        , shap_lb =
          quantile(shap_value, 0.25)
          - ifelse(shap_iqr == 0, shap_iqr_width, shap_iqr_width * shap_iqr)
        , shap_ub =
          quantile(shap_value, 0.75)
          + ifelse(shap_iqr == 0, shap_iqr_width, shap_iqr_width * shap_iqr)
      ) |>
      filter(shap_value >= shap_lb & shap_value <= shap_ub) |>
      select(-shap_iqr, -shap_lb, -shap_ub)
    
    data <-
      data |>
      group_by(feature) |>
      mutate(
        direction = mean(shap_value >= 0)
        , magnitude = max(shap_value)
      ) |>
      ungroup() |>
      mutate(
        magnitude =
          (magnitude - min(magnitude)) / (max(magnitude) - min(magnitude))
        ,impact =
          0.5 * direction + 0.5 * magnitude
      ) |>
      mutate(feature = reorder(feature, magnitude)) |>
      arrange(seq, feature)|>
      group_by(feature) |>
      mutate(
        shap_value_bin = rank(shap_value)
        , shap_value_bin =
          (shap_value_bin - min(shap_value_bin))
        / (max(shap_value_bin) - min(shap_value_bin))
        , shap_value_bin =
          round(shap_value_bin * 100, 0) / 100
      ) |>
      ungroup() |>
      group_by(feature, shap_value_bin) |>
      mutate(freq = n()) |>
      ungroup()|>
      mutate(
        jitter_width = 0.3 - (abs(shap_value_bin - 0.5) / 0.5 * 0.3)
        , feature_num = as.numeric(feature)
        , jitter_feature = feature_num + runif(freq, -jitter_width, jitter_width)
      )
    
    data |>
      shap_beeswarm_plotter(transparency)
  }