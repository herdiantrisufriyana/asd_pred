shap_beeswarm_plot <-
  function(data, samp_size = 1, shap_iqr_width = 1.5, seed, transparency = 0.5){
    
    set.seed(seed)
    data <-
      data |>
      group_by(feature, feature_value) |>
      filter(seq %in% sample(unique(seq), ceiling(samp_size * n()), FALSE)) |>
      ungroup()
    
    data <-
      data |>
      filter(feature != "expected_value") |>
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
        , shap_lb = quantile(shap_value, 0.25) - shap_iqr_width * shap_iqr
        , shap_ub = quantile(shap_value, 0.75) + shap_iqr_width * shap_iqr
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
      mutate(shap_value_bin = round(shap_value * 100, 0) / 100) |>
      group_by(feature, shap_value_bin) |>
      mutate(freq = n()) |>
      ungroup()|>
      mutate(
        jitter_width = freq / max(freq) * 0.3
        , feature_num = as.numeric(feature)
        , jitter_feature = feature_num + runif(n(), -jitter_width, jitter_width)
      )
    
    data |>
      ggplot(aes(jitter_feature, shap_value, color = feature_value)) +
      geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
      geom_point(position = "identity", size = 1.5, alpha = transparency) +
      coord_flip() +
      scale_x_continuous(
        breaks = unique(select(data, feature, feature_num))$feature_num
        ,labels =
          paste0(
            unique(select(data, feature, feature_num))$feature
            , ' - '
            , max(data$feature_num)
              - unique(select(data, feature,feature_num))$feature_num
              + 1
          )
      ) +
      scale_color_gradient(
        "Feature value"
        ,low = "#008AFB"
        ,high = "#FF0053"
        ,breaks = c(min(data$feature_value), max(data$feature_value))
        ,labels = c("Low", "High")
      ) +
      theme_minimal() +
      xlab("") +
      ylab("SHAP value (impact on model output)") +
      theme(
        panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.ticks.x = element_line()
        , axis.line.x = element_line()
        , legend.position = "right"
        , legend.title = element_text(angle = 90, hjust = 0.5)
      ) +
      guides(
        color = 
          guide_colorbar(
            barwidth = 0.2
            , barheight = 10
            , title.position = "right"
            , title.hjust = 0.5
            , label.hjust = 0.5
            , ticks = FALSE
            , draw.ulim = TRUE
            , draw.llim = TRUE
          )
      )
  }