shap_beeswarm_plotter <- function(data, transparency = 0.5){
  
  data |>
    ggplot(aes(jitter_feature, shap_value, color = feature_value)) +
    geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
    geom_point(
      position = "identity", size = 1.5, alpha = transparency, na.rm = TRUE
    ) +
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
      ,breaks =
        c(min(data$feature_value, na.rm = TRUE)
          , max(data$feature_value, na.rm = TRUE)
        )
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