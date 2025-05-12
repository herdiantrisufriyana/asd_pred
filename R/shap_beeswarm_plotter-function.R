shap_beeswarm_plotter <-
  function(
    data, transparency = 0.5, text_size = 9, feature_rank_num = TRUE
    , color_palette = list("#008AFB", "#FF0053")
  ){
    
    if(feature_rank_num){
      feature_label <-
        paste0(
          unique(select(data, feature, feature_num))$feature
          , ' - '
          , max(data$feature_num)
          - unique(select(data, feature,feature_num))$feature_num
          + 1
        )
    }else{
      feature_label <-
        unique(select(data, feature, feature_num))$feature
    }
    
    data |>
      ggplot(aes(jitter_feature, shap_value, color = feature_value)) +
      geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
      geom_point(
        position = "identity", size = 1.5, alpha = transparency, na.rm = TRUE
      ) +
      coord_flip() +
      scale_x_continuous(
        breaks = unique(select(data, feature, feature_num))$feature_num
        ,labels = feature_label
      ) +
      scale_color_gradient(
        "Feature value"
        ,low = color_palette[[1]]
        ,high = color_palette[[2]]
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
        , axis.title.x = element_text(size = text_size)
        , axis.text.x =
          element_text(size = text_size)
        , axis.ticks.x = element_line()
        , axis.line.x = element_line()
        , axis.title.y = element_text(size = text_size)
        , axis.text.y = element_text(size = text_size)
        , legend.position = "right"
        , legend.title = element_text(size = text_size, angle = 90, hjust = 0.5)
        , legend.text = element_text(size = text_size)
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