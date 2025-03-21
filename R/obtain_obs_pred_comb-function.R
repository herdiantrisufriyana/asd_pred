obtain_obs_pred_comb <- function(data, protocol){
  comb_model <- list()
  model_name <- unique(protocol$model)
  for(i in model_name){
    
    current_model <- filter(protocol, model == i)
    
    for(j in sort(unique(current_model$step))){
      current_step <- filter(current_model, step == j)
      
      if(j == 1 & j == max(current_model$step)){
        current_rule_in <- str_split_fixed(current_step$value, "\\=", 2)[1, ]
        
        results <-
          data |>
          rename_at(current_rule_in[1], \(x) "current_param") |>
          mutate(pred = current_param) |>
          rename_at("current_param", \(x) current_rule_in[1]) |>
          mutate(step = current_rule_in[1])
      }else if(j == 1  & j < max(current_model$step)){
        current_rule_in <- str_split_fixed(current_step$value, "\\=", 2)[1, ]
        
        next_step <- filter(current_model, step == j + 1)
        next_rule_in <- str_split_fixed(next_step$value, "\\=", 2)[1, ]
        
        results <-
          data |>
          rename_at(current_rule_in[1], \(x) "current_param") |>
          rename_at(next_rule_in[1], \(x) "next_param") |>
          mutate_at(c("current_param", "next_param"), as.character) |>
          mutate(
            current_param0 = current_param
            , pred =
              ifelse(
                current_param == current_rule_in[2]
                , next_param, current_param
              ) |>
              factor()
          ) |>
          mutate_at(c("current_param", "next_param"), factor) |>
          rename_at("next_param", \(x) next_rule_in[1]) |>
          rename_at("current_param", \(x) current_rule_in[1])
        
        results_exc <-
          results |>
          filter(current_param0 != current_rule_in[2]) |>
          mutate(
            step =
              paste0(
                current_rule_in[1]
                , "="
                , current_param0
              )
          ) |>
          select(-current_param0)
        
        results <-
          results |>
          filter(current_param0 == current_rule_in[2]) |>
          select(-current_param0) |>
          mutate(step = NA)
      }else if(j > 1  & j < max(current_model$step)){
        current_rule_in <- str_split_fixed(current_step$value, "\\=", 2)[1, ]
        
        next_step <- filter(current_model, step == j + 1)
        next_rule_in <- str_split_fixed(next_step$value, "\\=", 2)[1, ]
        
        results <-
          results |>
          rename_at(next_rule_in[1], \(x) "next_param") |>
          mutate_at(c("pred", "next_param"), as.character) |>
          mutate(
            pred0 = pred
            , pred =
              ifelse(
                pred == current_rule_in[2]
                , next_param
                , pred
              )
          ) |>
          mutate_at(c("pred", "next_param"), factor) |>
          rename_at("next_param", \(x) next_rule_in[1])
        
        results_exc <-
          results_exc |>
          rbind(
            results |>
              filter(pred0 != current_rule_in[2]) |>
              mutate(
                step =
                  paste0(
                    current_rule_in[1]
                    , "="
                    , pred0
                  )
              ) |>
              select(-pred0)
          )
        
        results <-
          results |>
          filter(pred0 == current_rule_in[2]) |>
          select(-pred0) |>
          mutate(step = NA)
      }else if(j > 1  & j == max(current_model$step)){
        current_rule_in <- str_split_fixed(current_step$value, "\\=", 2)[1, ]
        
        results <-
          results |>
          mutate(step = current_rule_in[1]) |>
          rbind(results_exc) |>
          arrange(factor(id, unique(data$id)))
      }
    }
    
    comb_model[[i]] <-
      results |>
      select(id, obs, pred, step)
  }
  
  comb_model <-
    comb_model |>
    `names<-`(model_name)
  
  return(comb_model)
}