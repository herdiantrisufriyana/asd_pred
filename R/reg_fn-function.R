reg_fn <- function(formula, data){
  
  if("t" %in% colnames(data)){
    
    if(as.character(formula)[2] == "survive"){
      data <-
        data |>
        mutate(survive = ifelse(t == max(t), survive, 1)) |>
        mutate(survive = 1 - survive)
    }
    
    data <-
      data |>
      mutate(tstart = t - 1) |>
      rename(tstop = t)
    
    data <-
      data |>
      rename_at(as.character(formula)[2], \(x) "outcome") |>
      mutate(
        outcome =
          Surv(time = tstart, time2 = tstop, event = outcome, type = "counting")
      ) |>
      rename_at("outcome", \(x) as.character(formula)[2])
    
    coxph(formula, data, id = id)
    
  }else{
    glm(formula, binomial(), data)
  }
  
}