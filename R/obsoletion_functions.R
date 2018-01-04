condition_obsolete <- function(condition_expression){
  cf_exp <- expr_to_function(condition_expression)
  of <- function(initial_state, create_state = F){
    if(create_state){
      if(missing(initial_state)){
        return(NULL)
      }else{
        return(initial_state)
      }
    }else{
      return(cf_exp())
    }
  }
  return(of)
}

time_obsolete <- function(time){
  of <- function(initial_state, create_state = F){
    if(create_state){
      if(missing(initial_state)){
        return(list(start_time =  Sys.time(), obsoletion_time = time))
      }else{
        initial_state$start_time <- Sys.time()
        initial_state$obsoletion_time <- time
        return(initial_state)
      }
    }else{
      return(initial_state$obsoletion_time <= Sys.time())
    }
  }
  return(of)
}

file_obsolete <- function(files, cols){
  cols_native <- colnames(file.info(files[1]))

  if(missing(cols)){
    cols <- cols_native
  }

  if(length(intersect(cols,cols_native))==0){
    warning("Column name not matched. All columns are used.")
    cols <- cols_native
  }

  file_state <- function(fn, cl){
    inf <- file.info(fn)
    inf <- inf[cl]
    inf %>% apply(1, paste0, collapse = "+") %>% paste0(collapse = "*") %>% digest()
  }

  of <- function(initial_state, create_state = F){
    if(create_state){
      if(missing(initial_state)){
        return(list(dependent_files =  files, dependent_file_cols = cols, start_files_state = file_state(files, cols)))
      }else{
        initial_state$dependent_files <- files
        initial_state$dependent_file_cols <- cols
        initial_state$start_files_state <- file_state(files, cols)
        return(initial_state)
      }
    }else{
      return(initial_state$start_files_state %>% equals(file_state(initial_state$dependent_files, initial_state$dependent_file_cols)) %>% not())
    }
  }

  return(of)
}

state_function_obsolete <- function(state_function, state_store_tag = "state_function_start_state"){

  of <- function(initial_state, create_state = F){
    if(create_state){
      if(missing(initial_state)){
        l <- list()
        l[[state_store_tag]] <- state_function() %>% digest()
        return(l)
      }else{
        initial_state[[state_store_tag]] <- state_function() %>% digest()
        return(initial_state)
      }
    }else{
      return(initial_state[[state_store_tag]] %>% equals(state_function() %>% digest()) %>% not())
    }
  }

  return(of)
}

dependent_obsolete <- function(ov, ov_store_tag = "dependent_on_ov"){

  if(!is.ov(ov)){
    stop("The input is not an Obsolete Value")
  }

  of <- function(initial_state, create_state = F){
    if(create_state){
      if(missing(initial_state)){
        l <- list()
        l[[ov_store_tag]] <- ov
        return(l)
      }else{
        initial_state[[ov_store_tag]] <- ov
        return(initial_state)
      }
    }else{
      return(initial_state[[ov_store_tag]]$is_obsolete())
    }
  }

  return(of)
}

opposite <- function(of){
  new_of <- function(initial_state, create_state = F){
    if(create_state){
      return(of(create_state = T))
    }else{
      return(of(initial_state, create_state = F) %>% not())
    }
  }
  return(new_of)
}

combine_obsoletion_functions <- function(of1, of2, join_function = and){
  comb_of <- function(initial_state, create_state = F){
    if(create_state){
      return(of1(create_state = T) %>% of2(create_state = T))
    }else{
      return(of1(initial_state, create_state = F)  %>% join_function(of2(initial_state, create_state = F)))
    }
  }
  return(comb_of)
}


`%and%` <- function(of1, of2){
  combine_obsoletion_functions(of1, of2, join_function = and)
}



`%or%` <- function(of1, of2){
  combine_obsoletion_functions(of1, of2, join_function = or)
}
