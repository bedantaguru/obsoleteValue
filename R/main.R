
obsolete_value <- function( value, obsoletion_function, store_function = F){

  is_get_value_by_call <- F

  if(is.function(value)){
    if(!store_function){
      is_get_value_by_call <- T
    }
  }

  force_obsolete <- F

  initial_state <- obsoletion_function(create_state =  T)

  # methods

  set_force_obsolete <- function(is_o){
    force_obsolete <<- is_o
  }

  is_obsolete <- function(){
    is_o <- force_obsolete
    if(!is_o){
      is_o <- obsoletion_function(initial_state, create_state = F)
    }
    return(is_o)
  }

  get_value <- function(){

    out <- NULL

    if(!is_obsolete()){
      if(is_get_value_by_call){
        if(is.function(value)){
          out <- value()
        }else{
          out <- value
        }
      }else{
        out <- value
      }
    }

    return(out)

  }


  structure(list(value = get_value,is_obsolete = is_obsolete, set_force_obsolete = set_force_obsolete),
            class = 'ov')
}

