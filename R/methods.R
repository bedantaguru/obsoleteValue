
is.ov <- function(x){
  inherits(x,"ov")
}

print.ov <- function(x, ...){
  print(x$value())
}

make_obsolete <- function(x){
  x$set_force_obsolete(T)
  invisible(0)
}


Ops.ov <- function(e1,e2){
  if(is.ov(e1)){
    e1 <- e1$value()
  }

  if(is.ov(e2)){
    e2 <- e2$value()
  }

  out <- get(.Generic)( e1, e2)

  return(out)

}
