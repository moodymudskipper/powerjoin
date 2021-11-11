as_conflict_function <- function(conflict) {
  if(is_function(conflict)) return(conflict)
  if(!is_formula(conflict)) abort("wrong `conflict` argument")
  if(identical(conflict[[2]], quote(rw))) {
    conflict <- conflict[-2]
    fun <- function(x,y) mapply(as_function(conflict), x, y)
    return(fun)
  }
  as_function(conflict)
}

handle_conflicts <- function(out, x_slicer, y_slicer, conflicted_data, conflict) {
  # return unaltered input if no conflict
  if(is.null(conflict) || is.null(conflicted_data)) return(out)

  if(is.list(conflict)) {
      res <- list()
      for(nm in names(conflict)) {
        conflict_i <- conflict[[nm]]
        # special case for patch
        if(identical(conflict_i, "patch")) {
          res[[nm]]  <- conflicted_data$x[[nm]][x_slicer]
          res[[nm]][!is.na(y_slicer)] <-
            conflicted_data$y[[nm]][y_slicer][!is.na(y_slicer)]
        } else {
          conflict_i <- as_conflict_function(conflict_i)
          res[[nm]] <- conflict_i(
            conflicted_data$x[[nm]][x_slicer],
            conflicted_data$y[[nm]][y_slicer])
        }
      }
      out[names(res)] <- res
      return(out)
    }

  # special case for patch
  if(identical(conflict, "patch")) {
    res <- conflicted_data$x[x_slicer,]
    res[!is.na(y_slicer),] <-
      conflicted_data$y[y_slicer,][!is.na(y_slicer),]
    out[names(res)] <- res
    return(out)
  }

  # vectorized conflict support
  res <- Map(
    as_conflict_function(conflict),
    conflicted_data$x[x_slicer,],
    conflicted_data$y[y_slicer,])

  out[names(res)] <- res
  out
}
