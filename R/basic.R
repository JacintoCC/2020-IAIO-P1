#' @title Initial State
#' @param problem Problema del cual obtener el estado inicial
#' @rdname initialState
#' @export initialState
initialState <- function(problem){
   UseMethod("initialState")
}


#' @title actions
#' @param problem Problema
#' @param state Estado actual del problema
#' @rdname actions
#' @export actions
actions <- function(problem, state) {
   UseMethod("actions")
}

#' @title checkValidState
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @rdname checkValidState
#' @export checkValidState
checkValidState <- function(problem, state, action){
   UseMethod("checkValidState")
}

#' @title nextState
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @rdname nextState
#' @export nextState
nextState <- function(problem, state, action){
   UseMethod("nextState")
}

#' @title finalState
#' @param problem Problema a resolver
#' @param state Estado a comparar si es final
#' @rdname finalState
#' @export finalState
finalState <- function(problem, state){
   UseMethod("finalState")
}

#' @title sameState
#' @param state1 Estado 1 a comparar
#' @param state2 Estado 2 a comparar
#' @rdname sameState
#' @export sameState
sameState <- function(state1, state2){
   UseMethod("sameState")
}

#' @title costState
#' @param problem Problema a resolver
#' @param state Estado a calcular el coste
#' @rdname costState
#' @export costState
costState <- function(problem, state){
   UseMethod("costState")
}

#' @title setCost
#' @param problem Problema a resolver
#' @param state Estado a cambiar el coste
#' @param new.cost Nuevo coste a establecer
#' @rdname setCost
#' @export setCost
setCost <- function(problem, state, new.cost){
   UseMethod("setCost")
}

#' @title searchList
#' @param state Estado a buscar
#' @param search.list Lista sobre la que buscar el estado
#' @export searchList
searchList <- function(state, search.list){
   if(length(search.list) > 0){
      equal.state <- sapply(search.list, function(x) sameState(state, x))
      which.equal <- which(equal.state)
      found.state <- ifelse(length(which.equal) > 0, which.equal[1], 0)
      return(found.state)
   }
   else{
      return(0)
   }
}

#' @title Path
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param closed Conjunto de estados cerrados sobre los que reconstruir el camino
#' @export path
path <- function(problem, state, closed){
  path_list <- list(state)

  while(state$prev > 0){
    state <- closed[[state$prev]]
    path_list <- append(list(state), path_list)
  }

  return(path_list)
}

#' @title representPath
#' @param problem Problema a resolver
#' @param path Estado actual
#' @export representPath
representPath <- function(problem, path){
   UseMethod("representPath")
}
