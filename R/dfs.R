#' DFS algorithm
#' @param problem Problema a buscar
#' @export dfs
dfs <- function(problem){
  current.state <- initialState(problem)
  set.open <- list(current.state)
  set.open.costs <- c(0)
  set.open.heuristic <- c(Inf)

  set.closed <- list()
  set.closed.costs <- numeric(0)

  nodos <- 0
  index <- 1

  while (!finalState(problem, current.state) && length(set.open) > 0) {
     print("NODOS")
     print(nodos)
     print(current.state)
    nodos <- nodos + 1

    set.closed <- append(set.closed,
                         list(set.open[[index]]))
    set.closed.costs <- append(set.closed.costs,
                               set.open.costs[[index]])

                                        # Update Open
    set.open[[index]] <- NULL
    set.open.costs <- set.open.costs[-index]
    set.open.heuristic <- set.open.heuristic[-index]


                                        # For each of these neighbor spaces, assign cost and pointers;
                                        # and if some are in the closed set and their costs are smaller,
                                        # update their cost and pointers

                                        # Find cost and heuristic of moving to neighbor spaces to goal
    actions = actions(problem, current.state)

    for(act in actions){
       print("CHECK NEW ACTION")
       print(act)
                                        #Compute next state
      next.state <- nextState(problem, current.state, act)
      next.state$prev <- length(set.closed)
      next.cost <- current.state$cost + costState(problem, next.state)
      next.heuristic <- 0
      print("NEXT STATE")
      print(next.state)
                                        # if cost is infinite, then it's an invalid state, so ignore
      if( is.finite(next.cost) ){
        next.node.is.open <- searchList(state = next.state, search.list = set.open)
        next.node.is.closed <- searchList(state = next.state, search.list = set.closed)

        if(!next.node.is.closed){
          problem <- setCost(problem, next.state, next.cost)

                                        # Update Open and their associated costs
          if(next.node.is.open){
            index <- searchList(next.state, set.open)
            set.open[[index]] <- NULL
            set.open.costs <- set.open.costs[-index]
            set.open.heuristic <- set.open.heuristic[-index]
          }

          set.open <- append(set.open, list(next.state))
          set.open.costs <- c(set.open.costs, next.cost)
          set.open.heuristic <- c(set.open.heuristic, next.heuristic)
        }
        else if(next.node.is.open){
          index <- searchList(next.state, set.open)
                                        # Update if we have a better route
          if(set.open.costs[index] > next.cost){
            problem <- setCost(problem, next.state, next.cost)
            set.open.costs[index] <- next.cost
            set.open[[index]]$prev <- length(set.closed)
            set.open.heuristic <- next.heuristic
          }
        }
        # else node has already been CLOSED, so check to see if we have found a
        # better route to it
        else{
          index <- searchList(next.state, set.closed)
                                        # Update if we have a better route
          if(set.closed.costs[index] > next.cost){
            problem <- setCost(problem, next.state, next.cost)
            set.open.costs[index] <- next.cost
            set.open[[index]]$prev <- length(set.closed)
            set.open.heuristic <- next.heuristic
          }
        }
      }

    }

    # Element in open with the smallest cost
    if(length(set.open) > 0){
      index <- length(set.open)
      current.state <- set.open[[index]]
    }
  }

  results <- list()

  if(finalState(problem, current.state)){
    print("Solucion Encontrada :D")
    print(paste("- Nodos expandidos: ", nodos))

    results <- list(
       "completed" = TRUE,
       "nodos" = nodos,
       "path" = path(problem, current.state, set.closed),
       "problem" = problem
    )
  }
  else if(length(set.open) == 0){
    print("No se ha encontrado una solucion :(")
    print(paste("- Nodos expandidos: ", nodos))

    results <- list(
       "completed" = FALSE,
       "nodos" = nodos,
       "problem" = problem
    )
  }

  return(results)
}
