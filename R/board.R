#' @title Get Definition of Board Problem
#'
#' @param n Length of the board
#' @export
getBoard <- function(n = 10){
   board <- matrix(0, nrow = n, ncol = n)
   board[c(1, n), ] <- Inf
   board[ , c(1, n)] <- Inf
   start <- c(2, 2)
   goal <- c(n-1, n-1)

   cost.matrix <- matrix(1, nrow = n, ncol = n)
   cost.matrix[start[1], start[2]] <- 0

   problem <- list("board"=board,
                   "start"=start,
                   "goal"=goal,
                   "cost.matrix"=cost.matrix)
   class(problem) <- "Board"

   return(problem)
}

#' @title costState for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado a calcular el coste
#' @export
#' @method costState Board
costState.Board <- function(problem, state){
   return(problem$board[state$position[1], state$position[2]])
}

#' @title setCost for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado a cambiar el coste
#' @param new.cost Nuevo coste a añadir
#' @export
#' @method setCost Board
setCost.Board <- function(problem, state, new.cost){
   problem$board[state$position[1], state$position[2]] <- new.cost
   return(problem)
}


#' @title Initial State for Board Problem
#'
#' @param problem Problema para obtener el estado inicial
#' @export
#' @method initialState Board
initialState.Board <- function(problem){
   state <- list()
   state$position <- problem$start
   state$cost <- costState(problem, state)
   state$prev <- 0

   class(state) <- c("BoardState")
   return(state)
}

#' @title Get actions for Board Problem
#'
#' @param problem Board problem
#' @param state Estado actual del problema
#' @export
#' @method actions Board
actions.Board <- function(problem, state){
   return(c("U", "R", "D", "L"))
}

#' @title Check State for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @export
#' @method checkValidState Board
checkValidState.Board <- function(problem, state, action){
   n <- nrow(problem$board)

   if(state$position[1] > n)
      state$position[1] = n
   else if(state$position[1] < 1)
      state$position[1] = 1
   else if(state$position[2] > n)
      state$position[2] = n
   else if(state$position[2] < 1)
      state$position[2] = 1

   return(state)
}

#' @title Next State for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado actual
#' @param action Accion tomada
#' @export
#' @method nextState Board
nextState.Board <- function(problem, state, action){
   new.state <- state

   # Apply action
   if(action == "U")
      new.state$position[2] <- new.state$position[2] + 1
   else if(action == "D")
      new.state$position[2] <- new.state$position[2] - 1
   else if(action == "L")
      new.state$position[1] <- new.state$position[1] - 1
   else if(action == "R")
      new.state$position[1] <- new.state$position[1] + 1

   new.state <- checkValidState(problem, new.state, action)

   new.state$cost <- new.state$cost + costState(problem, new.state)
   new.state$prev <- 0

   return(new.state)
}

#' @title Final State for Board Problem
#'
#' @param problem Problema a resolver
#' @param state Estado a comparar si es final
#' @export
#' @method finalState Board
finalState.Board <- function(problem, state){
   final <- all(problem$goal == state$position)
   return(final)
}

#' @title Same State for Board Problem
#'
#' @param state1 Estado 1 a comparar
#' @param state2 Estado 2 a comparar
#' @export
#' @method sameState BoardState
sameState.BoardState <- function(state1, state2){
   same <- all(state1$position == state2$position)
   return(same)
}

#' @title Same State for Board Problem
#'
#' @param problem Problema a representar
#' @param path Camino de la solución
#' @importFrom dplyr `case_when`
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @export
#' @method representPath Board
representPath.Board <- function(problem, path){
   # Board
   p.board <- problem$board %>%
      reshape2::melt() %>%
      dplyr::mutate(value=dplyr::case_when(value == Inf ~ NaN, TRUE ~ value))
   colnames(p.board) <- c("X", "Y", "value")

   # Cost
   cost.tiles <- problem$cost.matrix %>% reshape2::melt()
   colnames(cost.tiles) <- c("X", "Y", "cost")

   # Inner join
   p.board <- dplyr::left_join(p.board, cost.tiles, by=c("X","Y"))
   n.pasos <- length(path)
   p.board$pasos <- 1
   gif.board <- p.board
   p.board <- p.board %>%
      dplyr::mutate(
         value = dplyr::case_when(X == path[[1]]$position[1] & Y == path[[1]]$position[2] ~ 1.0,
                               TRUE ~ value*1.0))

   for(i in 2:n.pasos){
      paso <- path[[i]]
      p.board$pasos <- i

      p.board <- p.board %>%
         dplyr::mutate(value = dplyr::case_when(X == paso$position[1] & Y == paso$position[2] ~ i*1.0,
                                  TRUE ~ value))

      gif.board <- rbind(gif.board, p.board)
   }


   gif <- ggplot2::ggplot(gif.board, ggplot2::aes(x=.data$X, y=.data$Y, fill=.data$cost, color=.data$value)) +
      ggplot2::geom_tile(ggplot2::aes(width=0.95, height=0.9), size=1.8) +
      ggplot2::scale_fill_gradient2(low = "red", mid = "yellow", midpoint = n.pasos/2,
                                    high = "darkblue", space = "Lab") +
      ggplot2::scale_color_gradient2(low = "white", high = "darkgreen") +
      ggplot2::scale_x_continuous(breaks=1:10, labels = 1:10, minor_breaks = NULL) +
      ggplot2::scale_y_continuous(breaks=1:10, labels = 1:10, minor_breaks = NULL)+
      gganimate::transition_states(.data$pasos)

   return(gif)
}
