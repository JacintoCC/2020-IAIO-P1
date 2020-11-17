library(Practica1)
context("Board Problem")

test_that("Create Board Problem", {

   # Create Board
   board <- getBoard()

   # Check Initial State
   initial.state <- initialState(board)
   expect_equal(initial.state$position, c(2,2))
   expect_equal(initial.state$cost, 0)
   expect_equal(initial.state$prev, 0)

   # Check Same State
   expect_true(sameState(initial.state, initial.state))

   # New state
   possible.actions <- actions(board, initial.state)
   expect_equal(length(possible.actions), 4)

   # Next State
   next.state <- nextState(board, initial.state, possible.actions[1])
   expect_false(sameState(initial.state, next.state))

})

test_that("BFS",{
   # Create Board
   board <- getBoard()
   results.bfs <- Practica1::bfs(board)
})

test_that("Print BFS",{
   # Create Board
   board <- getBoard()
   results.bfs <- Practica1::bfs(board)
   representPath(results.bfs$problem, results.bfs$path)
})

test_that("DFS",{
   # Create Board
   board <- getBoard()
   results.bfs <- Practica1::dfs(board)
})
