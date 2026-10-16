#' astarDM
#'
#' Selects a package target and computes the next move using A* search.
#' The evaluation function is f(x) = g(x) + h(x).
#' 
#' @param roads See help documentation for the runDeliveryMan function
#' @param car See help documentation for the runDeliveryMan function
#' @param packages See help documentation for the runDeliveryMan function
#' @return See help documentation for the runDeliveryMan function
#' @export

myFunction <- function(roads, car, packages) {
    #easier directions
    down <- 2
    left <- 4
    stay <- 5
    right <- 6
    up <- 8
    
    #print(roads)
    #stop()
    
    # find closest package
    if (car$load == 0) { # can you pickup a package or do you already have one
        # go pickup a package
        best <- NULL
        best_dist <- Inf

        for (i in 1:nrow(packages)) { # iterate through the packages
            p <- packages[i, ]
            status <- 5

            if (p[status] == 0){ # if you can pick up the package
                pos <- c(p[1], p[2])
                d <- heuristic(c(car$x, car$y), pos)

                if (d < best_dist) {
                    best_dist <- d
                    best <- pos
                }
            }
        }

        target_pos <- best

    } else if (car$load> 0) { # already is holding a package
        # drop off the package
        target_pos <- c(packages[car$load, 3], packages[car$load, 4]) # target pos becomes the drop off location of the held package
    }

    path <- aStar(roads, c(car$x, car$y), target_pos)
    
    if (is.null(path) || length(path) < 2) {
      car$nextMove <- stay
    } else {
      nxt <- path[[2]]
      car$nextMove <- if (nxt[1] > car$x) right
      else if (nxt[1] < car$x) left
      else if (nxt[2] > car$y) up
      else down
    }

    return(car)
}
# here is the function for the assignment
aStar <- function(roads, start, goal){
  # Helper function that converts two coordinates into a string key
  key <- function(s) paste(s[1], s[2])
  
  visited <- character(0)
  frontier <- list()
  
  # Adding the start node into frontier
  frontier[[key(start)]] <- list( estimated = heuristic(start, goal), cost = 0, state = start, prev = NULL )
  # to trace our route backward
  cameFrom <- list()
  reachedGoal <- FALSE
  
  while (!reachedGoal && length(frontier) > 0) {
    # loops through every item and extracts all values into a vector
    ests <- sapply(frontier, function(e) e$estimated)
    costs <- sapply(frontier, function(e) e$cost)
    # finds the lowest estimated cost and which() finds node position (index)
    tie <- which(ests == min(ests))
    # here we break the tie by lowest cost 
    i <- tie[which.min(costs[tie])]
    
    node <- frontier[[i]]
    frontier[[i]] <- NULL
    
    state <- node$state 
    if (!is.null(node$prev)) cameFrom[[key(state)]] <- node$prev
    if (all(state == goal)){
      reachedGoal <- TRUE
    }else{
      visited <- c(visited, key(state))
      neighbour <- neighbours(state)
      for (r in 1:nrow(neighbour)){
        n <- neighbour[r,]
        k <- key(n)
        if ( k %in% visited) next
        
        d <- roadCost(roads, state, n)
        g <- node$cost + d
        costn <- g + heuristic(n, goal)
        
        if(!(k %in% names(frontier))){
          frontier[[k]] <- list(estimated = costn, cost = g, state = n,prev = state)
        }
        else if (costn < frontier[[k]]$estimated) {
        frontier[[k]] <- list(estimated = costn, cost = g, state = n, prev = state)
        }
      }
    }
  }
  if (!reachedGoal) return(NULL)
  
  path <- list()
  s <- goal
  while (!is.null(s)){
    path <- c(list(s), path)
    s <- if (key(s) %in% names(cameFrom)) cameFrom[[key(s)]] else NULL 
  }
  return(path)
}

neighbours <- function(position){
  result <- matrix(nrow = 0, ncol = 2)
  
  if(position[1] < 10)
    result <- rbind(result, c(position[1] + 1, position[2]))
  if(position[1] > 1)
    result <- rbind(result, c(position[1] - 1, position[2]))
  if(position[2] < 10)
    result <- rbind(result, c(position[1], position[2] + 1))
  if(position[2] > 1)
    result <- rbind(result, c(position[1], position[2] - 1))

  return(result)
}

roadCost <- function(roads, from, to) {
  if(from[1] < to[1])
    return(roads$hroads[from[1], from[2]])
  if(from[1] > to[1])
    return(roads$hroads[from[1] - 1, from[2]])
  if(from[2] > to[2])
    return(roads$vroads[from[1], from[2] - 1])
  if(from[2] < to[2])
    return(roads$vroads[from[1], from[2]])
  
  return(0)
}

# Returns the Manhattan distance in grid units from start to goal
heuristic <- function(start_pos, goal_pos) { 
  abs(start_pos[1] - goal_pos[1]) + abs(start_pos[2] - goal_pos[2])
}

clamp <- function(x, lower, upper){
  return(max(min(x, upper), lower))
}

# See README.md for installation, simulation, and testing instructions.
