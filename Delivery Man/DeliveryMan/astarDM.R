#' astarDM
#'
#' This function will go through the grid using the A* algorithm, we have the equation
#' f(x) = g(x) + h(x)...
#' 
#' @param roads See help documentation for the runDeliveryMan function
#' @param cars See help documentation for the runDeliveryMan function
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
   
    # reutrns the distance in grid units from start to goal
    heuristic <- function(start_pos, goal_pos) { 
        abs(start_pos[1] - goal_pos[1]) + abs(start_pos[2] - goal_pos[2])
    }

    
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

    # go to position
    if (car$x < target_pos[1]) {
        car$nextMove <- right
    } else if (car$x > target_pos[1]) {
        car$nextMove <- left
    } else if (car$y < target_pos[2]) {
        car$nextMove <- up
    } else if (car$y > target_pos[2]) {
        car$nextMove <- down
    } else {
        car$nextMove <- stay
    }

    return(car)
}

# to run/test/use: R
#                  source("DeliverMan/astarDM.R")
#                  runDeliveryMan(myFunction)
#                  testDM(myFunction)

# you need to change the seed in order for the results to be diffrrent