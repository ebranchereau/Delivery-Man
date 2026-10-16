# Delivery Man — A* Pathfinding in R

A university team project for the Uppsala University Artificial Intelligence course. The project implements an A* pathfinding controller for a delivery simulation with changing road costs.

## What the solution does

The student implementation is in `R/astarDM.R`. It:

- selects an available package or the destination of the package currently being carried;
- computes a route with A* search;
- uses Manhattan distance as the heuristic;
- accounts for the current horizontal and vertical road costs;
- returns the car's next move to the supplied simulation framework.

The repository includes the original course simulation and test framework. That framework remains attributed to its original authors in `DESCRIPTION`; the A* controller is the student-team contribution.

## Project structure

- `R/astarDM.R` — A* controller and pathfinding helpers
- `R/DM.R` — supplied Delivery Man simulation and test framework
- `R/runDeliveryMan.R` — runs an interactive simulation
- `R/runTestDM.R` — runs the automated evaluation
- `Pdfs/` — course materials retained for context

## Running the project

Requirements:

- R
- A local installation of this package

From the repository root, install the package:

```sh
R CMD INSTALL .
```

Run the automated evaluation:

```sh
Rscript R/runTestDM.R
```

Run the simulation:

```sh
Rscript R/runDeliveryMan.R
```

To test a different randomized scenario, change the seed passed to the supplied test framework.

## Notes

This repository is presented as a portfolio record of a university team project. It contains course-provided framework code as well as the student team's A* implementation. See `LICENSE` for reuse information.
