# Monty-Hall Problem
## Steps to run
- make sure you have R installed and Rstudio IDE.
- install all dependencies
```
install.packages(c("devtools", "roxygen2","usethis","testthat","knitr"))
```
- main function that initiates a simulation is *play_n_games(n, question_number)*
```
# play_n_games(1000, 2) # to run for question 2
# play_n_games(1000, 1) # to run for question 1
```
- uncomment the required function and click on run in Rstudio.

## Steps to test using testthat
- run this command in console 
```
devtools::test()
```
## Steps to update documentation
- make changes and run this command in console
```
devtools::document()
```
