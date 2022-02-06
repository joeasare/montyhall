#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of n doors
#'   with goats and cars behind them.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are n doors for a contestant
#'   to choose from. Then given constraints in questions are applied
#'   and results are noted. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param nCars number of cars
#' @param nGoats number of goats
#'
#' @return The function returns a length n character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game(1,2)
#'
#' @export
create_game <- function(nCars, nGoats)
{
    x = c()
    for( i in 1:nGoats )  # iterator
    {
        x <- append(x, "goat")
    }
    for( i in 1:nCars )  # iterator
    {
        x <- append(x, "car")
    }

    a.game <- sample( x, size=nCars + nGoats, replace=F )
    return( a.game )
}


#' @title selects door
#' @description
#'   `select_door()` selects a door out of n doors.
#'
#' @details
#'   In this function we wrote code such that the function
#'   will choose a door out of n availavble door
#'
#' @param nDoor number of doors is taken as argument.
#'
#' @return The function returns a the door choosen
#'
#' @examples
#'   select_door(10)
#' @export
select_door <- function( nDoor )
{
    doors <- 1:nDoor
    a.pick <- sample( doors, size=1 )
    return( a.pick )
}


#' @title opens a goat door
#' @description
#'      'open_goat_door()' simulates host opening a goat door after player
#'      picks his first door
#' @details
#'      This function returns the doors opened by the host
#'      after the player picks a door in first step.
#'      if the first picked door is a goat door, then choose a goat door
#'      other than the picked one.
#'      if the first picked door is a car door, then pick any goat door available
#       if the program is runnning for first question, select a car door,
#'      as well as a goat door from the game vector
#'
#' @param nDoor number of doors
#' @param game the game array containing randomly
#'    spread 'cars' and 'goats'
#' @param a.pick The choosen door
#' @param case_1
#'               True of False depending on
#'               whether program is running for question 1 or not (default = F)
#' @return
#'      the vector of gates opened by the host
#' @examples
#'      open_goat_door(5, game, 2, T)
#'      open_goat_door(7, game, 3, F)
#' @export
open_goat_door <- function( nDoor, game, a.pick, case_1 = F )
{
    doors <- 1:nDoor
    if( game[ a.pick ] == "car" )
    {
        goat.doors <- doors[ game != "car" ]
        opened.door <- sample( goat.doors, size=1 )
        if(case_1){
            car.doors <- doors[ game != "goat"  & doors != a.pick ]
            if( length(car.doors) == 1 ){
              opened.door <- append(opened.door, car.doors)
            }
            else{
              opened.door <- append(opened.door, sample( car.doors, size=1 ) )
            }
        }
    }
    if( game[ a.pick ] == "goat" )
    {
        goat.doors <- doors[ game != "car" & doors != a.pick ]
        if(length(goat.doors) == 1){
            opened.door <- goat.doors;
        }
        else{
            opened.door <- sample(goat.doors, size=1)
        }
        if(case_1){
          car.doors <- doors[ game != "goat" ]
          opened.door <- append(opened.door, sample(car.doors, size=1))
        }
    }
    return( opened.door )
}

#' @title change selected door
#' @description
#'      'change_door()' returns final choice to whether stay or change the door
#' @details
#'      if the stay parameter is true, then stay with the picked door
#'      if the stay parameter is false, then pick any other door from
#'      the game array except the one player picked and the one host opened
#'
#' @param nDoor number of doors in the game
#' @param stay true of false depicting whether to stay with the picked door or not
#' @param opened.door the door opened by the host
#' @param a.pick the door picked by the player
#'
#' @return the final door selected
#' @examples
#'      change_door(5, T, 2, 1)
#' @export
change_door <- function( nDoor, stay=T, opened.door, a.pick )
{
    doors <- 1:nDoor

    if( stay )
    {
        final.pick <- a.pick
    }
    if( ! stay )
    {
        final.pick <- doors[ doors != opened.door & doors != a.pick ]
    }

    return( final.pick )  # number between 1 and 3
}



#' @title Determine winner
#' @description
#'   `determine_winner()` determines who is winner on basis of the final
#'    pick . if its a car then its a win or else loss
#' @details
#'   If the final pick is car then the result is WIN
#'   If the final pick is goat then the result is LOSE
#'
#' @param final.pick The final choice
#' @param game the game array containing randomly
#'    spread 'cars' and 'goats'
#' @return The function returns WIN or LOSE
#'
#' @examples
#'   determine_winner(7,game)
#' @export
determine_winner <- function( final.pick, game )
{
    if( game[ final.pick ] == "car" )
    {
        return( "WIN" )
    }
    if( game[ final.pick ] == "goat" )
    {
        return( "LOSE" )
    }
}





#' @title play one round of Monty Hall Problem game.
#' @description
#'      'play_game()' execute the game steps and return the
#'      result of the game
#' @details
#'      This function first creates a new game by calling 'create_game()'
#'      first.pick stores the door picked by the user, returned by 'select_door()'
#'      The host opens new Door(s) and that is returned by 'open_goat_door()'
#'      and stored in opened.door
#'      Then 'determine_winner()' gets the winner for both the scenarios of staying
#'      and switching the picked door stratery
#'      Finally the verdict is retured from the function in 'game.results'
#'
#' @param goats number of goats
#' @param cars number of cars
#' @param question true if the program is running for the 1st Question
#'                 false if the program is running for 2nd question
#' @return
#'         returns whether the player wins or looses the game
#' @examples
#'       play_game(3, 2, T)
#'       play_game(5, 7, F)
#' @export
play_game <- function( goats, cars, question )
{
    doors <- cars + goats
    new.game <- create_game(cars, goats)
    first.pick <- select_door(doors)
    opened.door <- open_goat_door( doors, new.game, first.pick, question == 1 )

    final.pick.stay <- change_door( doors, stay=T, opened.door, first.pick )
    final.pick.switch <- change_door( doors, stay=F, opened.door, first.pick )

    outcome.stay <- determine_winner( final.pick.stay, new.game  )
    outcome.switch <- determine_winner( final.pick.switch, new.game )

    strategy <- c("stay","switch")
    outcome <- c(outcome.stay,outcome.switch)
    game.results <- data.frame( strategy, outcome, stringsAsFactors=F )
    return( game.results )
}



#' @title
#'   Play games for n times
#' @description
#'   `play_n_games()` runs the game for n times and notes the reults.
#'
#' @details
#'   The function calls play_game( ) function repeatedly
#'   The results obtained from play_game() as return value
#'   are noted and displayed as a table
#'
#' @param n number of times game is played
#' @param question question number (default = 1)
#'
#' @return The function returns a table containing the results obtained
#'         by running play_games()  n number of times.
#' @examples
#'   play_n_games(1000,2)
#' @export
play_n_games <- function( n=100, question=1 )
{

    library( dplyr )
    results.list <- list()   # collector
    loop.count <- 1

    goats <- 3
    cars <- 2
    if(question == 2){
        goats <- readline(prompt = "Enter number of goats : ")
        goats <- as.integer(goats)
        cars <- readline(prompt = "Enter number of cars : ")
        cars <- as.integer(cars)
    }

    for( i in 1:n )  # iterator
    {
        if(question == 1) {
            game.outcome <- play_game(goats, cars, T)
        }
        else if(question == 2) {
            game.outcome <- play_game(goats, cars, F)
        }
        else {
            print("Invalid input")
            return();
        }
        results.list[[ loop.count ]] <- game.outcome
        loop.count <- loop.count + 1
    }

    results.df <- dplyr::bind_rows( results.list )

    table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

    return( results.df )

}

# play_n_games(1000, 2) # to run for question 2
# play_n_games(1000, 1) # to run for question 1
