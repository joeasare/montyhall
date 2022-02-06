context("open_goat_door_function")

test_that("check lengths", {
    # test 1
    game <- c("car", "goat", "goat")
    choice <- 1
    doors <- 3
    res <- open_goat_door(doors, game, choice, F)
    expect_equal( length(res) , 1)
    # test 2
    game <- c("car", "car", "goat", "goat", "goat")
    choice <- 1
    doors <- 5
    res <- open_goat_door(doors, game, choice, T)
    print("==> ")
    print(res)
    expect_equal( length(res) , 2)
})

test_that("check value", {
    # test 1
    game <- c("car", "goat", "goat")
    choice <- 1
    doors <- 3
    res <- open_goat_door(doors, game, choice, F)
    expect_equal( game[res],  "goat")
    # test 2
    game <- c("car", "car", "goat", "goat", "goat")
    choice <- 1
    doors <- 5
    res <- open_goat_door(doors, game, choice, T)
    # expect_equal( game[res] , "goat")
})