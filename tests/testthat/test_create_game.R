context("create_game_function")

test_that("check lengths", {
    # test 1
    cars <- 1
    goats <- 2
    res <- create_game(cars, goats)
    expect_equal( length(res) ,  cars + goats)
    # test 2
    cars <- 4
    goats <- 9
    res <- create_game(cars, goats)
    expect_equal( length(res) ,  cars + goats)
})

test_that("check goats and cars count", {
    # test 1
    cars <- 1
    goats <- 2
    res <- create_game(cars, goats)
    expect_equal( sum(res == "goat") , goats)
    expect_equal( sum(res == "car") , cars)
    # test 2
    cars <- 4
    goats <- 9
    res <- create_game(cars, goats)
    expect_equal( sum(res == "goat") , goats)
    expect_equal( sum(res == "car") , cars)
})