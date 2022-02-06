context("determine_winner_function")

test_that("check values", {
    # test 1
    game <- c("car", "goat", "goat")
    choice <- 1
    res <- determine_winner(choice, game)
    expect_equal( res , "WIN")
    # test 2
    game <- c("car", "goat", "goat")
    choice <- 2
    res <- determine_winner(choice, game)
    expect_equal( res , "LOSE")
    # test 3
    game <- c("car", "goat", "goat")
    choice <- 3
    res <- determine_winner(choice, game)
    expect_equal( res , "LOSE")
})
