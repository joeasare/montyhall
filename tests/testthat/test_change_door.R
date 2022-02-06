context("change_door_function")

test_that("check lengths", {
    # test 1
    choice <- 1
    opened.door <- c(choice)
    doors <- 3
    res <- change_door(doors, T, opened.door, choice)
    expect_equal( length(res) , 1)
    # test 2
    choice <- 1
    opened.door <- c(choice)
    doors <- 3
    res <- change_door(doors, F, opened.door, choice)
    expect_equal( length(res) , 2)
})

test_that("check value", {
    # test 1
    choice <- 1
    opened.door <- c(choice)
    doors <- 3
    res <- change_door(doors, T, opened.door, choice)
    expect_equal( res , choice)
    # test 2
    choice <- 1
    opened.door <- c(choice)
    doors <- 3
    res <- change_door(doors, F, opened.door, choice)
    expect_equal( sum(res == choice), 0 )
})