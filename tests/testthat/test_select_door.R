context("select_door_function")

test_that("check lengths", {
    # test 1
    doors <- 10
    res <- select_door(doors)
    expect_equal( length(res) , 1)
    # test 2
    doors <- 4
    res <- select_door(doors)
    expect_equal( length(res) ,  1)
})

test_that("check value of door", {
    # test 1
    doors <- 10
    res <- select_door(doors)
    expect_gt( res , 0)
    expect_lt( res, doors + 1)
    # test 2
    doors <- 4
    res <- select_door(doors)
    expect_gt( res , 0)
    expect_lt( res, doors + 1)
})