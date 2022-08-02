tar_load(responses)

test_that("response variable names are right", {
    expect_true(
        all(colnames(responses) %in% c("id", "time", "code", "language", "item", "response"))
    )
})

test_that("response variable classes are right", {
    expect_type(responses$id, "character")
    expect_type(responses$time, "integer")
    expect_type(responses$code, "character")
    expect_type(responses$item, "character")
    expect_type(responses$response, "integer")
})

test_that("all codes have more than 100 associated responses", {
    expect_true(
        responses %>% 
            count(code) %>% 
            pull(n) %>% 
            all(. > 100)
    )
})

test_that("all codes have less than 800 associated responses", {
    expect_true(
        responses %>% 
            count(code) %>% 
            pull(n) %>% 
            all(. < 800)
    )
})

test_that("no times are duplicated for the same ID", {
    expect_true(
        responses %>% 
            count(id, time, item) %>% 
            pull(n) %>% 
            all(. < 2)
    )
})

test_that("no codes are duplicated", {
    expect_true(
        responses %>% 
            distinct(id, time, code) %>% 
            count(code) %>% 
            pull(n) %>% 
            all(. < 2)
    )
})

