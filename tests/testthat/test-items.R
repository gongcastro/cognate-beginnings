test_items <- function(items){
    
    test_that("item variable names are right", {
        expect_true(
            all(colnames(items) %in% c("te",
                                       "meaning",
                                       "language",
                                       "item",
                                       "ipa",
                                       "sampa",
                                       "lv",
                                       "n_phon",
                                       "freq",
                                       "list"))
        )
    })
    
    test_that("item variable classes are right", {
        expect_type(items$te, "integer")
        expect_type(items$meaning, "character")
        expect_type(items$language, "character")
        expect_type(items$item, "character")
        expect_type(items$ipa, "character")
        expect_type(items$sampa, "character")
        expect_type(items$lv, "double")
        expect_type(items$n_phon, "integer")
        expect_type(items$freq, "double")
        expect_type(items$list, "list")
    })
    
    test_that("all TEs have two items", {
        expect_true(
            items %>% 
                count(te) %>% 
                pull(n) %>% 
                all(. == 2)
        )
    })
    
    test_that("all frequencies are positive", {
        expect_true(
            all(items$freq > 0)
        )
    })
    
    test_that("all n_phon are positive", {
        expect_true(
            all(items$n_phon > 0)
        )
    })
    
    test_that("all lv range between 0 and 1", {
        expect_true(
            all(between(items$lv, 0, 1))
        )
    })
    
    test_that("all lv_dist are between 0 and 1", {
        expect_true(
            all(between(items$lv, 0, 1))
        )
    })
    
}
