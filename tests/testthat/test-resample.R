

test_that("Input and output data should have the same dimensions", {
  dat <- base::data.frame(participant=rep(1:10, each=10),
                           happiness = rnorm(100, mean = 3.5, sd=1.7),
                           stress = rnorm(100, mean = 2, sd= 1.3),
                           anxiety = rnorm(100, mean = 1.7, sd= 2))
  new_dat <- resample(dat, participant, c("stress", "anxiety"))
  expect_equal(dim(dat), dim(new_dat))
  expect_equal(dim(dat), dim(new_dat))
})


test_that("Only the order of the values of the specified variables should differ between
          the input and output data", {
            dat <- base::data.frame(participant=rep(1:10, each=10),
                                    happiness = rnorm(100, mean = 3.5, sd=1.7),
                                    stress = rnorm(100, mean = 2, sd= 1.3),
                                    anxiety = rnorm(100, mean = 1.7, sd= 2))
            new_dat <- resample(dat, participant, c("stress", "anxiety"))
            original_no_change <- dplyr::select(dat, participant, happiness)
            resample_no_change <- dplyr::select(new_dat, participant, happiness)
            expect_equal(original_no_change, resample_no_change)
            original_change <- dplyr::select(dat, participant, happiness)
            resample_change <- dplyr::select(new_dat, stress, anxiety)
            expect_false(isTRUE(all.equal(original_change, resample_change)))
            mean_sd_input <- dat %>% dplyr::group_by(participant) %>% dplyr::summarise(mean(stress), sd(stress))
            mean_sd_output <- new_dat %>% dplyr::group_by(participant) %>% dplyr::summarise(mean(stress), sd(stress))
            expect_equal(mean_sd_input, mean_sd_output)
          })




test_that("Function should only work for tibbles and data frames", {
  expect_error(resample("test_object"))
})
