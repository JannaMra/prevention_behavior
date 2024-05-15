### Iterative outlier removal
# based on Van Selst & Jolicoeur (1994)  

outlier_remove <- function(x, sd_thresh = 3, na.rm = TRUE) {
  # conservative behavior: return NA, if option specified
  if (!na.rm & any(is.na(x))) {
    return(rep(NA, length(x)))
  }
  # check that there are enough non-NA values so procedure below does not throw index errors
  if (sum(!is.na(x)) <= 2) {
    return(as.integer(!is.na(x)))
  }

  # avoid computing certain variables multiple times
  x_len <- length(x)

  # allocate vector indicating whether certain index is already excluded from analyses
  is_in_sample <- rep(TRUE, length(x))
  is_in_sample[is.na(x)] <- FALSE

  while (sum(is_in_sample) > 3) {

    # get currently included values (sub-sample)
    x_sample <- x[is_in_sample]

    # get min and max positions of this sample
    # x might contain non-unique elements. Thus, more than 1 values
    # might be equal to the current extremum, some of which are already excluded;
    # therefore, take the FIRST data point that meets the extremum and is still
    # included
    x_min_index <- which.max(x == min(x_sample) & is_in_sample)
    x_max_index <- which.max(x == max(x_sample) & is_in_sample)

    # temporarily remove extreme values
    # the values overwritten here will be explicitly set below
    is_in_sample[x_min_index] <- FALSE
    is_in_sample[x_max_index] <- FALSE
    x_sample_temp <- x[is_in_sample]

    # compute adjusted mean and sd
    mean_temp <- mean(x_sample_temp)
    sd_temp <- sd(x_sample_temp)

    # check whether changes must be made
    is_min_ok <- x[x_min_index] >= (mean_temp-sd_thresh*sd_temp)
    is_max_ok <- x[x_max_index] <= (mean_temp+sd_thresh*sd_temp)

    # explicitly set values that were temporarily overwritten
    is_in_sample[x_min_index] <- is_min_ok
    is_in_sample[x_max_index] <- is_max_ok

    # stop if algorithm converged, i.e., no more changes happened
    if (is_min_ok & is_max_ok) {
      break
    }
  }

  return(as.integer(is_in_sample))  # convert to integer for backwards compatibility
}


