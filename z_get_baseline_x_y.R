

get_baseline_x_y <- function(baseline_start, baseline_end, fixation_start, fixation_end, x, y) {
  # get fixations that (partially) overlap with baseline
  overlapping_fixations <- !(fixation_end <= baseline_start | fixation_start >= baseline_end)
  fixation_start_overlap <- fixation_start[overlapping_fixations]
  fixation_end_overlap <- fixation_end[overlapping_fixations]
  x_overlap <- x[overlapping_fixations]
  y_overlap <- y[overlapping_fixations]
  
  # return NA if no valid baseline fixations
  if (!any(overlapping_fixations, na.rm = TRUE)) {
    return(list(x = NA, y = NA))
  }
   
  # Calculate baseline coordinates
  x_mean <- sum(x_overlap * (fixation_end_overlap - fixation_start_overlap) ) / sum(fixation_end_overlap - fixation_start_overlap)
  y_mean <- sum(y_overlap * (fixation_end_overlap - fixation_start_overlap) ) / sum(fixation_end_overlap - fixation_start_overlap)
  return(list(x = x_mean, y = y_mean))
}



