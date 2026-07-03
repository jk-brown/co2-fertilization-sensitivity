library(tidyverse)
library(matilda)

normalize_gmst <- function(data,
                           ref_start = 1850,
                           ref_end = 1900) {
  
split_by_run_number <- split(data, data$run_number)

normalized_list <- lapply(split_by_run_number, function(df) {

  gmst_subset <- subset(df, variable == "gmst")
  var_subset <- subset(df, variable != "gmst")  

  ref_period <- subset(
    gmst_subset,
    year >= ref_start & 
      year <= ref_end
  )  

mean_ref_dat <- mean(ref_period$value, na.rm = TRUE)  

gmst_subset$value <- gmst_subset$value - mean_ref_dat  

df_normalized <- rbind(var_subset, gmst_subset)

return(df_normalized)
})

mod_result_norm <- do.call(r.bind, normalized_list)

rownames(mod_result_norm) <- NULL

return(mod_result_norm)

}

mod_result_norm <- normalize_gmst(mod_result,
                                  ref_start = 1850,
                                  ref_end = 1900)

lc_warming <- new_metric(var= "gmst", years = 2081:2100, op = mean)

lc_warming_results <- metric_calc(x = mod_result_norm,
                                  metric = lc_warming)

head(lc_warming_results)


