find_outliers <- function(data) {
  
  # delete useless cols
  vec <- c("date", "start", "end", "today", 
             "audit", "note", "calculate", "deviceid", "geopoint", "id", "X_id","X__version__", "__version__")
  data <- data[, !(names(data) %in% vec)]
  
  # add or fix index col
  data <- data %>% mutate(index = 1:n())

  # helper function - normal outliers
  data_validation_outliers_normal <- function(data, maximum_standard_deviations = 3) {
    outliers_normal <- data %>% lapply(outliers.numerical, maximum_standard_deviations = maximum_standard_deviations)
    return(outliers_normal)
  }

  outliers.numerical <- function(x, maximum_standard_deviations = 3) {
    # IN:
    # x: numerical vector
    # maximum_standard_deviations: integer
    # out: vector of indicies, of values in x that deviate more than maximum_standard_deviations from the mean.

    x <- gsub(" ","",x)
    x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
    x_data_only<-hasdata(x)
    x_data_only_indices<-hasdata(x,return.index = T)
    outliers_indices_in_data_only<-which(abs(x_data_only-mean(x_data_only))>maximum_standard_deviations*sd(x_data_only)& length(unique(x_data_only))>10)
    outliers_indices_in_original_vector<-x_data_only_indices[outliers_indices_in_data_only]
    return(
      cbind(
        index=outliers_indices_in_original_vector,
        value=x[outliers_indices_in_original_vector]) %>% as.data.frame(stringsAsFactors=F))

  }

  # helper function - log normal outliers
  data_validation_outliers_log_normal <- function(data, maximum_standard_deviations = 3) {
    outliers_log_normal <- data %>% lapply(log.outliers.numerical, maximum_standard_deviations = maximum_standard_deviations)
    return(outliers_log_normal)
  }


  log.outliers.numerical <- function(x, maximum_standard_deviations = 3) {
    # IN:
    # x: numerical vector
    # maximum_standard_deviations: integer
    # out: vector of indicies, of values in x that deviate more than maximum_standard_deviations from the mean.
    x<- gsub(" ","",x)
    x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
    x_not_logged<-x
    x <- suppressWarnings(log(x))
    x_data_only <- hasdata(x)
    x_data_only_indices <- hasdata(x, return.index = T)
    outliers_indices_in_data_only <- which(abs(x_data_only - mean(x_data_only)) > maximum_standard_deviations * sd(x_data_only) & length(unique(x_data_only)) > 10)
    outliers_indices_in_original_vector <- x_data_only_indices[outliers_indices_in_data_only]
    return(
      cbind(

        index=outliers_indices_in_original_vector,
        value=x_not_logged[outliers_indices_in_original_vector]) %>% as.data.frame(stringsAsFactors=F))
  }

  # general helper functions
  hasdata <- function(x, return.index = F) {
    # in: vector of any class
    # out: the in vector without NULL,NA, or ""
    index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
    value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
    if (return.index) {
      return(index)
    }
    return(value)
  }

  empty_issues_table <- function() {
    data.frame(
      index = numeric(), value = numeric(), variable = character(),
      has_issue = logical(), issue_type = character()
    )

  }


  ## calculate both normal and log normal outliers for the whole dataframe
  outliers_normal <- data %>% data_validation_outliers_normal()
  outliers_log_normal <- data %>% data_validation_outliers_log_normal()
  outliers <- lapply(names(data), function(x) {
    ## return an empty issues dataframe of issues if no outliers are found
    if ((nrow(outliers_log_normal[[x]]) == 0) & (nrow(outliers_normal[[x]]) ==
                                                 0)) {
      return(empty_issues_table())
    }
    else if (nrow(outliers_log_normal[[x]]) < nrow(outliers_normal[[x]])) { ## for each variable, select the method with fewer outliers

      data.frame(outliers_log_normal[[x]],
                 variable = rep(x, nrow(outliers_log_normal[[x]])), # rep(...,nrow()) makes this work for no rows etc.
                 has_issue = rep(T, nrow(outliers_log_normal[[x]])),
                 issue_type = rep("log normal distribution outlier", nrow(outliers_log_normal[[x]])), stringsAsFactors = FALSE
                 
      )
    }
    else {
      data.frame(outliers_normal[[x]],
                 variable = rep(x, nrow(outliers_normal[[x]])),
                 has_issue = rep(T, nrow(outliers_normal[[x]])),
                 issue_type = rep("normal distribution outlier", nrow(outliers_normal[[x]])), stringsAsFactors = FALSE
      
      )

    }
  }) %>% do.call(rbind, .)
  if (nrow(outliers) == 0) {
    return(empty_issues_table())
  }
  outliers$variable <- as.character(outliers$variable)
  return(outliers)
}
