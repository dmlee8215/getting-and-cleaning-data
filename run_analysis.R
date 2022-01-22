# set data_path to the path of the downloaded zip on your system
data_path <- "Dataset.zip"

# keep_only_mean_std is an indicator of whether to eliminate x column not like mean() or std()
# the assignment has true, but an analysis which includes all column just needs to set this as false
# NOTE: as far as I can tell there is inconsistency in the column names for some fields, energy band in particular
# setting this to FALSE will not work until this is resolved
keep_only_mean_std <- TRUE

# helper function for processing field names later on
# does sub, but if there is no match return NA (rather than original as sub does)
sub_with_na <- function(pattern, replacement, x, ...) {
  matches <- grepl(pattern, x, ...)
  y <- ifelse(matches, sub(pattern, replacement, x, ...), NA_character_)
  
  y
}

# lookup to be used later for displaying metrics
metric_lookup <- data.frame(
  metric = c("mean","std","mad","max","min","sma","energy","iqr","entropy",
             "arcoeff","correlation","maxinds","meanfreq","skewness",
             "kurtosis","bandsenergy","angle"),
  metric_description = c("mean of ","standard deviation of ","median absolute deviation of ",
                         "maximum of ","minimum of ","signal magnitude area of ",
                         "energy measure of ","interquartile range of ",
                         "signal entropy of ","autoregression coefficient ",
                         "correlation of ","index of largest frequency component for ",
                         "weighted frequency average of ","frequency distribution skewness for ",
                         "frequency distribution skewness for ",
                         "energy of band ","angle between ")
)

# unzip source file
dir <- tempdir()
unzip(data_path, exdir = dir)

# load raw objects
x_test <- read.table(file.path(dir, "UCI HAR Dataset", "test", "X_test.txt"))
x_train <- read.table(file.path(dir, "UCI HAR Dataset", "train", "X_train.txt"))
y_test <- read.table(file.path(dir, "UCI HAR Dataset", "test", "y_test.txt"))
y_train <- read.table(file.path(dir, "UCI HAR Dataset", "train", "y_train.txt"))
subject_test <- read.table(file.path(dir, "UCI HAR Dataset", "test", "subject_test.txt"))
subject_train <- read.table(file.path(dir, "UCI HAR Dataset", "train", "subject_train.txt"))
features <- read.table(file.path(dir, "UCI HAR Dataset", "features.txt"))
activity_labels <- read.table(file.path(dir, "UCI HAR Dataset", "activity_labels.txt"))

# combine test and train datasets
x <- rbind(x_test,x_train)
y <- rbind(y_test,y_train)
subject <- rbind(subject_test,subject_train)

# rename some of the columns for easier processing
colnames(features) <- c("original_position", "original_name")
colnames(activity_labels) <- c("label", "activity")
colnames(y) <- "label"

# join in activity to the labels
y <- inner_join(y, activity_labels, by = "label")

# combine data frames
df <- cbind(x, subject, y)

# work on a dictionary dataset, both for documentation and for column names
# dictionary dataset will contain all columns in the original dataset
# with original and final names, original and final position

# first manually create dictionary for the subject and the y parts of the data
# final position will be calculated from the whole dictionary based on the included field
# these column names will not be changed
subject_y_dictionary <- data.frame(
  original_position = nrow(features) + 1:3, 
  original_name = c("subject_id", "label", "activity"),
  column_name = c("subject_id", "label", "activity"), 
  column_description = c(
    "Identifier of the subject under observation",
    "Activity label code",
    "Activity description"),
  column_type = c("integer", "integer", "character"),
  included = c(TRUE, FALSE, TRUE)
  )

# now add in the column_name, column_description, column_type, and included fields to features
# NA for now, will be calculated later
features$column_name <- NA_character_
features$column_description <- NA_character_
features$column_type <- "double"
features$included <- NA

# create initial dictionary dataset
dictionary <- rbind(features, subject_y_dictionary)


# derive the following characteristics of the metrics (aids both creating the new column names and the description):
# time or frequency(domain)
# body vs gravity (body_gravity)
# X,Y,Z (component)
# whether a magnitude
# linear acceleration or angular velocity (linear_angular)
# jerk (derivative)
# mean,std,mad,max,min,sma,energy,iqr,entropy,arCoeff,correction,maxInds,meanFreq,skewness,kurtosis,bandsEnergy,angle (metric)
# energy band (band)
# autoregression coefficient (autoreg)
# correlation X,Y X,Z Y,Z (correlation)
# angle vector 1 and 2 (vector1, vector2)

# first fix some inconsistency in field names, lowercase
dictionary$fixed_name <- str_to_lower(sub("),", ",", sub("maxInds", "maxInds()", dictionary$original_name)))

# calculate the metric type
dictionary$metric <- sub_with_na("^.*\\b(\\w+)\\(.*\\).*$", "\\1", dictionary$fixed_name)
dictionary$metric_abbr <- ifelse(is.na(dictionary$metric), "", paste0(dictionary$metric, "_"))
dictionary <- left_join(dictionary, metric_lookup, by = "metric")

# calculate the domain
dictionary$domain <- sub_with_na("^([ft]).*$", "\\1", dictionary$fixed_name)
dictionary <- dictionary %>% mutate(
  domain = case_when(
    domain == "t" ~ "time",
    domain == "f" ~ "frequency"
    )
)
dictionary$domain_abbr <- ifelse(is.na(dictionary$domain), "", paste0(dictionary$domain, "_"))
dictionary$domain_description <- ifelse(is.na(dictionary$domain), "", paste(dictionary$domain, "domain "))

# calculate body_gravity
dictionary$body_gravity <- sub_with_na("^[ft](body|gravity).*$", "\\1", dictionary$fixed_name)
dictionary$body_gravity_abbr <- ifelse(is.na(dictionary$body_gravity), "", paste0(dictionary$body_gravity, "_"))
dictionary$body_gravity_description <- ifelse(is.na(dictionary$body_gravity), "", paste0(dictionary$body_gravity, " "))

# calculate component
dictionary$component <- sub_with_na("^.+-([xyz])(,[1-4])?$", "\\1", dictionary$fixed_name)
dictionary$component_abbr <- ifelse(is.na(dictionary$component), "", paste0(dictionary$component, "_"))
dictionary$component_description <- ifelse(is.na(dictionary$component), "", paste("in the", dictionary$component, "direction"))

# calculate whether magnitude
dictionary$magnitude <- sub_with_na("^.+(mag)-.+$", "\\1", dictionary$fixed_name)
dictionary$magnitude_abbr <- ifelse(is.na(dictionary$magnitude), "", "mag_")
dictionary$magnitude_description <- ifelse(is.na(dictionary$magnitude), "", "magnitude ")

# calculate linear_angular
dictionary$linear_angular <- sub_with_na("^\\w+(acc|gyro).+$", "\\1", dictionary$fixed_name)
dictionary <- dictionary %>% mutate(
  linear_angular_abbr = case_when(
    linear_angular == "acc" ~ "acceleration_",
    linear_angular == "gyro" ~ "angular_",
    TRUE ~ "" #must be NA
  ),
  linear_angular_desc = case_when(
    linear_angular == "acc" ~ "acceleration ",
    linear_angular == "gyro" ~ "angular velocity ",
    TRUE ~ ""
  )
)

# calculate whether derivative
dictionary$derivative <- grepl("^\\w+jerk", dictionary$fixed_name)
dictionary$derivative_abbr <- ifelse(dictionary$derivative, "deriv_", "")
dictionary$derivative_description <- ifelse(dictionary$derivative, "derivative ", "")

# calculate energy band
dictionary$band <- sub_with_na("^.+energy\\(\\)-([0-9,]+)$", "\\1", dictionary$fixed_name)
dictionary$band_abbr <- ifelse(is.na(dictionary$band), "", paste0(sub(",", "_", dictionary$band), "_"))
dictionary$band_description <- ifelse(is.na(dictionary$band), "", paste(dictionary$band, "of "))

# calculate autoregression coefficient
dictionary$autoreg <- sub_with_na("^.+,([1-4])$", "\\1", dictionary$fixed_name)
dictionary$autoreg_abbr <- ifelse(is.na(dictionary$autoreg), "", paste0(dictionary$autoreg, "_"))
dictionary$autoreg_description <- ifelse(is.na(dictionary$autoreg), "", paste(dictionary$autoreg, "of "))

# calculate correlation
dictionary$correlation <- sub_with_na("^.+([xyz],[xyz])$", "\\1", dictionary$fixed_name)
dictionary$correlation_abbr <- ifelse(is.na(dictionary$correlation), "", paste0(sub(",", "", dictionary$correlation), "_"))
dictionary$correlation_description <- ifelse(is.na(dictionary$correlation), "", paste0(sub(",", "", dictionary$correlation), " "))

# calculate vectors
dictionary$vectors <- sub_with_na("^.+\\((\\w+,\\w+)\\)$", "\\1", dictionary$fixed_name)
dictionary$vectors_abbr <- ifelse(is.na(dictionary$vectors), "", sub(",", "_", dictionary$vectors))
dictionary$vectors_description <- ifelse(is.na(dictionary$vectors), "", sub(",", " and ", dictionary$vectors))

# here are several examples worked by hand of what we want column names and descriptions to be
# first line original column name, second line new name, third description

# tBodyAcc-mean()-X
# mean_time_body_acceleration_x
# mean of time domain body acceleration in the x direction

# angle(tBodyGyroMean,gravityMean)
# angle_tbodygyromean_gravitymean
# angle between tbodygyromean and gravitymean

# tBodyGyroJerk-correlation()-X,Y
# xy_correlation_time_body_angular_deriv
# xy correlation of time domain body angular velocity derivative

# tBodyGyroJerk-arCoeff()-Y,3
# arcoeff_3_time_body_angular_deriv_y
# autoregression coefficient 3 of time domain body angular velocity derivative in the y direction

# fBodyAccJerk-maxInds-X
# maxinds_freq_body_acceleration_x
# index of largest frequency component for frequency domain body acceleration in the x direction

# fBodyBodyGyroMag-std()
# std_freq_body_angular_mag
# standard deviation of frequency domain body angular velocity magnitude

# fBodyAccJerk-bandsEnergy()-57,64
# bandsenergy_57_64_freq_body_acceleration_deriv
# energy of band 57,64 of frequency domain body acceleration derivative

# order is:
# correlation/metric/band/autoreg/vectors/domain/body_gravity/linear_angular/magnitude/component/derivative

# derive column_name and description from the metric characteristics
dictionary <- dictionary %>% mutate(
  column_name = coalesce(column_name,
                         paste0(correlation_abbr,metric_abbr,band_abbr,autoreg_abbr,
                                vectors_abbr,domain_abbr,body_gravity_abbr,
                                linear_angular_abbr,magnitude_abbr,
                                component_abbr,derivative_abbr)),
  column_description = coalesce(column_description,
                         paste0(correlation_description,metric_description,band_description,autoreg_description,
                                vectors_description,domain_description,body_gravity_description,
                                linear_angular_desc,magnitude_description,
                                component_description,derivative_description))
)

# remove trailing spaces and underscore
dictionary$column_name <- sub("_$", "", dictionary$column_name)
dictionary$column_description <- sub(" $", "", dictionary$column_description)

# fill in whether column is to be included, set new column position
# if keeping only mean or std default included to whether the metric is one of those
# otherwise default to TRUE if not otherwise present
if(keep_only_mean_std) {
  dictionary$included <- ifelse(
    is.na(dictionary$included),
    dictionary$metric %in% c("mean", "std"),
    dictionary$included
    )
} else { # as noted above this will not work until underlying column name inconsistencies are taken care of
  dictionary$included <- ifelse(
    is.na(dictionary$included),
    TRUE,
    dictionary$included
    )
}

dictionary$column_position <- row_number(ifelse(dictionary$included, dictionary$original_position, NA_integer_))
dictionary <- filter(dictionary, included) %>% 
  select(column_position, column_name, column_type, column_description, original_name, original_position)

# select only columns to include in df, set column names
df <- df[,dictionary$original_position]
colnames(df) <- dictionary$column_name

# now create aggregated dataset
cleaned_data <- df %>% group_by(subject_id, activity) %>% summarise_all(mean)

# can now write outputs
# first write aggregate df as cleaned_data.csv
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)

# then write dictionary as code_book.csv
write.csv(dictionary, "code_book.csv", row.names = FALSE)
