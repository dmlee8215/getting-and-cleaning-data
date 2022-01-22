# getting-and-cleaning-data
Course project for the JH Getting and Cleaning Data course

## Files
  - run_analysis.R: combined data cleaning script, can be sourced as is
  assuming the zip has been downloaded in the current working directory as Dataset.zip
  - cleaned_data.csv: output data file, with 66 metrics from the original data
  (those marked as mean or standard deviation), the subject_id, and the activity description.
  These metrics are all averaged to produce one row per activity and subject.
  - code_book.csv: programatically produced by run_analysis.R, describes all columns in the cleaned data.
  
## Notes
  - The original intent was to go above and beyond the assignment requirements, and have an option to export more columns if desired. The parameter keep_only_mean_std when set to FALSE was supposed to cleanly be able to do this. On further investigation I found that several of the columns in the underlying data are described as having the same column name, that is several of the columns repeat names (particularly for energy band) while having different data. I have not found an adequate explanation for what this means, and it is not possible to have identical column names in the cleaned data frame. As this is not required for the assignment, I decided it was enough to leave this as a TODO for some point.
