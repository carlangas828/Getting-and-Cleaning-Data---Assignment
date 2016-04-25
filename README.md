# Getting-and-Cleaning-Data---Assignment

The run_analysis.R script
- Reads in the features.txt file as a vector to use as the column names
- Reads in the test and train data sets
- Reads in the test and train Activity ID (y) and the Subject ID files
- Extracts only the meand and standard deviation columns
- Adds the Activity and Subject ID to the xtrain and xtest main data frames
- Groups by the activityid and subjectid before merging to avoid duplication
- Merges the two datasets
- Merge in the Activity labels
- Finally, creates the final summary and the file for submsission