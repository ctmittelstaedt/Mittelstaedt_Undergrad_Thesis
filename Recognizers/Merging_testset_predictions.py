import pandas as pd

# Step 1: Read the CSV files into DataFrames
file1 = pd.read_csv('./All_annotations_copy/test_set.csv',index_col=[0,1,2])
file2 = pd.read_csv('./All_annotations_copy/cnn_predictions_test_set.csv',index_col=[0,1,2])  # Adjust with your file path

# Step 2: Merge the files on the shared columns
merged_df = pd.merge(file1, file2, left_index=True, right_index=True, how='inner', suffixes=('_file1', '_file2'))

# Step 3: Optionally rename the 'PIKA' columns to something more descriptive
# In this case, 'PIKA_file1' and 'PIKA_file2' are renamed columns
merged_df.rename(columns={'PIKA_file1': 'pika_present', 'PIKA_file2': 'predict_score'}, inplace=True)

# Step 4: Save the merged DataFrame to a new CSV file if desired
merged_df.to_csv('./All_annotations_copy/merged_testset_predictions.csv', index=True)
