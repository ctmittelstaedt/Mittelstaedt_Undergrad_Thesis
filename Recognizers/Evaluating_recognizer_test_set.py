#TESTING
from opensoundscape.ml.cnn import load_model
from opensoundscape import Audio
import opensoundscape

# Other utilities and packages
import torch
from pathlib import Path
import numpy as np
import pandas as pd
from glob import glob
import subprocess
import os


#set up plotting
from matplotlib import pyplot as plt
plt.rcParams['figure.figsize']=[15,5] #for large visuals

#load model
#model = load_model("./model_training_checkpoints/epoch-99.model")
#model = load_model("C:/Users/User/Desktop/best.model")

##################From recognizer code to create test set so that I can play around with it.
# Make a list of all of the selection table files
#raven_files = sorted(glob("./raven_data/*/*.txt"))

# Create a list of audio files, one corresponding to each Raven file (Audio files have the same names as selection files with a different extension)
#audio_files = sorted(glob("./audio_data/*/*.wav") + glob("./audio_data/*/*.mp3"))

from opensoundscape.annotations import BoxedAnnotations
# Create a dataframe of annotations
#annotations = BoxedAnnotations.from_raven_files(
    #raven_files, 
    #"Annotation",
    #audio_files, 
    #keep_extra_columns=['Selection','View', 'Channel','Begin Time (s)', 'End Time (s)', 'Low Freq (Hz)', 'High Freq (Hz)','Annotation'])

# Access the underlying DataFrame
#annotations_data = annotations.df
#annotations_data.to_csv("./All_annotations_copy/annotations_data.csv")

# Parameters to use for label creation*changed from 0.4 to 0.3
#clip_duration = 0.3
#clip_overlap = 0.0
#min_label_overlap = 0.07
#min_label_fraction = 0.8
#species_of_interest = ["PIKA"] 

# Create dataframe of one-hot labels
#clip_labels = annotations.clip_labels(
    #clip_duration = clip_duration,
    #clip_overlap = clip_overlap,
    #min_label_overlap = min_label_overlap,
    #min_label_fraction = min_label_fraction,
    #class_subset = species_of_interest # You can comment this line out if you want to include all species.
#)

# Access the underlying DataFrame
#clip_labels.to_csv("./All_annotations_copy/clip_labels.csv")

# Select all files from testing_data_raven as a test set
#mask = clip_labels.reset_index()['file'].apply(lambda x: 'testing_data_audio' in x).values
#test_set = clip_labels[mask]

# All other files will be used as a training set
#train_and_val_set = clip_labels.drop(test_set.index)

# Save .csv tables of the training and validation sets to keep a record of them
#test_set.to_csv("./All_annotations_copy/test_set.csv")

#load the table listing files in the test set 
test_set = pd.read_csv('./All_annotations_copy/test_set.csv',index_col=[0,1,2])


#####PREPROCESS
from opensoundscape.preprocess.preprocessors import SpectrogramPreprocessor
from opensoundscape.ml.datasets import AudioFileDataset, AudioSplittingDataset
from opensoundscape import preprocess
import IPython.display as ipd
from opensoundscape.preprocess.utils import show_tensor, show_tensor_grid
from opensoundscape.ml.utils import collate_audio_samples_to_tensors
from opensoundscape.ml.dataloaders import SafeAudioDataloader

#preprocessor = SpectrogramPreprocessor(sample_duration=0.3) # sample_duration must match clip_duration
#train_dataset = AudioFileDataset(test_set, preprocessor)
#preprocessor.pipeline
#preprocessor.pipeline.bandpass.set(min_f=700,max_f=6000) # eliminate unnecessary frequencies
#preprocessor.pipeline.to_spec.params['overlap_fraction'] = 0.9 # fractional temporal overlap between consecutive windows
#preprocessor.pipeline.to_spec.params

#############################################

from opensoundscape import Audio

# subset the labels to only those the model was trained on
#test_set = test_set[model.classes]

# run "inference": use the CNN to predict the presence of each class in the audio clips
#predictions = model.predict(test_set,
                            #clip_overlap = 0.15,
                            #num_workers=0,
                            #batch_size=1000
#)

# save predictions
#predictions.to_csv('./All_annotations_copy/cnn_predictions_test_set.csv')

predictions = pd.read_csv('./All_annotations_copy/cnn_predictions_test_set.csv',index_col=[0,1,2])


#print(predictions.head())


# Binarize
def binarize(x, threshold):
    """Return a list of 0, 1 by thresholding vector x"""
    if len(np.shape(x)) > 2:
        raise ValueError("Shape must be 1-dimensional or 2-dimensional")

    # Check if the array is 2D (which it is in this case)
    if len(np.shape(x)) == 2:
        #Access the values correctly using .iloc for DataFrame
        return [1 if x.iloc[i, 0] > threshold else 0 for i in range(len(x))]

    # If it's a 1D array, process each element
    return [1 if xi > threshold else 0 for xi in x]

#try this for binary: opensoundscape.metrics.predict_multi_target_labels(scores, threshold)
binary_predictions = pd.Series(binarize(predictions['PIKA'], threshold=5), index=predictions.index)

predictions['Binary_Predictions'] = binary_predictions

predictions.to_csv('./All_annotations_copy/cnn_predictions_test_set_binarized.csv')

predictions = pd.read_csv('./All_annotations_copy/cnn_predictions_test_set_binarized.csv',index_col=[0,1,2])

# Step 2: Merge the DataFrames on the index
merged_df = pd.merge(test_set, predictions, left_index=True, right_index=True, how='inner', suffixes=('_file1', '_file2'))

# Step 3: Rename the PIKA columns to more descriptive names
merged_df.rename(columns={'PIKA_file1': 'pika_present', 'PIKA_file2': 'predict_score'}, inplace=True)

# Step 4: Optionally, rename the 'Binary_Predictions' column from predictions to something more descriptive
merged_df.rename(columns={'Binary_Predictions': 'binary_predictions'}, inplace=True)

merged_df.to_csv('./All_annotations_copy/predictions_fully_merged.csv')

#######################
# Generating evaluation metrics
from sklearn import metrics

# Define 'actual' as the 'pika_present' column, converting TRUE/FALSE to 1/0
# Assuming TRUE is represented as `True` and FALSE as `False` in the 'pika_present' column
df = pd.read_csv('./All_annotations_copy/predictions_fully_merged.csv')

actual = df['pika_present'].astype(int)

# Define 'predicted' as the 'binary_score' column
predicted = df['binary_predictions']

# Define 'score' as the 'predict_score' column
score = df['predict_score']

confusion_matrix = metrics.confusion_matrix(actual, predicted)

import matplotlib.pyplot as plt
import numpy as np

cm_display = metrics.ConfusionMatrixDisplay(confusion_matrix = confusion_matrix, display_labels = ['absent', 'present'])
cm_display.plot()
plt.show()


# Function to evaluate model's performance with wandb (F1, Precision, Recall)
from sklearn.metrics import f1_score, precision_score, recall_score, average_precision_score

f1 = f1_score(actual, predicted)
print('f1:', f1)

precision = precision_score(actual, predicted)
print('precision:', precision)

recall = recall_score(actual, predicted)
print('recall:', recall)

#mAP = average_precision_score(actual, score)
#print('mAP:', mAP)

# The average ROC AUC, precision-recall  AUC, average maximum F1 scores, optimal threshold values, and average precision  and recall scores associated with the optimal threshold are listed in Table 3.

# Histogram for predictions
fig, axs = plt.subplots(1,1, figsize = (10,20))
axs = np.ravel(axs)
for ax, species in enumerate(model.classes):
    positives = test_set[species] == 1
    negatives = test_set[species] == 0
    axs[ax].hist(predictions[positives][species], alpha=0.5, color="red", label="Positives")
    axs[ax].hist(predictions[negatives][species], alpha=0.5, color="blue", label="Negatives")
    axs[ax].set_yscale("log")
    axs[ax].set_ylabel("Number of audio segments")
    axs[ax].set_xlabel("Score")
    axs[ax].legend()

plt.tight_layout(pad=9.0)
plt.show()