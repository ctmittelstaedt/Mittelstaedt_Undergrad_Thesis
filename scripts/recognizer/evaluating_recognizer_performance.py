##########################################################################################
# Evaluating the performance of the pika recognizer
# Charlotte Mittelstaedt
# Created 20 December 2024
##########################################################################################

# Import utilities and packages
from opensoundscape.ml.cnn import load_model
from opensoundscape import Audio
import opensoundscape
import torch
from pathlib import Path
import numpy as np
import pandas as pd
from glob import glob
import subprocess
import os
import sklearn.metrics
import matplotlib
from opensoundscape.annotations import BoxedAnnotations

# Set up plotting
from matplotlib import pyplot as plt
plt.rcParams['figure.figsize']=[8,6] #for large visuals
plt.rcParams['font.family'] = 'Arial'

# Load model
model = load_model("./model_training_checkpoints/epoch-99.model")

# Specify test set
## Make a list of all of the selection table files
raven_files = sorted(glob("./raven_data/testing_data_raven/*.txt"))

## Create a list of audio files, one corresponding to each Raven file (Audio files have the same names as selection files with a different extension)
audio_files = sorted(glob("./audio_data/testing_data_audio/*.wav") + glob("./audio_data/testing_data_audio/*.mp3"))

## Create a dataframe of annotations
annotations = BoxedAnnotations.from_raven_files(
    raven_files, 
    "Annotation",
    audio_files, 
    keep_extra_columns=['Selection','View', 'Channel','Begin Time (s)', 'End Time (s)', 'Low Freq (Hz)', 'High Freq (Hz)','Annotation'])

## Access the underlying DataFrame
annotations_data = annotations.df
annotations_data.to_csv("./annotations_data.csv")

## Parameters to use for label creation*changed from 0.4 to 0.3
clip_duration = 0.3
clip_overlap = 0.0
min_label_overlap = 0.07
min_label_fraction = 0.8
species_of_interest = ["PIKA"] 

## Create dataframe of one-hot labels
clip_labels = annotations.clip_labels(
    clip_duration = clip_duration,
    clip_overlap = clip_overlap,
    min_label_overlap = min_label_overlap,
    min_label_fraction = min_label_fraction,
    class_subset = species_of_interest # You can comment this line out if you want to include all species.
)

## Access the underlying DataFrame
clip_labels.to_csv("./clip_labels.csv")

## Select all files from testing_data_raven as a test set
mask = clip_labels.reset_index()['file'].apply(lambda x: 'testing_data_audio' in x).values
test_set = clip_labels[mask]

## Save .csv tables of the training and validation sets to keep a record of them
test_set.to_csv("./test_set.csv")

## Load the table listing files in the test set 
test_set = pd.read_csv('./test_set.csv',index_col=[0,1,2])

## Preprocessing utilities
from opensoundscape.preprocess.preprocessors import SpectrogramPreprocessor
from opensoundscape.ml.datasets import AudioFileDataset, AudioSplittingDataset
from opensoundscape import preprocess
import IPython.display as ipd
from opensoundscape.preprocess.utils import show_tensor, show_tensor_grid
from opensoundscape.ml.utils import collate_audio_samples_to_tensors
from opensoundscape.ml.dataloaders import SafeAudioDataloader

## Uncomment to test the effect of preprocessing on peformance
#preprocessor = SpectrogramPreprocessor(sample_duration=0.3) # sample_duration must match clip_duration
#train_dataset = AudioFileDataset(test_set, preprocessor)
#preprocessor.pipeline
#preprocessor.pipeline.bandpass.set(min_f=700,max_f=6000) # eliminate unnecessary frequencies
#preprocessor.pipeline.to_spec.params['overlap_fraction'] = 0.9 # fractional temporal overlap between consecutive windows
#preprocessor.pipeline.to_spec.params

## Subset the labels to only those the model was trained on
test_set = test_set[model.classes]

# Run "inference": use the CNN to predict the presence of each class in the audio clips
predictions = model.predict(test_set,
                            clip_overlap = 0.15,
                            num_workers=0,
                            batch_size=1000
)

# Save predictions
predictions.to_csv('./cnn_predictions_test_set.csv')

predictions = pd.read_csv('./cnn_predictions_test_set.csv',index_col=[0,1,2])

# Define a function for binarizing predictions
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

#Binarize predictions
binary_predictions = pd.Series(binarize(predictions['PIKA'], threshold=10), index=predictions.index) # Set threshold
predictions['Binary_Predictions'] = binary_predictions
predictions.to_csv('./cnn_predictions_test_set_binarized.csv')
predictions = pd.read_csv('./cnn_predictions_test_set_binarized.csv',index_col=[0,1,2])

# Merge the DataFrames on the index
merged_df = pd.merge(test_set, predictions, left_index=True, right_index=True, how='inner', suffixes=('_file1', '_file2'))

# Rename the PIKA columns to more descriptive names
merged_df.rename(columns={'PIKA_file1': 'pika_present', 'PIKA_file2': 'predict_score'}, inplace=True)
merged_df.rename(columns={'Binary_Predictions': 'binary_predictions'}, inplace=True)
merged_df.to_csv('./predictions_fully_merged.csv')

# Generating evaluation metrics
from sklearn import metrics

# Define 'actual' as the 'pika_present' column, converting TRUE/FALSE to 1/0
df = pd.read_csv('./predictions_fully_merged.csv')

actual = df['pika_present'].astype(int)

# Define 'predicted' as the 'binary_score' column
predicted = df['binary_predictions']

# Define 'score' as the 'predict_score' column
score = df['predict_score']

# For plot colours
import cmocean

# Create confusion matrix
confusion_matrix = metrics.confusion_matrix(actual, predicted)

import matplotlib.pyplot as plt
import numpy as np
import cmcrameri.cm as cmc

cm_display = metrics.ConfusionMatrixDisplay(confusion_matrix = confusion_matrix, display_labels = ['No pika', 'Pika'])
cm_display.plot(cmap='cmc.batlow')
for label in cm_display.ax_.texts:  # 'texts' contains the numbers in the matrix
    label.set_fontsize(16)
for label in cm_display.ax_.get_xticklabels():
    label.set_fontsize(16)  # Adjust font size
for label in cm_display.ax_.get_yticklabels():
    label.set_fontsize(16)  
cbar = cm_display.im_.colorbar  # Access colorbar from the im_ attribute

cbar.ax.tick_params(labelsize=16)
plt.xlabel('Predicted label', fontsize=21, labelpad=10)  # Adjust fontsize and labelpad for X-axis
plt.ylabel('True label', fontsize=21, labelpad=10)
plt.show()


# Calculate metrics
from sklearn.metrics import f1_score, precision_score, recall_score, average_precision_score

f1 = f1_score(actual, predicted)
print('f1:', f1)

precision = precision_score(actual, predicted)
print('precision:', precision)

recall = recall_score(actual, predicted)
print('recall:', recall)

# Precision recall over threshold plot
from sklearn.metrics import precision_recall_curve

precisions, recalls, thresholds = sklearn.metrics.precision_recall_curve(y_true=actual, y_score=score, pos_label=1, sample_weight=None, drop_intermediate=False, probas_pred='deprecated')

plt.plot(thresholds, precisions[:-1], label='Precision', color='#ee9e72', linewidth=4)  # We exclude the last threshold value
plt.plot(thresholds, recalls[:-1], label='Recall', color='#2f6260', linewidth=4)      # Same for recall, to match threshold length

plt.xlabel('Threshold', fontsize=21, labelpad=10)
plt.ylabel('Score', fontsize=21, labelpad=10)
plt.tick_params(axis='both', which='major', labelsize=16)

plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.gca().spines['left'].set_visible(True)
plt.gca().spines['bottom'].set_visible(True)
plt.gca().spines['left'].set_linewidth(2) 
plt.gca().spines['bottom'].set_linewidth(2)

plt.legend(loc='best', frameon=False, fontsize=16)
plt.show()

# Precision recall curve
plt.plot(recalls, precisions, label='Precision-Recall Curve', color='#1a3f60', linewidth=4)

plt.xlabel('Recall', fontsize=21, labelpad=10)
plt.ylabel('Precision', fontsize=21, labelpad=10)
plt.tick_params(axis='both', which='major', labelsize=16)

plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.gca().spines['left'].set_linewidth(2) 
plt.gca().spines['bottom'].set_linewidth(2)

plt.show()

# AUC score
from sklearn.metrics import auc

# Calculate AUC (area under the curve)
auc_pr = auc(recalls, precisions)  # Exclude last precision value to match lengths
print(f'Area Under Precision-Recall Curve (AUC-PR): {auc_pr}')


# Histogram for predictions
fig, axs = plt.subplots(1,1)
axs = np.ravel(axs)
for ax, species in enumerate(model.classes):
    positives = test_set[species] == 1
    negatives = test_set[species] == 0
    axs[ax].hist(predictions[positives][species], alpha=0.5, color="#ee9e72", label="Positives")
    axs[ax].hist(predictions[negatives][species], alpha=0.5, color="#2f6260", label="Negatives")
    axs[ax].set_yscale("log")
    axs[ax].set_ylabel("Number of audio clips", fontsize = 21, labelpad=10)
    axs[ax].set_xlabel("Prediction score", fontsize = 21, labelpad=10)
    axs[ax].legend(frameon=False, fontsize = 16)
    axs[ax].tick_params(axis='y', which='minor', left=False)
    axs[ax].tick_params(axis='x', labelsize=16)  # For x-axis
    axs[ax].tick_params(axis='y', labelsize=16)


plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.gca().spines['left'].set_visible(True)
plt.gca().spines['bottom'].set_visible(True)
plt.gca().spines['left'].set_linewidth(2) 
plt.gca().spines['bottom'].set_linewidth(2)
plt.show()