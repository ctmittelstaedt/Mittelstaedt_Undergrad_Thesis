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

#set up plotting
from matplotlib import pyplot as plt
plt.rcParams['figure.figsize']=[15,5] #for large visuals

#load model
model = load_model('model_training_checkpoints/best.model')

#load audio files
#audio_files = glob("./audio_data/*/*.mp3")

##################
# Make a list of all of the selection table files
raven_files = glob("./raven_data/*/*.txt")


# Create a list of audio files, one corresponding to each Raven file (Audio files have the same names as selection files with a different extension)
audio_files = glob("./audio_data/*/*.wav") + glob("./audio_data/*/*.mp3")

from opensoundscape.annotations import BoxedAnnotations
# Create a dataframe of annotations
annotations = BoxedAnnotations.from_raven_files(
    raven_files, 
    "Annotation",
    audio_files, 
    keep_extra_columns=['Selection','View', 'Channel','Begin Time (s)', 'End Time (s)', 'Low Freq (Hz)', 'High Freq (Hz)','Annotation'])


# Access the underlying DataFrame
annotations_data = annotations.df
annotations_data.to_csv("./All_annotations_copy/annotations_data.csv")

# Parameters to use for label creation*changed from 0.4 to 0.3
clip_duration = 0.3
clip_overlap = 0.15
min_label_overlap = 0.07
species_of_interest = ["PIKA"] 

# Create dataframe of one-hot labels
clip_labels = annotations.clip_labels(
    clip_duration = clip_duration,
    clip_overlap = clip_overlap,
    min_label_overlap = min_label_overlap,
    class_subset = species_of_interest # You can comment this line out if you want to include all species.
)

# Access the underlying DataFrame
clip_labels.to_csv("./All_annotations_copy/clip_labels.csv")

# Select all files from testing_data_raven as a test set
mask = clip_labels.reset_index()['file'].apply(lambda x: 'testing_data_audio' in x).values
test_set = clip_labels[mask]

# All other files will be used as a training set
train_and_val_set = clip_labels.drop(test_set.index)

# Save .csv tables of the training and validation sets to keep a record of them
test_set.to_csv("./All_annotations_copy/test_set.csv")

#load the table listing files in the test set 
test_set = pd.read_csv('./All_annotations_copy/test_set.csv',index_col=[0,1,2])

#################

#*balancing the negatives and positives so there are 2x negatives as positives
from opensoundscape.data_selection import resample
# Identify positives (rows where any column is TRUE)
positives = test_set[test_set['PIKA'] > 0]

# Identify negatives (rows where any column is FALSE)
negatives = test_set[test_set['PIKA'] == 0]

# Check how many positives and negatives there are
num_positives = len(positives)
num_negatives = len(negatives)

# Now sample the negatives to achieve twice as many negatives as positives
num_negatives_to_sample = min(2 * num_positives, num_negatives)

# Sample exactly `num_negatives_to_sample` negatives without replacement
negatives_downsampled = negatives.sample(num_negatives_to_sample, replace=False, random_state=0)

# Concatenate positives and downsampled negatives into a balanced training set
test_set = pd.concat([positives, negatives_downsampled])
#############################################

from opensoundscape import Audio

# subset the labels to only those the model was trained on
test_set = test_set[model.classes]

print(test_set.shape)
print(test_set.head())

# run "inference": use the CNN to predict the presence of each class in the audio clips
predictions = model.predict(test_set,num_workers=0,batch_size=4)

# save predictions if desired
predictions.to_csv('./All_annotations_copy/cnn_predictions_test_set.csv')

predictions = pd.read_csv('./All_annotations_copy/cnn_predictions_test_set.csv',index_col=[0,1,2])

import matplotlib.pyplot as plt
import numpy as np
fig, axs = plt.subplots(1,1, figsize = (10,40))
axs = np.ravel(axs)
for ax, species in enumerate(model.classes):
    positives = test_set[species] == 1
    negatives = test_set[species] == 0
    axs[ax].hist(predictions.loc[positives][species], alpha=0.5, color="red", label="Positives")
    axs[ax].hist(predictions.loc[negatives][species], alpha=0.5, color="blue", label="Negatives")
    axs[ax].set_yscale("log")
    axs[ax].title.set_text(species)
    axs[ax].set_ylabel("Number of audio segments")
    axs[ax].set_xlabel("Score")
    axs[ax].legend()

    plt.tight_layout()
    plt.show()