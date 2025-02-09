
# Make predictions using pika recognizer

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
model = load_model('./model_training_checkpoints/epoch-99.model') #set this to where you've saved the model

#load audio files
audio_files = sorted(glob("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/MD/Jul31/*.wav")) #set this to where the audio files are
audio_files

if __name__ == '__main__':  
    scores = model.predict(audio_files, clip_overlap = 0.15, num_workers = 8, batch_size=256)
    scores.head()
    scores.to_csv("./Predict/predict_score_MD_31Jul2024.csv") #set this to where you want to save the predicitons

############## Don't worry about this next stuff #########################

# Binarize
#def binarize(x, threshold):
    #"""Return a list of 0, 1 by thresholding vector x"""
    #if len(np.shape(x)) > 2:
        #raise ValueError("Shape must be 1-dimensional or 2-dimensional")

    # Check if the array is 2D (which it is in this case)
    #if len(np.shape(x)) == 2:
        #Access the values correctly using .iloc for DataFrame
        #return [1 if x.iloc[i, 0] > threshold else 0 for i in range(len(x))]

    # If it's a 1D array, process each element
    #return [1 if xi > threshold else 0 for xi in x]

#try this for binary: opensoundscape.metrics.predict_multi_target_labels(scores, threshold)
#binary_predictions = pd.Series(binarize(scores['PIKA'], threshold=5), index=scores.index)

#binary_predictions.to_csv('./Predict/predictions_MB_binarized.csv')

#binary_predictions = pd.read_csv('./Predict/predictions_MB_binarized.csv',index_col=[0,1,2])

# Step 2: Merge the DataFrames on the index
#merged_MB_predictions = pd.merge(scores, binary_predictions, left_index=True, right_index=True, how='inner', suffixes=('_file1', '_file2'))

# Step 3: Rename the PIKA columns to more descriptive names
#merged_MB_predictions.rename(columns={'PIKA_file1': 'predict_score', 'PIKA_file2': 'pika_present'}, inplace=True)

#merged_MB_predictions.to_csv('./Predict/predictions_fully_merged.csv')