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
audio_files = glob("./audio_data/*/*.mp3")

#load the table listing files in the test set 
test_set = pd.read_csv('./All_annotations_copy/test_set.csv',index_col=[0,1,2])

from opensoundscape import Audio

# Check the length of each audio file before processing
for file in audio_files:
    audio = Audio.from_file(file)
    print(f"File: {file}, Length: {audio.duration}s")
    if audio.duration < 0.3:
        print(f"Warning: {file} is shorter than 0.3s")
        
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
fig, axs = plt.subplots(7,1, figsize = (10,40))
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