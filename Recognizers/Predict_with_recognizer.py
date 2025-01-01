
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
model = load_model('./model_training_checkpoints/best.model')

#load audio files
audio_files = glob("./audio_data/*/*.mp3")
audio_files

#Testing that opensoundscape is loading the files correctly
for file in audio_files:
    audio = Audio.from_file(file, sample_rate=44100)
    print(f"File: {file}, Length: {audio.duration} seconds")

audio = Audio.from_file("./audio_data/testing_data_audio/CWS-NOR_EG-CARIB-A3_20190718_074500.mp3", sample_rate=44100)
print(f"Audio loaded: {audio}")
print(f"Duration: {audio.duration} seconds")
print(f"Sample rate: {audio.sample_rate}")
print(f"Samples: {audio.samples[:10]}")
    
scores = model.predict(audio_files, clip_overlap_fraction=0.2)
scores.head()
scores.to_csv("./All_annotations_copy/predict_score.csv")