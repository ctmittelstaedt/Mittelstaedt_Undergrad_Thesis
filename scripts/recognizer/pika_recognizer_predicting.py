########################################################################################
# Making predictions with the pika recognizer
# Charlotte Mittelstaedt - University of British Columbia
# Created 1 January 2025
########################################################################################

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

# Set up plotting
from matplotlib import pyplot as plt
plt.rcParams['figure.figsize']=[15,5] # For large visuals

# Load model
model = load_model('./model_training_checkpoints/epoch-99.model') # Set this to where you've saved the model

# Load audio files
audio_files = sorted(glob("C:/PythonGitHub/Mittelstaedt_Undergrad_Thesis/Recognizers/Predict/BC/*.wav")) # Set this to where the audio files are
audio_files

if __name__ == '__main__':  
    scores = model.predict(audio_files, clip_overlap = 0.15, num_workers = 8, batch_size=256)
    scores.head()
    scores.to_csv("./predict_score_BC_7Aug2024_ARU16-19.csv") # Set this to where you want to save the predicitons