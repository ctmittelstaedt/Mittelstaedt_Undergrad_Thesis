########################################################################################
# Modifying audio parameters in spectrograms
# A script to view spectrograms and preprocess audio by modifying parameters before using 
# as training data for pika recognizer
# Author: Charlotte Mittelstaedt - University of British Columbia - using Opensoundscape code
# Created 3 November 2024
########################################################################################

# Set the backend and import matplotlib
import os
os.environ['MPLBACKEND'] = 'TkAgg'  # or 'Agg', 'Qt5Agg', etc.

import matplotlib.pyplot as plt

import matplotlib

# Choose the backend 
matplotlib.use('TkAgg')  

# Import packages
from opensoundscape import Audio, Spectrogram
from pathlib import Path

# Load audio file
audio_object = Audio.from_file("C:\\Users\\User\\Desktop\\All_annotations_all_pika\\MD_training data\\PIKARU14\\PIKARU14_20240731_162002.wav")
audio_object

# Trim audio to centre pika call in the spectrogram frame and apply bandpass filter
trimmed = audio_object.trim(279.6,280).bandpass(700,15000,order=10)

# Create a spectrogram object
spectrogram_object = Spectrogram.from_audio(trimmed,
                                            overlap_fraction=0.9,
                                            )

# Plot the spectrogram
spectrogram_object.plot()
plt.show()  # Display the plot

