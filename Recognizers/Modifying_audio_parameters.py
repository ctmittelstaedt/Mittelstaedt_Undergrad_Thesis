########################################################################################
# Modifying audio parameters in spectrograms
# A script to view spectrograms and preprocess audio by modifying parameters before using 
# as training data for pika recognizer
# Author: Charlotte Mittelstaedt - University of British Columbia - using Opensoundscape code
# Created 3 November 2024
########################################################################################

# Set the backend and import matplotlib
import os
os.environ['MPLBACKEND'] = 'TkAgg' 

import matplotlib.pyplot as plt
import matplotlib

# Choose the backend 
matplotlib.use('TkAgg')  

# Import packages
from opensoundscape import Audio, Spectrogram
from pathlib import Path

# Load audio file
audio_object = Audio.from_file("C:/Users/User/Desktop/temp_test/PIKARU25_20240808_212002.wav")
audio_object

# Trim audio to centre pika call in the spectrogram frame and apply bandpass filter
trimmed = audio_object.trim(15.98,16.38)

# Create a spectrogram object
spectrogram_object = Spectrogram.from_audio(trimmed,
                                            overlap_fraction=0.9,
                                            )

# Remove higher frequencies
spectrogram_object = spectrogram_object.bandpass(700,15000)

# Plot the spectrogram
spectrogram_object.plot()
plt.show()  # Display the plot

