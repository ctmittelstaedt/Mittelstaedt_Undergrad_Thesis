########################################################################################
# Modifying audio parameters in spectrograms to produce high resolution image
# Charlotte Mittelstaedt - University of British Columbia
# Created 3 November 2024
########################################################################################

# Set the backend and import packages
import os
os.environ['MPLBACKEND'] = 'TkAgg' 
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('TkAgg')  
from opensoundscape import Audio, Spectrogram
from pathlib import Path

# Set plot format guidelines
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['axes.labelsize'] = 14  # Adjust the size as needed
plt.rcParams['axes.titlesize'] = 16

# Load audio file
audio_object = Audio.from_file("E:/Pika_Field_Data_2024/MB_08Aug2024/MB_ARU_Data_15Aug2024/PIKARU25/Data/PIKARU25_20240812_055002.wav")
audio_object

# Trim audio to centre pika call in the spectrogram frame
trimmed = audio_object.trim(337.05,337.67)

# Create a spectrogram object
spectrogram_object = Spectrogram.from_audio(trimmed,
                                            overlap_fraction=0.9,
                                            )

# Bandpass frequencies
spectrogram_object = spectrogram_object.bandpass(700,15000)

spectrogram_object.plot()

# Show the plot
plt.show()