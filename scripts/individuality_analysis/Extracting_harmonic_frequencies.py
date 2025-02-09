


import librosa
import numpy as np
import matplotlib.pyplot as plt
import librosa.display

# Load the audio file
file_path = 'C:\\Users\\User\\Desktop\\Pika_Data_Analysis_copy_Oct2024\\Individuality\\MB\\MB-01.wav'
y, sr = librosa.load(file_path, sr=None)  # y is the audio signal, sr is the sampling rate

# Compute the Short-Time Fourier Transform (STFT)
D = librosa.stft(y)  # Complex-valued matrix
D_abs = np.abs(D)  # Magnitude of the STFT

# Estimate the fundamental frequency (f0) using librosa's YIN algorithm
f0, voiced_flag, voiced_probs = librosa.pyin(y, fmin=librosa.note_to_hz('C1'), fmax=librosa.note_to_hz('C8'))

# Optionally: Filter out unvoiced sections if necessary
voiced_f0 = f0[voiced_flag]  # Only keep the voiced parts

def find_harmonics(f0, D_abs, sr, harmonic_range=(0.5, 3)):
    harmonics = []
    harmonic_freqs = []
    for fundamental in f0:
        if fundamental is not None:
            # Search for harmonics from the 1st to the 5th (as an example)
            harmonic_freqs_for_this_f0 = []
            for i in range(1, 11):  # Look for the first 10 harmonics
                harmonic_freq = fundamental * i
                # Only consider harmonics within the desired range
                if harmonic_range[0] <= harmonic_freq <= harmonic_range[1] * fundamental:
                    # Find the closest frequency bin in the spectrogram
                    harmonic_bin = int(harmonic_freq * D_abs.shape[0] / sr)
                    # Make sure the bin index is within bounds
                    if 0 <= harmonic_bin < D_abs.shape[0]:
                        harmonic_freqs_for_this_f0.append(harmonic_freq)
            
            if harmonic_freqs_for_this_f0:
                harmonics.append(harmonic_freqs_for_this_f0)
                harmonic_freqs.append(harmonic_freqs_for_this_f0)
    
    return harmonics, harmonic_freqs

# Call the function to get harmonic frequencies
harmonics, harmonic_freqs = find_harmonics(voiced_f0, D_abs, sr)

# Print the harmonic frequencies for each fundamental frequency
for i, fundamental in enumerate(voiced_f0):
    print(f"Fundamental Frequency: {fundamental:.2f} Hz")
    print("Harmonic Frequencies (Hz):", harmonic_freqs[i])

##############################################

import librosa
import numpy as np
import matplotlib.pyplot as plt

# Load the audio file
file_path = 'C:\\Users\\User\\Desktop\\Pika_Data_Analysis_copy_Oct2024\\Individuality\\MB\\MB-01.wav'
y, sr = librosa.load(file_path, sr=None)  # y is the audio signal, sr is the sampling rate

# Compute the Short-Time Fourier Transform (STFT)
D = librosa.stft(y)  # Complex-valued matrix
D_abs = np.abs(D)  # Magnitude of the STFT

# Estimate the fundamental frequency (f0) using librosa's YIN algorithm
f0, voiced_flag, voiced_probs = librosa.pyin(y, fmin=librosa.note_to_hz('C1'), fmax=librosa.note_to_hz('C8'))

# Optionally: Filter out unvoiced sections if necessary
voiced_f0 = f0[voiced_flag]  # Only keep the voiced parts

def find_harmonics(f0, D_abs, sr, harmonic_range=(1, 5)):
    harmonics = []
    for fundamental in f0:
        if fundamental is not None:
            # Search for peaks at integer multiples of the fundamental frequency
            harmonic_freqs = [fundamental * (i+1) for i in range(5)]  # Check the first 5 harmonics
            
            # Find the closest frequency bins in the spectrogram
            harmonic_indices = []
            for h in harmonic_freqs:
                closest_bin = int(h * D_abs.shape[0] / sr)
                if 0 <= closest_bin < D_abs.shape[0]:
                    harmonic_indices.append(closest_bin)
            
            # Store the harmonics frequencies
            harmonics.append(harmonic_indices)
    return harmonics

# Call the function
harmonics = find_harmonics(voiced_f0, D_abs, sr)




