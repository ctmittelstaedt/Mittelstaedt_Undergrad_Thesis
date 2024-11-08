
import wandb
try:
   wandb.login()
   wandb_session = wandb.init(
       entity='ctmittelstaedt-university-of-british-columbia', 
       project='Pika Recognizer',
       name='Trial 2',
    )
except: #if wandb.init fails, don't use wandb logging
    print('failed to create wandb session. wandb session will be None')
    wandb_session = None

# the cnn module provides classes for training/predicting with various types of CNNs
from opensoundscape import CNN

#other utilities and packages
import torch
import pandas as pd
from pathlib import Path
import numpy as np
import pandas as pd
import random
import subprocess
from glob import glob
import sklearn
import os

# *For preprocessor
from opensoundscape.preprocess.preprocessors import SpectrogramPreprocessor
from opensoundscape.ml.datasets import AudioFileDataset, AudioSplittingDataset
from opensoundscape import preprocess
import IPython.display as ipd

#set up plotting
from matplotlib import pyplot as plt
from matplotlib import rcParams
plt.rcParams['figure.figsize']=[15,5] #for large visuals

rcParams['figure.figsize'] = (10, 6)  # Example of setting figure size
rcParams['figure.dpi'] = 100           # Example of setting dpi

# take these out when running actual model
torch.manual_seed(0)
random.seed(0)
np.random.seed(0)

# *Modify the default preprocessor**still need to figure out dB modifications
preprocessor = SpectrogramPreprocessor(sample_duration=2)
preprocessor.pipeline

preprocessor.pipeline.bandpass.set(min_f=700,max_f=15000)
preprocessor.pipeline.to_spec.params['overlap_fraction'] = 0.9

preprocessor.pipeline.to_spec.params

print(preprocessor.pipeline.to_spec.params)

# Set the current directory to where the dataset is downloaded
dataset_path = Path("./All_annotations_copy/")

# Make a list of all of the selection table files
raven_files = glob(f"{dataset_path}/raven_data/*/*.txt")

# Create a list of audio files, one corresponding to each Raven file 
# (Audio files have the same names as selection files with a different extension)
audio_files = glob(f"{dataset_path}/audio_data/*/*.wav")


from opensoundscape.annotations import BoxedAnnotations
# Create a dataframe of annotations
annotations = BoxedAnnotations.from_raven_files(
    raven_files, 
    "Annotation",
    audio_files, 
    keep_extra_columns=['Selection','View', 'Channel','Begin Time (s)', 'End Time (s)', 'Low Freq (Hz)', 'High Freq (Hz)','Annotation'])


# Access the underlying DataFrame
#annotations_data = annotations.df
#annotations_data.to_csv("./Recognizers/All_annotations_copy/annotations_data.csv")

# Parameters to use for label creation
clip_duration = 0.3
clip_overlap = 0.16
min_label_overlap = 0.01
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
mask = clip_labels.reset_index()['file'].apply(lambda x: 'testing_data_raven' in x).values
test_set = clip_labels[mask]

# All other files will be used as a training set
train_and_val_set = clip_labels.drop(test_set.index)

# Save .csv tables of the training and validation sets to keep a record of them
train_and_val_set.to_csv("./All_annotations_copy/train_and_val_set.csv")
test_set.to_csv("./All_annotations_copy/test_set.csv")

train_and_val_set = pd.read_csv('./All_annotations_copy/train_and_val_set.csv',index_col=[0,1,2])
test_set = pd.read_csv('./All_annotations_copy/test_set.csv',index_col=[0,1,2])

# Split our training data into training and validation sets
train_df, valid_df = sklearn.model_selection.train_test_split(train_and_val_set, test_size=0.1, random_state=0)

train_df.to_csv("./All_annotations_copy/train_set.csv")
valid_df.to_csv("./All_annotations_copy/valid_set.csv")

from opensoundscape.data_selection import resample


# upsample (repeat samples) so that all classes have 80 samples
balanced_train_df = resample(train_df,n_samples_per_class=80,random_state=0)

# Create a CNN object designed to recognize 3-second samples
from opensoundscape import CNN

# Use resnet34 architecture
architecture = 'resnet34'

# Can use this code to get your classes, if needed
class_list = list(train_df.columns)

model = CNN(
    architecture = architecture,
    classes = class_list,
    sample_duration = clip_duration #3s, selected above
)

print(f'model.device is: {model.device}')


checkpoint_folder = Path("model_training_checkpoints")
checkpoint_folder.mkdir(exist_ok=True)

#*changed epochs from 2 to 10
if __name__ == '__main__':
    
    model.train(
    balanced_train_df,
    valid_df,
    epochs = 10,
    batch_size = 20,
    log_interval = 100, #log progress every 100 batches
    num_workers = 6, #4 parallelized cpu tasks for preprocessing
    wandb_session = None, #wandb_session,
    save_interval = 10, #save checkpoint every 10 epochs
    save_path = checkpoint_folder #location to save checkpoints
)

