#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 23 10:29:55 2023

@author: mgruenig
"""


BATCH_SIZE = 8192
RUN_VERSION = 'v17_2' # name of the run in tensorboard
GPU = '1' ## cuda_visible_device -> which gpu to train on

CACHE = '/home/mgruenig/tmp/tf_cache/' # prefix of the cache file


import tensorflow as tf

from tensorflow.keras.models import  Model
from tensorflow.keras.layers import Embedding
from tensorflow.keras.callbacks import TensorBoard
from tensorflow.keras.callbacks import EarlyStopping

import pyarrow.feather
import numpy as np
import pandas as pd

import re
import glob
import os
import random
from itertools import compress
import sqlite3


#  GPU settings
os.environ['CUDA_VISIBLE_DEVICES'] = GPU # Use 3rd GPU
gpus = tf.config.experimental.list_physical_devices('GPU')
tf.config.experimental.set_memory_growth(gpus[0], True)


# load simulation databaase 
db = sqlite3.connect("/.../simulation_data/simulation_db_users_v6.sqlite")


# create lookup dict
states_lookup = pd.read_sql_query("SELECT * FROM states_lookup_pruned_5_v16", db)
states_lookup.isnull().values.any()
states_dict = pd.Series(states_lookup.stateID.values, index = states_lookup.state).to_dict()
states_dict.get('PIAB_2_28_30')
db.close()


# load training data
path = "/.../svd_training_data_v17/"
path_aug = "/.../svd_training_data_augmentation_v17/"

train_files_list_all = glob.glob(path + "*.parquet", recursive = True)
train_files_list_aug = glob.glob(path_aug + "*.parquet", recursive = True)


from random import shuffle, seed
# Set seed for reproducibility
seed(1)

shuffle(train_files_list_all)

# split into training and validation data
length_all = len(train_files_list_all)
length_train = round(0.8* length_all)
length_val = round(0.2 * length_all)

train_files_list = train_files_list_all[0:length_train]
val_files_list = train_files_list_all[length_train : length_train + length_val]

# get the augmentation data
val_split = [os.path.split(x) for x in val_files_list]
val_files_aug_list = [path_aug+x[1].replace('training', 'augmentation_data') for x in val_split]
files_exist = [os.path.exists(x) for x in val_files_aug_list] 
val_files_aug_list = list(compress(val_files_aug_list, files_exist))
val_files_list = val_files_list + val_files_aug_list 
shuffle(val_files_list)

train_split = [os.path.split(x) for x in train_files_list]
train_files_aug_list = [path_aug+x[1].replace('training', 'augmentation_data') for x in train_split]
files_exist = [os.path.exists(x) for x in train_files_aug_list] 
train_files_aug_list = list(compress(train_files_aug_list, files_exist))
train_files_list = train_files_list + train_files_list_aug
shuffle(train_files_list)

length_train = len(train_files_list)
length_val = len(val_files_list)

train_files = tf.data.Dataset.from_tensor_slices(train_files_list)
val_files = tf.data.Dataset.from_tensor_slices(val_files_list)

total_examples = len(train_files_list) * 500 #  estimate
total_loaded_examples = 0
total_valid_examples = 0


def load_file(fname):
   
   # load from disk:
   global total_loaded_examples, total_valid_examples
   
   sfname = fname.numpy().decode('utf-8')        
   df = pd.read_parquet(sfname, engine='pyarrow')
   df = df[df.notnull()]
   
   if df.isnull().values.any():
       print("HAS IS NAN: " + fname)
       df = df.dropna().reset_index(drop=True)
       
   total_loaded_examples = total_loaded_examples + df.shape[0] 
   total_valid_examples = total_valid_examples + df.shape[0]
   
   # state
   state = df['svd_state_1'].map(states_dict)
   if state.isnull().values.any():
       state = state.fillna(0)
       print("HAS IS NAN: " + fname)

   state = state.to_numpy(dtype = 'int16')
   state = tf.cast(state, tf.int16)
   state.set_shape([None])
   
   #restime
   restime = df['residence_time_1'] / 10
   restime = restime.to_numpy(dtype = 'float32')
   restime = tf.cast(restime, tf.float32)
   restime.set_shape([None])
   
   #site
   site = df[['WHC_1', "TextureSand_1", 'SoilDepth_1', 'AvailableNitrogen_1']] / [1000, 100, 10000, 100]
   site = site.to_numpy(dtype = 'float32')
   site = tf.cast(site, tf.float32)
   site.set_shape([None, 4])
    
   #climate
   scaling_array = np.concatenate([10*[20.], 10*[10.], 10*[20.], 10*[1.], 120*[30.], 120*[20.], 140*[1.], 100*[1.]]).flatten()
   climate = df.loc[:, 'MAT_1':'npp_10_10'] 
   climate = climate / scaling_array
   climate = climate.to_numpy(dtype = 'float32')
   climate = np.reshape(climate, (-1, 10, 52), order='F')
   climate = tf.cast(climate, tf.float32)
   climate.set_shape([None, 10, 52])
   
   # # state history
   state_hist1 = df['hist_state1_1'].map(states_dict) 
   state_hist2 = df['hist_state2_1'].map(states_dict) 
   state_hist3 = df['hist_state3_1'].map(states_dict)
   df_list = [state_hist1, state_hist2, state_hist3]
   states_hist = pd.concat(df_list, axis = 1)
   
   # Replace NaN with 0
   states_hist = states_hist.fillna(0)
   states_hist = states_hist.to_numpy(dtype = 'int16')
   states_hist = tf.cast(states_hist, tf.int16)
   states_hist.set_shape([None, 3])
        
   # time history
   time_hist = df.loc[:, 'hist_time1_1':'hist_time3_1'] / [10, 10, 10]
   time_hist = time_hist.to_numpy(dtype = 'float32')
   time_hist = tf.cast(time_hist, tf.float32)
   time_hist.set_shape([None, 3])
   
   # dependent variables
   target_state = df['target_state_1'].map(states_dict) 
   targetState = target_state.to_numpy(dtype = 'int16')
   targetState = tf.cast(targetState, tf.int16)
   targetState.set_shape([None])
   
   target_time = df['target_time_1']
   targetTime = target_time.to_numpy(dtype = 'int32')
   targetTime = tf.cast(targetTime-1, tf.int32)
   targetTime.set_shape([None])
    
   return (state, states_hist, restime, time_hist, site, climate, targetState, targetTime)
   
def load_file_wrap(x):
    state, states_hist, restime, time_hist, site, climate, targetState, targetTime = tf.py_function(load_file, [x], Tout=[tf.int16, tf.int16, tf.float32, tf.float32, tf.float32, tf.float32, tf.int16, tf.int32])
    state.set_shape([None])
    states_hist.set_shape([None, 3])
    restime.set_shape([None])
    site.set_shape([None, 4])
    time_hist.set_shape([None, 3])
    climate.set_shape([None, 10, 52])
    targetTime.set_shape([None])
    targetState.set_shape([None])
    return (state, states_hist, restime, time_hist, site, climate), (targetState, targetTime)


# define repeat datasets
train_ds = (
    train_files.map(load_file_wrap, num_parallel_calls=24)
    .unbatch()
    .batch(256)
    .cache(CACHE + "cache_t")
    .unbatch()
    .shuffle(4096*50)
    .batch(BATCH_SIZE)
    .repeat()
    )

val_ds = (
    val_files.map(load_file_wrap, num_parallel_calls=12)
    .unbatch()
    .cache(CACHE + "cache_e")
    .shuffle(4096*10)
    .batch(BATCH_SIZE)
    .repeat()
    )


# clear cache
for f in os.listdir(CACHE):
    os.remove(os.path.join(CACHE, f))



def skip_block(x, n=512, activ="gelu"):
    
    y = Dense(n, activation=activ)(x) # 
    y = Dropout(rate=0.4)(y)
    y = Dense(n, activation=activ, kernel_regularizer = tf.keras.regularizers.l2(0.001))(y) # 
    y = Dropout(rate=0.4)(y)
    z = layers.Add()([x,y])
    z = layers.BatchNormalization()(z)
    return z


from tensorflow.keras.layers import Input,  concatenate
from tensorflow.keras.layers import Dense, Dropout,  Flatten, Reshape
from tensorflow.keras.callbacks import TensorBoard, ReduceLROnPlateau
from tensorflow.keras import layers
from keras.callbacks import LambdaCallback

### define network
NClasses = states_lookup.shape[0] 
NTimeSteps = 10 

# Define the mask options and probabilities
Masks = tf.constant([[1, 1, 1], [1, 1, 0], [1, 0, 0], [0, 0, 0]], dtype=tf.float32)
mask_pred = tf.constant([[1, 1, 1]], dtype=tf.float32)
pMask = tf.constant([0.4, 0.2, 0.2, 0.2], dtype=tf.float32)


# Define the masking function
def apply_mask(x):
    
    def apply_mask_train():
        # Generate a separate mask for each element of the input tensor
        mask_indices = tf.random.categorical(tf.math.log([pMask]), tf.shape(x)[0])
        mask = tf.gather(Masks, mask_indices)
        mask = tf.squeeze(tf.gather(Masks, mask_indices), axis=1)

        # Apply the mask to each element of the input tensor
        x_masked = x * mask
        return x_masked

    def apply_mask_test():
        x_masked = x * mask_pred
        return x_masked
        
    return tf.cond(tf.keras.backend.learning_phase(), apply_mask_train, apply_mask_test)


# Inputs
sinput = Input(shape=(1,), dtype="int16",  name='state')
state_hist_input = Input(shape=(3,), dtype="int16",  name='hist_state') 
state_hist_input = tf.cast(state_hist_input, tf.float32)  # cast input to float32

# Define the Lambda layer to apply the binary mask
state_hist_input = tf.keras.layers.Lambda(apply_mask)(state_hist_input)
state_hist_input = tf.cast(state_hist_input, tf.int16) 

timeinput = Input(shape=(1,), dtype='float32', name='restime')
hist_timeinput = Input(shape=(3,), dtype='float32', name='hist_time')

# Define the Lambda layer to apply the binary mask
hist_timeinput = tf.keras.layers.Lambda(apply_mask)(hist_timeinput)
hist_timeinput = tf.keras.layers.Reshape((3,))(hist_timeinput)

siteinput = Input(shape=(4,), name='site')
climinput = Input(shape=(10,52,), name='climate')

# For the state we use an Embedding layer with 6 output dimmensions - changed to 32 because we have much more states
states_input = concatenate([sinput, state_hist_input])
sinput_em = Embedding(output_dim=64, input_dim=NClasses, input_length=4, name="stateem")(states_input)
sinput_em = Flatten()(sinput_em)
sinput_em = Dense(512, activation="relu")(sinput_em)
sinput_em = Reshape( (16,32,) )(sinput_em)
sinput_em = layers.Conv1D( filters=32, kernel_size=5, strides=3, activation='relu')(sinput_em)
sinput_em = layers.MaxPooling1D(pool_size = (2))(sinput_em)
sinput_em = layers.Flatten()(sinput_em)

#  The climate runs through a fully connected layer, and gets flattened
clim = Dense(256, activation="relu")(climinput)
clim = Dense(512, activation="relu")(clim)
clim = Dense(1024, activation="relu")(clim)
clim = Dense(128, activation="relu")(clim)
clim = Dropout(rate=0.5)(clim)
clim = Dense(64, activation="relu")(clim)
envx = layers.Flatten()(clim)

# The different inputs are joined together
minput = concatenate([sinput_em, timeinput, hist_timeinput, siteinput, envx])
comb = Dense(512, activation="relu")(minput)

for i in range(1):
    comb = skip_block(comb, 512)

c_time = Dense(512, activation="relu")(comb)
for i in range(1):
    c_time = skip_block(c_time, 512)

# target time branch
c_time = Dropout(rate=0.5)(c_time)

out_time = Dense(NTimeSteps, name="targetTime", activation="softmax")(c_time)

# target state branch
x = Dense(512, activation="relu")(comb)

for i in range(1):
    x = skip_block(x, 512, "relu")
 
x = Dropout(rate=0.5)(x)

# output layer
out = Dense(NClasses, activation="softmax", name="targetState")(x)

# define input list
input_list = [sinput, state_hist_input, timeinput, hist_timeinput, siteinput, climinput]

# create the model
model = Model(inputs=input_list, outputs=[out, out_time]) # 


import math

def scheduler(step, lr):
    decay_steps = 90
    initial_learning_rate = 0.001
    alpha = 0.05
    step = min(step, decay_steps)
    cosine_decay = 0.5 * (1 + math.cos(math.pi * step / decay_steps))
    decayed = (1 - alpha) * cosine_decay + alpha
    return initial_learning_rate * decayed


model.summary()


# compile the model
model.compile(loss={'targetState': 'sparse_categorical_crossentropy' , 'targetTime': 'sparse_categorical_crossentropy' }, #
              loss_weights={'targetState':1, 'targetTime': 1}, #
              optimizer=tf.keras.optimizers.Adam(lr=0.001),  
              metrics=['accuracy', 'sparse_categorical_accuracy', tf.keras.metrics.SparseTopKCategoricalAccuracy(
    k=2, name="sparse_top_k_categorical_accuracy", dtype=None)])

callbacks = [
    TensorBoard(log_dir='/home/mgruenig/logs/svd_dnn_test1/' + RUN_VERSION),
    tf.keras.callbacks.LearningRateScheduler(scheduler),
    tf.keras.callbacks.ModelCheckpoint('/home/mgruenig/logs/dnn_v17_2', monitor="val_loss", save_best_only=True)
]

# Scheme for learning rate: reduce LR by 50% if there no further progress for 3 epochs
tbReduceLr = ReduceLROnPlateau(monitor='val_loss', factor=0.5,
              patience=3, min_lr=0.00001)


# train the model
# ==============
steps_epoch = total_examples / BATCH_SIZE

model.fit(train_ds, 
          validation_data=val_ds, 
          epochs = 100,
          steps_per_epoch = steps_epoch, 
          validation_steps = round(steps_epoch/3),
          callbacks=[callbacks, tbReduceLr])

# examples actually used
print(total_valid_examples)


model.save("/.../saved_models/final_v17_2.h5")

### end 
