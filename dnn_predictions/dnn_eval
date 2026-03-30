#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 20 14:03:58 2024

@author: mgruenig
"""


GPU = '0' ## cuda_visible_device -> which gpu to train on
CACHE = '/home/mgruenig/tmp/tf_cache/' # prefix of the cache file



### for LORIEN change path from Public to public
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
# missing_state = pd.DataFrame({'state':'missing', 'stateID':0}, index =[0])
# states_lookup = pd.concat([missing_state, states_lookup[:]]).reset_index(drop = True)

states_dict = pd.Series(states_lookup.stateID.values, index = states_lookup.state).to_dict()
states_dict.get('PIAB_2_28_30')
db.close()

inverse_dict = { y: x for (x, y) in states_dict.items()}
inverse_dict.get(1866)



model = tf.keras.models.load_model("/.../saved_models/final_v17_2.h5", compile=True)



# load training data
path = "/.../svd_dnn/svd_training_data_v17/"
path_aug = "/.../svd_training_data_augmentation_v17/"

# train_files_list1 = glob.glob(path + "*ICHEC*" + "*.parquet", recursive = True)
# train_files_list2 = glob.glob(path + "*NCC*" + "*.parquet", recursive = True)
# train_files_list = train_files_list1 + train_files_list2
# val_files_list = glob.glob(path + "*MPI*" + "*.parquet", recursive = True)

# train_files_list = train_files_list_all[0:len(train_files_list_all)-10]
# val_files_list = train_files_list_all[len(train_files_list_all)-10:len(train_files_list_all)]

train_files_list_all = glob.glob(path + "*.parquet", recursive = True)
train_files_list_aug = glob.glob(path_aug + "*.parquet", recursive = True)
# matching_strings = ["_1016_"]
# train_files_list_all = [file for file in train_files_list_all if not any(matching_string in file for matching_string in matching_strings)]
# train_files_list_aug = [file for file in train_files_list_aug if not any(matching_string in file for matching_string in matching_strings)]

matching_strings = ["_1031_", "_1037_", "_1038_", "_1039_", "_1040_", "_1041_", "_1042_", "_1043_", "_1044_", "_1045_", "_1046_", "_1047_"]
train_files_list_all = [file for file in train_files_list_all if not any(matching_string in file for matching_string in matching_strings)]
train_files_list_aug = [file for file in train_files_list_aug if not any(matching_string in file for matching_string in matching_strings)]

# train_files_list_1025 = glob.glob(path + "*_1025_*" + "*.parquet", recursive = True)
#train_files_list_1031 = glob.glob(path + "*_1031_*" + "*.parquet", recursive = True)
#train_files_list_all = train_files_list_1012

from random import shuffle, seed
# Set seed for reproducibility
seed(1)

shuffle(train_files_list_all)

length_all = len(train_files_list_all)
length_train = round(0.7* length_all)
length_val = round(0.1 * length_all)
length_test = round(0.2 * length_all)


train_files_list = train_files_list_all[0:length_train]
val_files_list = train_files_list_all[length_train : length_train + length_val]
test_files_list = train_files_list_all[length_train + length_val : length_all]


# get the augmentation data
val_split = [os.path.split(x) for x in val_files_list]
# val_split[0][1].replace('training', 'augmentation_data')
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


# add augented *only* to training
length_train = len(train_files_list)
length_val = len(val_files_list)

train_files = tf.data.Dataset.from_tensor_slices(train_files_list)
val_files = tf.data.Dataset.from_tensor_slices(val_files_list)



# get the augmentation data
test_split = [os.path.split(x) for x in test_files_list]
# test_split[0][1].replace('training', 'augmentation_data')
test_files_aug_list = [path_aug+x[1].replace('training', 'augmentation_data') for x in test_split]
files_exist = [os.path.exists(x) for x in test_files_aug_list] 
test_files_aug_list = list(compress(test_files_aug_list, files_exist))
test_files_list = test_files_list + test_files_aug_list 
shuffle(test_files_list)

test_files = tf.data.Dataset.from_tensor_slices(test_files_list)




def load_file(fname):
   
   sfname = fname.numpy().decode('utf-8')        
   df = pd.read_parquet(sfname, engine='pyarrow')
   df = df[df.notnull()]
  
   if df.isnull().values.any():
       print("HAS IS NAN: " + fname)
       df = df.dropna().reset_index(drop=True)
       

   # state
   state = df['svd_state_1'].map(states_dict)
   state = state.to_numpy(dtype = 'int16')
   # np.random.shuffle(state)  # Shuffle in-place
   state = tf.cast(state, tf.int16)
   state.set_shape([None])
   
   #resttime
   restime = df['residence_time_1'] / 10
   restime = restime.to_numpy(dtype = 'float32')
   # np.random.shuffle(restime)  # Shuffle in-place
   restime = tf.cast(restime, tf.float32)
   restime.set_shape([None])
   
   #site
   site = df[['WHC_1', "TextureSand_1", 'SoilDepth_1', 'AvailableNitrogen_1']] / [1000, 100, 10000, 100]
   site = site.to_numpy(dtype = 'float32')
   # np.random.shuffle(site)  # Shuffle in-place
   site = tf.cast(site, tf.float32)
   site.set_shape([None, 4])
    
   #climate
   scaling_array = np.concatenate([10*[20.], 10*[10.], 10*[20.], 10*[1.], 120*[30.], 120*[20.], 140*[1.], 100*[1.]]).flatten()
   climate = df.loc[:, 'MAT_1':'npp_10_10'] 
   climate = climate / scaling_array
   climate = climate.to_numpy(dtype = 'float32')
   climate = np.reshape(climate, (-1, 10, 52), order='F')
   # np.random.shuffle(climate)  # Shuffle in-place
   climate = tf.cast(climate, tf.float32)
   climate.set_shape([None, 10, 52])
   
   # # state history
   state_hist1 = df['hist_state1_1'].map(states_dict) 
   state_hist2 = df['hist_state2_1'].map(states_dict) 
   state_hist3 = df['hist_state3_1'].map(states_dict)
   df_list = [state_hist1, state_hist2, state_hist3]
   states_hist = pd.concat(df_list, axis = 1)


   states_hist = states_hist.fillna(0)
   states_hist = states_hist.to_numpy(dtype = 'int16')
   states_hist = tf.cast(states_hist, tf.int16)
   states_hist.set_shape([None, 3])
   
     
   # # time history
   time_hist = df.loc[:, 'hist_time1_1':'hist_time3_1'] / [10, 10, 10]
   time_hist = time_hist.to_numpy(dtype = 'float32')
   # np.random.shuffle(time_hist)  # Shuffle in-place
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





test_ds = (
    test_files
    .shuffle(256)
    .map(load_file_wrap, num_parallel_calls=24)
    .unbatch()
    .batch(100000)
    )


next_elem = next(iter(test_ds))

X = next_elem[0]
Y = next_elem[1][0]

# shuffle svd state first
results_permutation_states = []
    
    
for i in range(10):
    
    shuffled_tensor = tf.random.shuffle(X[0])
    X_shuff = (shuffled_tensor, X[1], X[2], X[3], X[4], X[5])

    predictions = model.predict(X_shuff, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_permutation_states.append(mean_value)

# Print all results
print("Results array:", results_permutation_states)



# then the state history
results_permutation_hist_states = []
    
    
for i in range(10):
    
    shuffled_tensor = tf.random.shuffle(X[1])
    X_shuff = (X[0], shuffled_tensor, X[2], X[3], X[4], X[5])

    predictions = model.predict(X_shuff, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_permutation_hist_states.append(mean_value)

# Print all results
print("Results array:", results_permutation_hist_states)



# then the state history
results_permutation_restime = []
    
    
for i in range(10):
    
    shuffled_tensor = tf.random.shuffle(X[2])
    X_shuff = (X[0], X[1], shuffled_tensor, X[3], X[4], X[5])

    predictions = model.predict(X_shuff, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_permutation_restime.append(mean_value)

# Print all results
print("Results array:", results_permutation_restime)


# then the state history
results_permutation_hist_restime = []
    
    
for i in range(10):
    
    shuffled_tensor = tf.random.shuffle(X[3])
    X_shuff = (X[0], X[1], X[2], shuffled_tensor, X[4], X[5])

    predictions = model.predict(X_shuff, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_permutation_hist_restime.append(mean_value)

# Print all results
print("Results array:", results_permutation_hist_restime)



# then the state history
results_permutation_soil = []
    
    
for i in range(10):
    
    shuffled_tensor = tf.random.shuffle(X[4])
    X_shuff = (X[0], X[1], X[2], X[3], shuffled_tensor, X[5])

    predictions = model.predict(X_shuff, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_permutation_soil.append(mean_value)

# Print all results
print("Results array:", results_permutation_soil)


#  soil, do all columns individually
def shuffle_column_in_tensor(X, column_to_shuffle, model, Y):
    """
    Shuffle a specific column in tensor X, predict with the model, and calculate the accuracy.
    
    Args:
    - X (tuple of tensors): The input data tuple, where each element is a tensor.
    - column_to_shuffle (int): The index of the column to shuffle.
    - model: The trained model to make predictions.
    - Y: True labels for the data.
    
    Returns:
    - mean_value (float): The mean accuracy of the predictions after shuffling the column.
    """
    tensor_to_shuffle = X[4]
    
    # Step 1: Extract the column to shuffle
    column = tensor_to_shuffle[:, column_to_shuffle]
    
    # Step 2: Shuffle the column
    shuffled_column = tf.random.shuffle(column)
    
    # Step 3: Split the tensor and reassemble with the shuffled column
    left = tensor_to_shuffle[:, :column_to_shuffle]  # Columns before the shuffled one
    right = tensor_to_shuffle[:, column_to_shuffle + 1:]  # Columns after the shuffled one
    
    # Concatenate the left, shuffled column, and right back together
    shuffled_tensor = tf.concat([left, tf.expand_dims(shuffled_column, axis=-1), right], axis=1)
    
    # Prepare the new input with the shuffled tensor
    X_shuff = (X[0], X[1], X[2], X[3], shuffled_tensor, X[5])
    
    # Predict with the model
    predictions = model.predict(X_shuff, verbose=1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis=1)
    
    # Calculate mean accuracy
    mean_value = np.mean(pL == Y)
    
    return mean_value


def apply_column_shuffling(X, model, Y, repetitions=10):
    """
    Apply the column shuffling to multiple columns and store the results for each column in separate lists.
    
    Args:
    - X (tuple of tensors): The input data tuple.
    - model: The trained model to make predictions.
    - Y: True labels for the data.
    - repetitions (int): The number of times to shuffle each column.
    
    Returns:
    - results (dict): Dictionary containing the results of the column shuffling for each column.
    """
    results = {}
    
    # Loop through columns 0 to 3 (4 columns in total)
    for column_to_shuffle in range(4):
        results_column = []
        
        # Perform the shuffle for the specified number of repetitions
        for _ in range(repetitions):
            mean_value = shuffle_column_in_tensor(X, column_to_shuffle, model, Y)
            results_column.append(mean_value)
        
        # Store the results for this column
        results[f"results_permutation_soil{column_to_shuffle + 1}"] = results_column
    
    return results

# Apply column shuffling and repetition for 10 times
results = apply_column_shuffling(X, model, Y, repetitions=10)

# Convert the results dictionary into a DataFrame
results_df = pd.DataFrame(results)

# Print the DataFrame
print(results_df)



# then the state history
results_permutation_clim = []
    
    
for i in range(10):
    
    shuffled_tensor = tf.random.shuffle(X[5])
    X_shuff = (X[0], X[1], X[2], X[3], X[4], shuffled_tensor)

    predictions = model.predict(X_shuff, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_permutation_clim.append(mean_value)

# Print all results
print("Results array:", results_permutation_clim)



# shuffle svd state first
results_overall = []
    
    
for i in range(10):
    
    predictions = model.predict(X, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    results_overall.append(mean_value)

# Print all results
print("Results array:", results_overall)



# Concatenate arrays along columns (axis 1) to create a 2D array
combined_array = np.column_stack((results_overall, results_permutation_states, results_permutation_hist_states,
                                      results_permutation_restime, results_permutation_hist_restime, results_permutation_clim,
                                          results_permutation_soil))

# Convert the combined array to a Pandas DataFrame
df = pd.DataFrame(combined_array, columns=['Overall', 'State', 'State_hist', "Restime", 'Restime_hist', 'Clim', 'Soil'])

df.to_csv("/.../variable_permutation_mod2.csv", index=False)





#  test the classes of state frequency
state_count = pd.read_csv("/.../training_samples_statecount.csv", delimiter=',')
state_count['state_id'] = state_count['svd_state'].map(states_dict)


# then the state history
overall_acc = []

for i in range(20):
    
    shuffle(test_files_list)
    test_files = tf.data.Dataset.from_tensor_slices(test_files_list)


    test_ds = (
        test_files
        .shuffle(256)
        .map(load_file_wrap, num_parallel_calls=24)
        .unbatch()
        .batch(100000)
        )

    next_elem = next(iter(test_ds))
    
    X = next_elem[0]
    Y = next_elem[1][0]
    
    predictions = model.predict(X, verbose = 1)
    predictions = predictions[0]
    pL = np.argmax(predictions, axis = 1)
        
    mean_value = np.mean(pL == Y)
    overall_acc.append(mean_value)

# Print all results
print("Results array:", overall_acc)





def evaluate_model_for_state_class(
    model, test_files_list, state_count, class_value, iterations=20, batch_size=100000
):
    """
    Evaluates the model for a specific state class over multiple iterations.

    Parameters:
    - model: The TensorFlow model to predict with.
    - test_files_list: List of test file paths.
    - state_count: DataFrame with 'state_id' and 'count_class'.
    - class_value: The class value to filter by.
    - iterations: Number of times to shuffle and evaluate.
    - batch_size: Batch size for the dataset.

    Returns:
    - List of mean values from predictions over all iterations.
    """
    results = []

    # Extract state IDs for the specified class_value
    state_ids_to_filter = state_count.loc[
        state_count['count_class'] == class_value, 'state_id'
    ].dropna().astype(int).tolist()

    for _ in range(iterations):
        shuffle(test_files_list)
        test_files = tf.data.Dataset.from_tensor_slices(test_files_list)

        test_ds = (
            test_files
            .shuffle(256)
            .map(load_file_wrap, num_parallel_calls=24)
            .unbatch()
            .batch(batch_size)
        )

        next_elem = next(iter(test_ds))

        # Create a boolean mask to filter state IDs
        first_tensor = next_elem[0][0]  # Assuming the state_id is in the first tensor
        state_ids_tensor = tf.constant(state_ids_to_filter, dtype=first_tensor.dtype)
        mask = tf.reduce_any(tf.equal(tf.expand_dims(first_tensor, axis=-1), state_ids_tensor), axis=-1)

        # Apply the mask to filter tensors
        filtered_elem_0 = tuple(tf.boolean_mask(tensor, mask) for tensor in next_elem[0])
        filtered_elem_1 = tuple(tf.boolean_mask(tensor, mask) for tensor in next_elem[1])

        # Prepare input and output
        X = filtered_elem_0
        Y = filtered_elem_1[0]

        # Make predictions
        predictions = model.predict(X, verbose=1)
        predictions = predictions[0]
        pL = np.argmax(predictions, axis=1)

        # Calculate mean accuracy
        mean_value = np.mean(pL == Y)
        results.append(mean_value)

    return results



# Placeholder for results
results_states = {}

# Assuming `model`, `test_files_list`, and `state_count` are defined
for class_value in range(1, 6):  # Loop through 5 classes (1 to 5)
    results_states[f"results_statesclass{class_value}"] = evaluate_model_for_state_class(
        model, test_files_list, state_count, class_value=class_value, iterations=20
    )

# Convert results dictionary to a DataFrame
results_df = pd.DataFrame(results_states)

# Add iteration numbers as a column
results_df.insert(0, 'Iteration', range(1, len(results_df) + 1))

# add overall acc
results_df['Overall'] = overall_acc

# Display the DataFrame
print(results_df)

results_df.to_csv("/.../crossval_state_freq2_mod2.csv", index=False)




# test which states are predicted
state_ids_to_filter = state_count.loc[
        state_count['count_class'] == 1, 'state_id'
    ].dropna().astype(int).tolist()

shuffle(test_files_list)
test_files = tf.data.Dataset.from_tensor_slices(test_files_list)

test_ds = (
    test_files
    .shuffle(256)
    .map(load_file_wrap, num_parallel_calls=24)
    .unbatch()
    .batch(100000)
)

next_elem = next(iter(test_ds))

# Create a boolean mask to filter state IDs
first_tensor = next_elem[0][0]  # Assuming the state_id is in the first tensor
state_ids_tensor = tf.constant(state_ids_to_filter, dtype=first_tensor.dtype)
mask = tf.reduce_any(tf.equal(tf.expand_dims(first_tensor, axis=-1), state_ids_tensor), axis=-1)

# Apply the mask to filter tensors
filtered_elem_0 = tuple(tf.boolean_mask(tensor, mask) for tensor in next_elem[0])
filtered_elem_1 = tuple(tf.boolean_mask(tensor, mask) for tensor in next_elem[1])

# Prepare input and output
X = filtered_elem_0
Y = filtered_elem_1[0]

# Make predictions
predictions = model.predict(X, verbose=1)
predictions = predictions[0]
pL = np.argmax(predictions, axis=1)

# Find mismatched elements
mismatched_indices = np.where(pL != Y)[0]  # Indices where pL and Y differ
mismatched_predictions = pL[mismatched_indices]  # Predicted labels for mismatches

results = [] 
# Append mismatched indices to results
results.append({
    "mismatched_predictions": mismatched_predictions,
})

mapped_predictions = [inverse_dict.get(pred, "Unknown") for pred in mismatched_predictions]

# Filter the state_count DataFrame using mismatched_predictions
filtered_df = state_count[state_count['state_id'].isin(mismatched_predictions)]

# Get the unique count_class values
mismatched_classes = filtered_df['count_class'].value_counts()

