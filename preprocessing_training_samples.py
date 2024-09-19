#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 21 09:28:15 2022

@author: mgruenig
"""


import tensorflow as tf
import os

GPU = '3'
os.environ['CUDA_VISIBLE_DEVICES'] = GPU # Use 3rd GPU
gpus = tf.config.experimental.list_physical_devices('GPU')
tf.config.experimental.set_memory_growth(gpus[0], True)


# read meta data and examples from simulation db
import sqlite3
import pandas as pd
import numpy as np
from datar.all import c, f, is_element, filter
import pyarrow as pa
import pyarrow.parquet as pq
import gc
from timeit import default_timer as timer
import csv
import time

db = sqlite3.connect("/.../forest_simulation_db_v1.1.sqlite")
cursor = db.cursor()
cursor.execute("SELECT name FROM sqlite_master WHERE type = 'table'; ")
print(cursor.fetchall())


df_metadata = pd.read_sql_query("SELECT * FROM metadata_with_fitting_scenario", db)
df_examples = pd.read_sql_query("SELECT * FROM examples_pruned_5_v16", db)
# alternatively for augmentation data
#df_examples = pd.read_sql_query("SELECT * FROM examples_aug_pruned_5_v16", db)

df_examples['uniqueID'] = df_examples['uniqueID'].astype(int)
df_examples['simulationID'] = df_examples['simulationID'].astype(float)
df_examples['Year'] = df_examples['Year'].astype(int)
df_examples['residence_time'] = df_examples['residence_time'].astype(int)
df_examples['target_time'] = df_examples['target_time'].astype(int)
db.close()


filtered_df = df_examples[df_examples['scenario'].str.contains('ICHEC-EC-EARTH_historical')]

# load the climate compressor
from tensorflow.keras.models import Sequential, save_model, load_model
filepath = "/.../models/climcompressor_v4.h5"
model_tas = load_model(filepath, compile = True)


# isolate the scenarios
scenarios = df_examples["scenario"].unique()

# Filter out None values
scenarios = [s for s in scenarios if s is not None]

# Sort the filtered array alphabetically
scenarios = np.sort(scenarios)

# start loop over the scenarios
for s in scenarios:
    
    print(s)
    
    # isolate the examples of the focal scenario
    scen_examples = df_examples.loc[df_examples["scenario"] == s]
    
    # check which users occurr in this scenario
    users = scen_examples["uniqueID"].unique()
      
    # start looping over users
    for u in users:
        
      # check users and print for workflow surveilance 
      print("user: " + str(u))
      u = int(u)
      
      # select examples of the user and isolate the simulations associated to this user
      user_examples = scen_examples.loc[scen_examples["uniqueID"] == u]
      examples_sims = user_examples["simulationID"].unique()
     
      # else:
      db_training = sqlite3.connect(f'/.../harmonized_db_{s}_v1.1.sqlite')
           
      # define sql query with placeholder for examples IDs
      sql = '''SELECT * FROM '{}' WHERE simulationID IN (''' + ','.join('?' for i in range(len(examples_sims))) + ')'
      
      # define user table name 
      user_table = f'harmonized_simulations_{u}_{s}'     
      
      # make query and close DB. The chunksize argument makes a iterator out of it, serving 10000 rows at the time
      # Here a problem could be that simulations are split at 10000
      df_user_sims = pd.read_sql_query(sql.format(user_table), db_training, params = (examples_sims), chunksize = 10000)
      db.close()
       
      
      # then loop over the data iterator with chunks of 10000
      chnk = 0
      for df_user in df_user_sims:
          
          # keep count with chunks for naming the files
          chnk = chnk + 1
                          
          # drop the not needed columns and change type to integer if possible
          df_user = df_user.drop(["X", "svd_state", "Scenario", 'age', 'MeanDbh', 'Species1', 'Proportion1', 'Species2', 'Proportion2', 'Species3', 'Proportion3', 'Species4', 'Proportion4', 'Species5', 'Proportion5', 'MeanHeight', 'MaxHeight', 'MinHeight', 'LAI', 'Temp', 'Precip', 'BiomassLive', 'BasalArea', 'MeanDiameter', 'StemNumber', 'StandAge', 'Volume', 'meanDBH', 'sp_state2', 'lai_state', 'Hdom', 'height_state', 'Year_clim'], axis = 1, errors = 'ignore') 
          df_user['uniqueID'] = df_user['uniqueID'].astype(int)
          df_user['simulationID'] = df_user['simulationID'].astype(int)
          df_user['Year'] = df_user['Year'].astype(int)
                
          # for the simulations in the chunk, isolate the examples
          focal_sims = df_user["simulationID"].unique()
          user_examples_sub = user_examples.loc[user_examples['simulationID'].isin(focal_sims)]
          
          # create list for outputs
          list_user_all_examples = []
 
          # loop over the examples
          for i in range(0, int(len(user_examples_sub))):
                            
                # extract the information of the focal example
                focal_row = user_examples_sub.iloc[i,:]
                curID = int(focal_row.uniqueID)
                curSimID = int(focal_row.simulationID)
                year = focal_row.Year
                
                years = np.arange(year, year+10)
                    
                # get the climate info from the example simID and uniqueID to make sure to fit the right data
                sim_df = df_user.loc[(df_user["simulationID"] == curSimID)]
                sim_df = sim_df.loc[(sim_df["uniqueID"] == curID)]

                # List of columns to remove
                columns_to_remove = ["sp_comp", "lai_class", "dom_height", "veg_state", "WHC", "TextureSand", "TextureSilt", "TextureClay", "SoilDepth", "AvailableNitrogen"]

                # Remove the specified columns
                sim_df = sim_df.drop(columns=columns_to_remove)
                
                
                # isolate the needed years
                years_df = sim_df.loc[sim_df['Year'].isin(years)]
                years_df.insert(3, "svd_state", focal_row.svd_state)
                
                #  add exampleID and gridID
                years_df.insert(3, "exampleID", i)
                years_df.insert(4, "gridID", focal_row.grid_id)
                
                #  add target state etc
                years_df.insert(6, "residence_time", focal_row.residence_time)
                years_df.insert(7, "target_state", focal_row.target_state)
                years_df.insert(8, "target_time", focal_row.target_time)
                
                # add soil
                years_df.insert(9, "WHC", focal_row.WHC)
                years_df.insert(10, "TextureSand", focal_row.TextureSand)
                years_df.insert(11, "SoilDepth", focal_row.SoilDepth)
                years_df.insert(12, "AvailableNitrogen", focal_row.AvailableNitrogen)
                
                #  add history
                years_df.insert(13, "hist_state1", focal_row.hist_state1)
                years_df.insert(14, "hist_state2", focal_row.hist_state2)
                years_df.insert(15, "hist_state3", focal_row.hist_state3)
                years_df.insert(16, "hist_time1", focal_row.hist_time1)
                years_df.insert(17, "hist_time2", focal_row.hist_time2)
                years_df.insert(18, "hist_time3", focal_row.hist_time3)
                # print(years_df.columns.tolist()[0:20])
                
                if years_df.isnull().values.any():
                   print("HAS NAN: " + s + " user:" + str(u) + " chnk:" + str(chnk))
                   years_df = years_df.dropna().reset_index(drop=True)
                # add together
                list_user_all_examples.append(years_df)
                
                
          # bind together the list but exclude the examples that are not complete (i.e. less than 10 years)
          list_user_all_examples_filtered = [x for x in list_user_all_examples if len(x) == 10]
          if len(list_user_all_examples_filtered) == 0: 
              continue
          
          df_user_all_examples = pd.concat(list_user_all_examples_filtered)
          #print(df_user_all_examples.columns.tolist())
          del(list_user_all_examples_filtered)
          del(list_user_all_examples, df_user)
          
          # split up to climate dataframe and df with all other info
          df_user_all_examples_base = df_user_all_examples[list(df_user_all_examples.loc[:, 'uniqueID':"hist_time3"])]
          df_user_all_examples_clim = df_user_all_examples[list(df_user_all_examples.loc[:, "tas_1":"vpd_365"])]
          # add constant values for site conditions - taken from bins npp basic layers: nitrogen, depth, pctSand, pctSilt, pctClay
          df_user_all_examples_site = pd.DataFrame({'nitrogen' : len(df_user_all_examples_base) * [70], 'depth' : len(df_user_all_examples_base) * [90], 'pctSand' : len(df_user_all_examples_base) * [45], 'pctSilt' : len(df_user_all_examples_base) * [30], 'pctClay' : len(df_user_all_examples_base) * [25]})
          
          # the climate data frame is reformated and then fed to the climate compressor        
          df_user_all_examples_np = df_user_all_examples_clim.to_numpy()
          clim = np.reshape(df_user_all_examples_np, (-1, 4, 365))
          scale_clim = np.array( (10, 3, 20, 1) )
          scale_clim = scale_clim[ np.newaxis, :, np.newaxis]
          clim = clim / scale_clim
          clim = np.moveaxis(clim, 1, 2)
          clim.astype(np.float32)
          clim = tf.convert_to_tensor(clim, dtype = tf.float32)
          
          site = df_user_all_examples_site.to_numpy()
          site = site / [100, 100, 100, 100, 100]
          site.astype(np.float32)
          env = tf.convert_to_tensor(site, dtype = tf.float32)
          dat = clim, env
          
          # climate compressor
          tas_layer_output = model_tas.predict(dat, verbose = 0)
          tas_df = pd.DataFrame(tas_layer_output[1])
          tas_df = tas_df.set_axis([f'comp_clim_{i+1}' for i in range(14)], axis = 1, copy = False)

          npp_df = pd.DataFrame(tas_layer_output[0])
          npp_df = npp_df.set_axis([f'npp_{i+1}' for i in range(10)], axis = 1, copy = False)
          
                  
          df_clim_averages = pd.DataFrame()
          df_clim_averages["MAT"] = df_user_all_examples_clim.loc[:, 'tas_1':'tas_365'].mean(axis = 1)
          df_clim_averages["ANP"] = df_user_all_examples_clim.loc[:, 'prec_1':'prec_365'].mean(axis = 1)
          df_clim_averages["Mrad"] = df_user_all_examples_clim.loc[:, 'rad_1':'rad_365'].mean(axis = 1)
          df_clim_averages["Mvpd"] = df_user_all_examples_clim.loc[:, 'vpd_1':'vpd_365'].mean(axis = 1)
            
    
          days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
          day_in_year = 0

          for n, days in enumerate(days_in_month):
              df_clim_averages["tas_month" + str(n+1)] = \
              df_user_all_examples_clim.iloc[:, day_in_year:day_in_year + days].mean(axis = 1)
                
              day_in_year += days
                
          day_in_year = 0
            
          for n, days in enumerate(days_in_month):
              df_clim_averages["prec_month" + str(n+1)] = \
              df_user_all_examples_clim.iloc[:, 365 + day_in_year: 365 + day_in_year + days].mean(axis = 1)
              day_in_year += days
                
          df_clim_averages = df_clim_averages.reset_index(drop=True)
          df_user_all_examples_base = df_user_all_examples_base.reset_index(drop=True)   
            
          # put everything together               
          df_user_all_examples_filled = pd.concat([df_user_all_examples_base.reset_index(drop=True), df_clim_averages.reset_index(drop=True), \
                                                   tas_df.reset_index(drop=True), npp_df.reset_index(drop=True)], axis = 1)
 
        # then reshape the data so that the years of the example are after each other
          df_user_all_examples_filled_np = df_user_all_examples_filled.to_numpy()
          # examples_fin = np.reshape(df_user_all_examples_filled_np, (-1, 10, 71))
    
          df_user_all_examples_filled["s"] = df_user_all_examples_filled.groupby(["exampleID", "uniqueID", "simulationID"], dropna = False).cumcount() + 1
            
          df_new = df_user_all_examples_filled.set_index(["s", "exampleID", "uniqueID", "simulationID"]).unstack(0)
          df_new.columns=[f"{x}_{y}" for x,y in df_new.columns]
          df_new = df_new.reset_index()

          # remove double entries
          df_new_reduced = df_new[['exampleID', 'uniqueID', 'simulationID', 'gridID_1', "Year_1",  'svd_state_1', 'target_state_1', 'residence_time_1', 'target_time_1', 'WHC_1', "TextureSand_1", 'SoilDepth_1', 'AvailableNitrogen_1', 'hist_state1_1', "hist_state2_1", 'hist_state3_1', 'hist_time1_1', 'hist_time2_1', 'hist_time3_1']]
          df_new_mat = df_new.filter(like = "MAT")
          df_new_anp = df_new.filter(like = "ANP")
          df_new_rad = df_new.filter(like = "Mrad")
          df_new_vpd = df_new.filter(like = "Mvpd")
          df_new_month = df_new.filter(like = "month")
          df_new_clim = df_new.filter(like = "comp_clim")
          df_new_npp = df_new.filter(like = "npp")
          
          df_list = [df_new_reduced, df_new_mat, df_new_anp, df_new_rad, df_new_vpd, df_new_month, df_new_clim, df_new_npp]
          example_final = pd.concat(df_list, axis = 1)
          
          # drop Nas
          if example_final.isnull().values.any():
                   print("HAS NAN: " + s + " user:" + str(u) + " chnk:" + str(chnk))
                   example_final = example_final.dropna().reset_index(drop=True)
          
          #print(example_final.columns.tolist())
          number_of_examples = number_of_examples + example_final.shape[0]
          # print(number_of_examples)

          example_arrow = pa.Table.from_pandas(example_final, preserve_index = False)
          pq.write_to_dataset(example_arrow, f"/.../svd_training_data_v19/training_{s}_{u}_chunk{chnk}.parquet", existing_data_behavior = 'overwrite_or_ignore')
          # alternatively for augmentation dataset:
          # pq.write_to_dataset(example_arrow, f"/data/public/Projects/Resonate/svd_dnn/svd_training_data_augmentation_v19/augmentation_data_{s}_{u}_chunk{chnk}.parquet", existing_data_behavior = 'overwrite_or_ignore')






