# -*- coding: utf-8 -*-
"""
Created on Tue Mar  7 14:34:59 2023

@author: gu47yiy
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb  6 13:51:12 2023

@author: mgruenig
"""

# import libraries
import tensorflow as tf

from tensorflow.keras.models import  Model
from tensorflow.keras.layers import Embedding
from tensorflow.keras.callbacks import TensorBoard
from tensorflow.keras.callbacks import EarlyStopping

import pyarrow.feather as ft
import numpy as np
import pandas as pd

import re
import glob
import os
import random
import sqlite3
import pathlib
import platform

import pdb
import sys

# Get the directory containing the init_states.py
module_path = os.path.abspath(os.path.join(os.path.dirname('/home/mgruenig/python_scripts/')))
sys.path.append(module_path)

from init_states_v17 import generate_init_states


# define functions

class SVD(object):
    '''class for conducting a SVD prediction. Designed to be called by DNNrun.py.
    '''
    def __init__(self, spec_scen_year_init, simulation_sql,\
             dnn_model, load_data, save_pred, \
                 input_files = None, file_props = None):
        self.spec_scen_year_init = spec_scen_year_init
        self.simulation_sql = simulation_sql
        self.dnn_model = dnn_model
        self.load_data = load_data
        self.save_pred = save_pred
        self.input_files = input_files
        self.file_props = file_props        
        
        ### set in self.initSQL()
        self.states_dict = dict() 
        self.states_lookup = []
        self.inverse_dict = dict()
        ###
        self.initSQL()
        
        ### set in self.setDNNmodel()
        self.model = None
        ###
        self.setDNNmodel()
        
        
    def setDNNmodel(self):
         
        #  load model
        self.model = tf.keras.models.load_model(self.dnn_model, compile = True)
        #  self.model.summary()
        
        
    def initSQL(self):

        # connect to sqlite db 
        db = sqlite3.connect(self.simulation_sql.as_posix())
        
        # create lookup dict
        # many rows (6452 in one call), two columns
        states_lookup = pd.read_sql_query("SELECT * FROM states_lookup_pruned_5_v17", db) 
        
        # check whether there are nan's in states_lookup
        states_lookup.isnull().values.any()
        states_lookup['stateID'] = states_lookup['stateID'].astype(int)
           
        self.states_dict = pd.Series(states_lookup.stateID.values,\
                                index = states_lookup.state).to_dict()
        # states_dict.get('PIAB_2_28_32')
        self.inverse_dict = { y: x for (x, y) in self.states_dict.items()}
        self.states_lookup = states_lookup

    
    def predict(self):
        
        # random.seed(1)

        years = self.spec_scen_year_init['years']
        species = self.spec_scen_year_init['species']
        scenarios = self.spec_scen_year_init['scenarios']
        # init_states = self.spec_scen_year_init['init_states']
        # Load and process the data
        lookup_table_single, lookup_table_mixed = generate_init_states()

        
        # define history - in this case missing
        history_state = "missing"
        initial_restime = 1
        
        for file in self.input_files:
        
            # progress
            print(f"Current year: {file['year']}\n")
                
            # species loop
            # for sp in species:
            sp = file['species']    
            print(f"Current species: {sp}\n")
                
            # define initial state with species code
            sp_initial = sp.split("_")
            sp_initial = sp_initial[0][0:2] + sp_initial[1][0:2]
            sp_initial = sp_initial.upper()

            # get initial states
            init_states = lookup_table_single.get(sp_initial, [])
            # OR use the mixed species lookup for the dominance predictions
            # init_states = lookup_table_mixed.get(sp_initial, [])       
            
              
            # loop over initial states
            for init_state in init_states:
                print(f"init_state: {init_state}\n")
                                            
                # pdb.set_trace()
                # initial_state = sp_initial + init_state
                initial_state = init_state
                history_state = "missing"
                
                # load the file of the year 
                sfname = file['path']
                
                ### format data for DNN input ---
                df_orig = pd.read_parquet(sfname, engine='pyarrow')
                df_orig = df_orig[df_orig.notnull()]
                
                # translate state with lookup table
                init_state_df = pd.DataFrame({'state':len(df_orig) * \
                               [int(self.states_dict.get(initial_state))]})
                state_df1 = pd.concat([df_orig, init_state_df], axis=1)
                state_df = state_df1['state']
                
                # residence time we give a random value
                restime_array = np.array(len(state_df) * [initial_restime]) / 10
                  
                # state history
                state_hist1 = pd.DataFrame(len(state_df) * [int(self.states_dict.get(history_state))])
                df_list = [state_hist1] * 3
                states_hist_df = pd.concat(df_list, axis = 1)
                states_hist_df.columns = ['state_hist1', 'state_hist2', 'state_hist3']
                # states_hist_df = pd.concat(df_list, axis = 1)
                        
                time_hist1 = pd.DataFrame(len(state_df) * [random.randint(1, 10)])
                time_hist2 = pd.DataFrame(len(state_df) * [random.randint(1, 10)])
                time_hist3 = pd.DataFrame(len(state_df) * [random.randint(1, 10)])
                    
                df_list = [time_hist1, time_hist2, time_hist3]
                time_hist_df = pd.concat(df_list, axis = 1)
                time_hist_df.columns = ['time_hist1', 'time_hist2', 'time_hist3']
                time_hist_df = time_hist_df / 10
                                
                # add coordinates and original states
                df_coords = df_orig.loc[:, 'point_id':'wgs_y']
                state1 = pd.DataFrame({'state':len(df_coords) * [initial_state]})
                df1 = pd.concat([df_coords, state1], axis=1)
                 
                # combine
                df_coords_state = pd.concat([df1])#, df2, df3])
                df_coords_state.reset_index(drop=True, inplace=True)
                    

                ### then convert to numpy and tensors ---
             
                # state
                state = state_df.to_numpy(dtype = 'int16')
                state = tf.cast(state, tf.int32)
                state.set_shape([None])
                
                #resttime
                restime = tf.cast(restime_array, tf.float32)
                restime.set_shape([None])
                
                # state history
                states_hist = states_hist_df.to_numpy(dtype = 'int16')
                states_hist = tf.cast(states_hist, tf.int32)
                states_hist.set_shape([None, 3])
                
                # time history
                time_hist = time_hist_df.to_numpy(dtype = 'float32')
                time_hist = tf.cast(time_hist, tf.float32)
                time_hist.set_shape([None, 3])

                #site
                site = state_df1[['whc_extract', 'sand_extract', 'depth_extract', 'r_nitro']] / [1000, 100, 10000, 100]
                site = site.to_numpy(dtype = 'float32')
                site = tf.cast(site, tf.float32)
                site.set_shape([None, 4])
                 
                #climate
                scaling_array = np.concatenate([10*[20.], 10*[10.], 10*[20.], 10*[1.], 120*[30.], 120*[20.], 140*[1.], 100*[1.]]).flatten()
                climate = state_df1.loc[:, 'MAT_1':'npp_10_10'] / scaling_array
                climate = climate.to_numpy(dtype = 'float32')
                climate = np.reshape(climate, (-1, 10, 52), order='F')
                climate = tf.cast(climate, tf.float32)
                climate.set_shape([None, 10, 52])
                        
                
                ### make prediction with the model ---
                
                x = (state, states_hist, restime, time_hist, site, climate)
                predictions = self.model.predict(x)
                
                # output dimensions - 5 for the five most probable classes
                k = 5
                
                state_proba, state_index = tf.nn.top_k(predictions[0], k=k)
                time_proba, time_index = tf.nn.top_k(predictions[1], k=k)
                
                state_index_df = pd.DataFrame(state_index)
                
                # pdb.set_trace()
                # state_index_df = state_index_df.applymap(str)
                state_index_df = state_index_df.applymap(self.inverse_dict.get)
                state_index_df = state_index_df.set_axis([f'pred_state_{i+1}' for i in range(k)], axis = 1, copy = False)
                
                state_proba = pd.DataFrame(state_proba)
                state_proba = state_proba.set_axis([f'state_proba_{i+1}' for i in range(k)], axis = 1, copy = False)
                time_proba = pd.DataFrame(time_proba)
                time_proba = time_proba.set_axis([f'time_proba_{i+1}' for i in range(k)], axis = 1, copy = False)
                time_index = pd.DataFrame(time_index) + 1
                time_index = time_index.set_axis([f'time_index_{i+1}' for i in range(k)], axis = 1, copy = False)
                starting_state = pd.DataFrame(state_df1['state'].reset_index(drop = True))
                starting_state.columns = ['starting_state']

                # Resetting indexes for alignment before concatenation
                df_coords_state = df_coords_state.reset_index(drop=True)
                starting_state = starting_state.reset_index(drop=True)
                state_index_df = state_index_df.reset_index(drop=True)
                state_proba = state_proba.reset_index(drop=True)
                time_index = time_index.reset_index(drop=True)
                time_proba = time_proba.reset_index(drop=True)

                df_new = pd.concat([df_coords_state, starting_state, state_index_df, state_proba, time_index, time_proba], axis = 1)
                
                file_name_t = sfname.name
                # file_name_t = file_name_t.replace('prep', 'pred')
                prep_pre = self.file_props['prefix_prep']
                pred_pre = self.file_props['prefix_pred']
                ext_prep = self.file_props['file_ext_prep']
                ext_pred = '_' + init_state + '.' + \
                        self.file_props['file_ext_pred']
                if prep_pre != '' and pred_pre != '':
                    file_name_t = file_name_t.replace('prep_' + prep_pre,\
                                                      'pred_' + pred_pre)
                elif prep_pre == '' and pred_pre != '':
                    file_name_t = file_name_t.replace('prep',\
                                                      'pred_' + pred_pre)
                elif prep_pre != '' and pred_pre == '':
                    file_name_t = file_name_t.replace('prep_' + prep_pre,\
                                                      'pred')
                elif prep_pre == '' and pred_pre == '':
                    file_name_t = file_name_t.replace('prep', 'pred')
                file_name_t = file_name_t.replace('.' + ext_prep, ext_pred)
                # pdb.set_trace()
                ft.write_feather(df_new, self.save_pred / file_name_t)
                    

