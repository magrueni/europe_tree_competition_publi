#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 13:43:48 2023

@author: mbaumann
"""

# import sqlite3
import pandas as pd
import numpy as np
import glob
import pyarrow.feather
# import pyarrow as pa
# import pyarrow.parquet as pq
import itertools
import os
import pathlib
import re

import pdb

import tensorflow as tf
# load the climate compressor
# from tensorflow.keras.models import Sequential, save_model, load_model
import tensorflow.keras.models as tfkm

#%%

class Preprocessor(object):
    '''Class serving as a baseclass for different data preprocessing schemes.
    Includes only fundamental functionality shared by all processing units.
    (as of 08.02.2023)
    Init values:
        file_name_spec: dictionary, 
    '''
    def __init__(self, spec_scen_year_init, load_dir, save_dir, \
                 file_props, input_files = None):
        self.file_name_spec = spec_scen_year_init # dictionary
        self.file_props = file_props
        self.file_ext_pred = self.file_props['file_ext_pred']
        self.file_ext_raw = self.file_props['file_ext_raw']
        
        self.load_dir = pathlib.Path(load_dir)
        self.save_dir = pathlib.Path(save_dir)
        # self.file_names_full_path, self.file_names = self.set_file_names()
        self.file_names_full_path = input_files
        
    def set_file_names(self, file_names = None, file_ext = None, path = None):
        if file_names == None:
            file_names = self.file_name_spec
        if file_ext == None:
            file_ext = self.file_ext_raw
        if path == None:
            path = self.load_dir
        
        file_list_expr = [] 
        file_list = []
        label_iterator = itertools.product(file_names['species'], \
                                           file_names['years'], \
                                           file_names['scenarios'])
        
        self.suffix_raw = f"{self.file_props['suffix']}.{file_ext}"
        
        all_files = [str(p) for p in list(path.glob('*.dat'))]
            
        for label in label_iterator:
           # pdb.set_trace()
           cur_file = f'{label[0]}.*_{label[1]}_{label[2]}.*.dat'
           r = re.compile(cur_file)
           cur_match = list(filter(r.search, all_files))
           # cur_file = '*' + '*'.join(label) \
           #     + self.suffix_raw
           file_list_expr.append(cur_file)
           # file_list_full_path += path.glob(cur_file)
           file_list.extend(cur_match)

        file_list_full_path = [pathlib.Path(p) for p in file_list]
        # pdb.set_trace()
        return file_list_full_path, file_list_expr
    
    def get_file_names(self):
        
        return self.__file_names


class SVD_Preprocessing(Preprocessor):
    '''Class inherits from Preprocessor. Processing implemented according to
    prediction_data_prep
    '''
    def __init__(self, spec_scen_year_init, load_dir, save_dir, file_props,\

                 
                 climate_model = "/data/public/Projects/Resonate/3pg/\
                     dnn_climate_indices/models/climcompressor_v4.h5", \
                 prep_dict = dict({'scaling_clim' : [10., 3., 20., 1.], \
                                   'scaling_site' : [100, 100, 100, 100, 100], \
                                   'soil_scale_c' : ["const", "True", "numpy"],\
                                   'soil' : dict({"nitrogen": 70, "depth": 90,\
                                                   "pctSand": 45, "pctSilt": 30,\
                                                   "pctClay": 25})}), \
                     input_files = None):
        super().__init__(spec_scen_year_init, load_dir,\
                         save_dir, file_props, input_files)
        self.prep_dict = prep_dict
        self.climate_model = climate_model
        self.scaling_clim = prep_dict['scaling_clim']
        self.scaling_site = prep_dict['scaling_site']
        self.soil_spec = prep_dict['soil']
        self.len_df = None # set in get_data_info()
        self.shape_df = None # set in get_data_info()
                
        self.set_climate_model()
        
        if self.soil_spec == None:
            self.soil_spec = pd.DataFrame({\
                            'nitrogen' : self.len_df * [70], \
                            'depth' : self.len_df * [90], \
                            'pctSand' : self.len_df * [45], \
                            'pctSilt' : self.len_df * [30], \
                            'pctClay' : self.len_df * [25]}\
                            )

    def set_climate_model(self):#, model = None, path = None):
        
        self.model_tas = tfkm.load_model(self.climate_model, compile = True) 
    
    
    def get_data_info(self):
        example_file = self.file_names_full_path[0]
        df = pyarrow.feather.read_feather(example_file)
        df = df.sort_values("year")
        
        self.len_df = len(df)
        self.shape_df = df.shape
        
        print(f"{len(self.file_names)} files found.\n")
        print(f"Look into {example_file}:\n")
        print(f"Number of columns: {self.shape_df[0]}\n")
        print(f"Number of rows: {self.shape_df[1]}\n")
        
        
    def set_soil(self, df_sh = None):
        '''Set soil properties. 
        Can be extended to work with different types of input data, like files
        or dataframes.
        soil_spec[0] contains information about how to deal with the data
        soil_spec[0][0]: currently only "const". 
        soil_spec[0][1]: Bool to indicate whether data should be scaled
        soil_spec[0][2]: return_type, "numpy" or "dataframe"
        '''
        
       
        input_data, scaling, return_type = self.prep_dict['soil_scale_c']
        
        cols = ['nitrogen', 'depth', 'pctSand', 'pctSilt', 'pctClay']
        data = np.array([self.soil_spec[col] for col in cols], dtype = np.float32)
        # cols = temp[:, 0]
        
        if scaling:
            data /= self.scaling_site        
        
        if input_data == "const":
            sh = (df_sh[0], len(cols))

            
            data_np = np.full(sh, data)
            
        if return_type == 'numpy':
            return data_np
        elif return_type == 'dataframe':
            return pd.DataFrame(data=data_np, columns = cols)
        
        
    def process(self):
        # tst = [self.file_names_full_path[0]]
        # for i, fname in enumerate(tst):
        # pdb.set_trace()
        for i, fname in enumerate(self.file_names_full_path):
            # df = pyarrow.feather.read_feather(fname)
            path = fname['path']
            df = pyarrow.feather.read_feather(path)
            df = df.sort_values("year")
            df_sh = df.shape
            
            # separate climate data from other columns and add soil info for climate compressor
            df_base = df.loc[:, "point_id":"r_nitro"]
            df_clim = df.loc[:, "tas_1":"vpd_365"]
            
            # add constant values for site conditions - taken from bins npp basic layers: nitrogen, depth, pctSand, pctSilt, pctClay
           
            # the climate data frame is reformated and then fed to the climate compressor        
            df_clim_np = df_clim.to_numpy(copy = True)
            clim = np.reshape(df_clim_np, (-1, 4, 365))
            scale_clim = np.array(self.scaling_clim).reshape((1, -1, 1))
            
            clim /= scale_clim
            clim = np.moveaxis(clim, 1, 2)
            clim.astype(np.float32)
            clim = tf.convert_to_tensor(clim, dtype = tf.float32)
            
            env = tf.convert_to_tensor(self.set_soil(df_sh), dtype = tf.float32)
            dat = clim, env
            
                              
            # climate compressor
            tas_layer_output = self.model_tas.predict(dat, verbose = 0)
            tas_df = pd.DataFrame(tas_layer_output[1])
            
            tas_df = tas_df.set_axis([f'comp_clim_{i+1}' for i in range(14)], axis = 1)
            tas_df = tas_df.reset_index(drop=True)
            
            npp_df = pd.DataFrame(tas_layer_output[0])
            npp_df = npp_df.set_axis([f'npp_{i+1}' for i in range(10)], axis = 1)
            npp_df = npp_df.reset_index(drop=True)
                        
            df_clim_averages = pd.DataFrame()
            df_clim_averages["MAT"] = df_clim.loc[:, 'tas_1':'tas_365'].mean(axis = 1)
            df_clim_averages["ANP"] = df_clim.loc[:, 'prec_1':'prec_365'].mean(axis = 1)
            df_clim_averages["Mrad"] = df_clim.loc[:, 'rad_1':'rad_365'].mean(axis = 1)
            df_clim_averages["Mvpd"] = df_clim.loc[:, 'vpd_1':'vpd_365'].mean(axis = 1)
            
            days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            day_in_year = 0
            
            for n, days in enumerate(days_in_month):
                df_clim_averages["tas_month" + str(n+1)] = \
                df_clim.iloc[:, day_in_year:day_in_year + days].mean(axis = 1)
                
                day_in_year += days
                
            day_in_year = 0
            
            for n, days in enumerate(days_in_month):
                df_clim_averages["prec_month" + str(n+1)] = \
                df_clim.iloc[:, 365 + day_in_year: 365 + day_in_year + days].mean(axis = 1)
                
                day_in_year += days

            df_clim_averages = df_clim_averages.reset_index(drop=True)
            df_base = df_base.reset_index(drop=True)
            # put everything together
            # df_base: soil data
            # df_clim_averages: climate averages
            # tas_df: climate data after compressor
            # npp_df: predicted data, climate data and soil data
            df_fin = pd.concat([df_base, df_clim_averages, tas_df, npp_df], axis = 1)
            
            # then reshape the data so that the years of the example are after each other
            df_fin["s"] = df_fin.groupby(["point_id", "wgs_x", "wgs_y", \
                  'whc_extract', 'sand_extract', 'depth_extract', "r_nitro"], \
                dropna = False).cumcount() + 1
              
            df_fin = df_fin.set_index(["s", "point_id", "wgs_x", "wgs_y", \
                                       'whc_extract', 'sand_extract', \
                                       'depth_extract', "r_nitro"]).unstack(0)
            
            df_fin.columns = [f"{x}_{y}" for x,y in df_fin.columns]
            df_new = df_fin.reset_index()
            
            df_new_reduced = df_new[["point_id", "wgs_x", "wgs_y", \
                                     'whc_extract', 'sand_extract', \
                                     'depth_extract', "r_nitro"]]
            
            labels = ["MAT", "ANP", "Mrad", "Mvpd", "month", "comp_clim", "npp"]
            
            
            
            example_final = df_new_reduced.join([df_new.filter(like = label)\
                                                 for label in labels])
            if self.file_props['prefix_prep'] != '':            
                prefix = f"prep_{self.file_props['prefix_prep']}_"
            else:
                prefix = "prep_"
            # if self.file_props['suffix'] != '':
            #     suffix = '.'
            # else:
            #     suffix = '_' + self.file_props['suffix'] + '.'
            # suffix_pred = self.suffix_raw.replace('dat', f"{self.file_props['file_ext_predicted']}")
            fname_t = path.name.removesuffix('dat')
            file_name = prefix + fname_t + \
                self.file_props['file_ext_prep']
            # file_name = prefix + f"{fname.name.replace(self.file_ext_raw, '')}" \
            #     + f"{self.file_props['file_ext_predicted']}"
            
            # pdb.set_trace()
            example_final.to_parquet(\
                self.save_dir / file_name)


if __name__ == "__main__":
    print('Running PrepData as primary process:\n')
    scenarios = ["MPI-M-MPI-ESM-LR_rcp_8_5"]
    species = ["Abies_alba", "Pinus_sylvestris"]
    years = ["2011-2020", "2021-2030", "2031-2040", "2041-2050", "2051-2060", \
                "2061-2070", "2071-2080", "2081-2090", "2091-2100"]
            
    a = dict({"species" : species, \
              "years" : years,\
              "scenarios" : scenarios})
    
    file_props = dict({'prefix': 'test', 'suffix': '_all_grids_v7', \
                       'file_ext_raw': 'dat', 'file_ext_predicted': 'parquet'})
    
    load_dir = "/data/public/Projects/Resonate/svd_dnn/pred_data_clim/"
    # load_dir = "F://Projects//Resonate//svd_dnn//pred_data_clim//"
    
    # save_dir = "data/Public/Projects//Resonate/svd_dnn/pred_data/baumann_tst/"
    save_dir = "F://Projects//Resonate//svd_dnn//pred_data//baumann_tst//"
    
    model_path = "/data/Public/Projects/Resonate/3pg/dnn_climate_indices/models/"
    # model_path = "F://Projects//Resonate//3pg//dnn_climate_indices//models//"
    
    climate_model = pathlib.Path("F://Projects/Resonate/3pg/dnn_climate_indices/models/climcompressor_v4.h5")
    
    Preprocessor = SVD_Preprocessing(a, load_dir, save_dir, \
                                     file_props, climate_model)
    
    Preprocessor.process()
    
    