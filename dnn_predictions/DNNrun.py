# -*- coding: utf-8 -*-
"""
Created on Mon Mar  6 10:16:10 2023

@author: gu47yiy
"""

import pathlib
import numpy as np
import re
import pandas as pd
# import tkinter as tk
from tkinter import filedialog as tk_fd
import pprint
import platform
import itertools
import pyarrow
import sys
import functools

import pyarrow.feather
import tensorflow as tf
import os

import PrepData as prep
import PredictionScript as predict

import pdb

pp = pprint.PrettyPrinter()

class DNNsetup(object):
    def __init__(self, path_config, root_path = 'default'):
        self.path_config = path_config
        self.root_path = root_path
        #pdb.set_trace()
        if root_path == 'default':
            self.root_path = self.setBaseDirectory()
        else: 
            path_exists = False
            while not path_exists:
                path_exists, path = self.checkPathValidity(self.root_path)
                if path_exists:
                    self.root_path = path
                else:
                    print(f'Given reference path \n{root_path} \nnot valid.\n')
        
        self.init_dicts()    
        
        self.file_props = dict()
        self.prep_data = dict()
        
        self.loadPathConfig()
        self.loadSpecYeScenInit()
        self.loadConfigFiles()
        
        self.menu()
        
    def menu(self):
        print('\nAuswahl treffen:\n')
        print('(1) Load config-files\n')
        print('(2) Show currently loaded config\n')
        print('(3) Prepare data (no prediction)\n')
        print('(4) Run prediction\n')
        print('(5) GPU selection\n')
        print('(6) Quit\n\n')
        
        uInp = input('> ')
        print('\n')
        
        if uInp in ['1', '2', '3', '4', '5', '6']:
            # if uInp == '1' or uInp == '2':
            #     print('Messergebnisse inkl. Plots und Zwischenspeichern wird in Ordner auf Desktop abgelegt.\n')
            #     uDir = input('\nBezeichnung für Messung bzw. Ordner eingeben > ')
            #     os.mkdir(os.environ['USERPROFILE'] + '\\Desktop\\' + uDir)
            if uInp == '1':
                self.init_dicts()
                self.loadPathConfig()
                self.loadSpecYeScenInit()
                self.loadConfigFiles()
                # pdb.set_trace()
                self.menu()
            elif uInp == '2':
                self.showConfig()
                self.menu()
            elif uInp == '3':
                self.prepareData()
                # self.checkExistingFiles(self.input_data, self.paths['load_prep_data'])
                # self.checkFiles()
                self.menu()
            elif uInp == '4':
                self.runPrediction()
                
                self.menu()
            elif uInp == '5':
                uInp2 = '0'
                while uInp2 != '3':
                    print('(1) List GPUs\n')
                    print('(2) Select GPU\n')
                    print('(3) Back to menu\n')
                    
                    uInp2 = input('> ')
                    print('\n')
                    
                    if uInp2 in ['1', '2']:
                        if uInp2 == '1':
                            gpu_list = tf.config.experimental.list_physical_devices('GPU')
                            pp.pprint(gpu_list)
                            print("\nNum GPUs Available: ", len(gpu_list))
                            
                        elif uInp2 == '2':
                            print('Select GPU(s).\n')
                            print('For more than one GPU use a comma-separated list without whitespaces, e. g. "0,1"\n')
                            gpuInp = input('> ')
                            print('\n')
                            os.environ['CUDA_VISIBLE_DEVICES'] = gpuInp
                            
                            uInp2 = '3'
                            
                self.menu()
            elif uInp == '6':
                print('\nEnde!\n')

    
    def setBaseDirectory(self):
        '''Sets base directory to Public/public/F: depending on operating system.
        Paths in path_config should always be set relative to /Public
        '''
        
        cur_os = platform.system()

        if cur_os == "Windows":
            out = "F:/"
        elif cur_os == "Linux":
            if pathlib.Path("/data/public/").exists():
                out = "/data/public/"
            else:
                out = "/data/public/"
                
        out = "F:/" if cur_os == "Windows" else "/data/public"
        
        return pathlib.Path(out)
    
    
    def init_dicts(self):
        self.input_data = dict({'species': [], 'scenarios': [],\
                                'years': [], 'init_states': []})
        self.paths = dict({'climate_model': '', 'dnn_model': '', \
                           'sql_data': '', 'input_data_raw': '', \
                           'save_prep_data': '', 'load_prep_data' : '', \
                           'save_dnn_pred': '', \
                           'species': '', 'init_states': '', 'years': '',\
                           'scenarios': '', 'prep_data': ''})
        self.paths_exist = dict({'climate_model': 0, 'dnn_model': 0, \
                           'sql_data': 0, 'input_data_raw': 0, \
                           'save_prep_data': 0, 'load_prep_data': 0, \
                           'save_dnn_pred': 0, \
                           'species': 0, 'init_states': 0, 'years': 0,\
                           'scenarios': 0, 'prep_data': 0})
    
    def loadPathConfig(self):
        '''Uses root path to load all the paths used in the calculations.
        
        Returns:
            dict with paths.
        '''
        
        print('Loading file with specified paths.\n')
        config_exists, path_config = self.checkPathValidity(self.path_config)
        
        if config_exists:
            
            b = dict()
            with path_config.open() as file:
                for line in file.readlines():
                    temp = line.strip().split()
                    if not np.size(temp) == 0:
                        if np.size(temp) == 1:
                            b[temp[0]] = ''
                        else:
                            b[temp[0]] = temp[-1].strip()
                    
            # self.paths = b
            
            self.paths = dict({'climate_model': \
                    pathlib.Path(b['climate_model_dir']) / b['climate_model'], \
                               'dnn_model': \
                    pathlib.Path(b['path_model_dir']) / b['dnn_model'], \
                               'sql_data': \
                    pathlib.Path(b['path_sql_dir']) / b['sql_model'], \
                               'input_data_raw': \
                    pathlib.Path(b['path_raw_data']), \
                               'save_prep_data': \
                    pathlib.Path(b['path_prep_data']), \
                               'load_prep_data': \
                    pathlib.Path(b['load_prep_data']), \
                               'save_dnn_pred': \
                    pathlib.Path(b['path_results_dir']), \
                               'species': \
                    pathlib.Path(b['species']), \
                               'init_states': \
                    pathlib.Path(b['init_states']), \
                               'years': 
                    pathlib.Path(b['years']), \
                               'scenarios': \
                    pathlib.Path(b['scenarios']), \
                                'prep_data': \
                    pathlib.Path(b['prep_data'])})
                
            self.file_props = dict({'prefix_prep': b['prefix_prep'],
                                    'prefix_pred': b['prefix_pred'],
                                    'suffix': b['suffix'],
                                    'file_ext_raw': b['file_ext_raw'],
                                    'file_ext_prep': b['file_ext_prep'],
                                    'file_ext_pred': b['file_ext_pred']})
            
            # pdb.set_trace()
            print('Checking, if paths exist.\n')
            
            for which_path, path in self.paths.items():
                path_exists, path = self.checkPathValidity(self.root_path / path)
                self.paths_exist[which_path] = path_exists
                self.paths[which_path] = path

        # self.menu()
        
    
    def showConfig(self):
        '''Simple function to print currently loaded data. Uses PrettyPrinter.
        '''
        print('Defined initial states, scenarios, species and years:\n')
        pp.pprint(self.input_data)
        
        print('\nDefined paths:\n')
        pp.pprint(self.paths)
        
        print('\nAdditional file properties:\n')
        pp.pprint(self.file_props)
        
        print('\nConfiguration used for preparing data:\n')
        pp.pprint(self.prep_data)
        
        # self.menu()
        
    
    def checkFiles(self):
        self.checkExistingFiles(self.input_data, self.paths['input_data_raw'])
    
    
    def checkExistingFiles(self, input_data, path, ext = 'parquet'):
        '''Given scenarios, species, etc. in input_data, checks if there 
        is existing data in given path.
        '''
        # file_list_expr = [] 
        files_list = []
        
        caller = sys._getframe(1).f_code.co_name
        # print(caller)
        
        pat_spec = '(?P<species>\\w*)_'
        pat_year = '(?P<year>[0-9]{4}-[0-9]{4})_'
        pat_scen = '(?P<scenario>[-\\w]*)[.]'
        # pat_prefix_prep = f"(?P<prefix_p>{self.file_props['prefix_prep']})_"
        suf = self.file_props['suffix']
        
        if caller == 'prepareData':# or 'checkFiles':
            print('CheckFiles called by prepareData\n')
            pattern = pat_spec + pat_year + pat_scen + self.file_props['file_ext_raw']
            # pdb.set_trace()
        elif caller == 'runPrediction':
            print('CheckFiles called by runPrediction\n')
            # n = '{4}'
            # pattern = f'(?P<typ>pred)_?(?P<prefix>.*)'\
            #     f'__(?P<species>\w+)_(?P<year>[0-9]{n}-[0-9]{n})'\
            #         f'_(?P<scenario>.+)__(?P<suf>{suf}?)[.](?P<extension>[a-z]+)'
            prefix_prep_raw = self.file_props['prefix_prep']
            
            if prefix_prep_raw != '':
                prefix_prep = '_' + f"(?P<prefix_p>{prefix_prep_raw})_"
            else:
                prefix_prep = '_'
                            
            # pattern = f'(?P<typ>pred)' + prefix_prep +\
            #     f'_(?P<species>\w+)_(?P<year>[0-9]{n}-[0-9]{n})'\
            #         f'_(?P<scenario>.+)_(?P<suf>{suf}?)[.](?P<extension>[a-z]+)'
            pattern = "(?P<typ>prep)" + prefix_prep +\
                pat_spec + pat_year + pat_scen +\
                    self.file_props['file_ext_prep']
                    
                                
        files = path.glob('*')
        
        for file in files:
            if file.is_file():
                cur_name = file.name
                cur_match = re.search(pattern, cur_name)
                # pdb.set_trace()
                if cur_match:
                    match_dict = cur_match.groupdict()
                    cur_scen = match_dict['scenario'].removesuffix('_' + suf)
                    # pdb.set_trace()
                    if (match_dict['species'] in self.input_data['species'] and
                        match_dict['year'] in self.input_data['years'] and
                        
                        cur_scen in self.input_data['scenarios']):
                        match_dict['path'] = file
                        files_list.append(match_dict)        
                    
        # label_iterator = itertools.product(input_data['species'], \
        #                                     input_data['years'], \
        #                                     input_data['scenarios'])
        
        # for label in label_iterator:
        #     # pdb.set_trace()
        #     cur_file = '*' + '*'.join(label) + '.' + ext
        #     # file_list_expr.append(cur_file)
        #     file_list_full_path += path.glob(cur_file)
        # print(path)
        # file_list_full_path = list(path.glob('*.' + ext))
        
        print(f'\nFound {len(files_list)} files.\n')   
        # print(files_list)
        
        return files_list
            
    
    # def checkCaller(self, func):
    #     '''Decorator used to make sure checkExistingFiles look for the right
    #     files. Also, because fun.
    #     '''
    #     @functools.wraps(func) # preserve __name__ of func
    #     def wrapper_checkCaller(*args, **kwargs):
    #         caller = sys._getframe(1).f_code.co_name
    #         if caller == "prepareData":
                
            
    
    def checkPathValidity(self, path, default = '0'):
        '''Checks, if path exist
        s. If not, user can decide whether to cancel
        or browse for the right path.
        '''
        
        print(f'Check path:\n{path}\n')
        path = pathlib.Path(path)
        return_value = True
        
        if path.exists():
            print('Path exists! Continue...\n')
        else:
            if default == '0':
                print('Could not find path. Choose what to do:\n')
                print('(1) Cancel\n(2) Browse for path\n(3) Type path manually\n\n')
            
                uInp = input('> ')
            else:
                uInp = default
            
            if uInp == '1':
                return_value = False
                print(f'{path} not found. Canceled by user.\n')
                path = []
            elif uInp == '2':
                path = pathlib.Path(tk_fd.askopenfilename())
                return_value = True
            elif uInp == '3':
                print('Path not found. Type in path\n')         
                input_path = pathlib.Path(input('> '))
                if input_path.exists():
                    return_value = True
                    path = input_path
                else:
                    return_value, path = self.checkPathValidity(input_path, default = '3')                
        
        return return_value, path
        
    
    def loadSpecYeScenInit(self):
        
        for n, key in enumerate(self.input_data.keys()):
            if self.paths_exist[key]:
                print(f'Load {key} data\n')
                with self.paths[key].open() as file:
                      for line in file.readlines():
                          temp = line.strip().split()[0]
                          if temp and temp[0] != '#':
                              self.input_data[key].append(temp)
                              # pdb.set_trace()
            else:
                print(f'Skip {key}, path not defined\n')
                
    
    def loadConfigFiles(self):
        
        if self.paths_exist['prep_data']:
            c = dict()
            with self.paths['prep_data'].open() as file:
                # pdb.set_trace()
                for line in file.readlines():
                    temp = line.strip().split()
                    if not np.size(temp) == 0:
                        cur_key = temp[0]
                        cur_value = temp[-1]
                        
                        if cur_key in ("scaling_clim", "scaling_site"):
                            c[cur_key] = np.array([np.float64(x) \
                                                   for x in cur_value.split(";")])
                        elif cur_key == "soil_scale_c":
                            c[cur_key] = cur_value.split(";")
                            if c[cur_key][1] in ("True", "true", "1"):
                                c[cur_key][1] = True
                            else:
                                c[cur_key][1] = False
                        
                        elif cur_key == "soil":
                            t_dict = dict()
                            for props in cur_value.split(";"):
                                cur_prop = props.split(",")
                                t_dict[cur_prop[0]] = cur_prop[1]
                            c[cur_key] = t_dict
                    
            self.prep_data = c
        

    def prepareData(self):#, input_data = self.input_data,\
                    #load_raw_data = self.paths['input_data_raw'],\
                    #save_prep_data = self.paths['save_prep_data'],\
                    #climate_model = self.paths['climate_model'],\
                    #prep_data = self.prep_data):
        '''Function used to do the preprocessing of the data.
        Paths for climate_model, input_raw_data, input data (pat_data_dir),
        directory where the data should be saved (path_results_dir), path to
        init file for preprocessing (prep_data), as well as species, init_states,
        years, and scenarios must be set and valid.
        '''
        keys = np.array(['climate_model', 'input_data_raw', 'save_prep_data', \
                         'species', 'init_states', 'years', 'scenarios', 'prep_data'])
        
        valid = np.array([self.paths_exist[key] for key in keys])
    
        if False in valid:
            print(f'{keys[~valid]} not set. Cancel!\n')
        else:
            input_data = self.input_data
            load_raw_data = self.paths['input_data_raw']
            save_prep_data = self.paths['save_prep_data']
            climate_model = self.paths['climate_model']
            prep_data = self.prep_data
            file_props = self.file_props

            # input_files = []
            input_files = self.checkExistingFiles(self.input_data, \
                self.paths['input_data_raw'])
                       
            print('Processing...\n')
            # pdb.set_trace()
            if input_files: 
                Processor = \
                    prep.SVD_Preprocessing(spec_scen_year_init = input_data,\
                                           load_dir = load_raw_data, \
                                           save_dir = save_prep_data,\
                                           climate_model = climate_model,\
                                           prep_dict = prep_data,\
                                           file_props = file_props,\
                                           input_files = input_files)
                                               
                
                Processor.process()
            else:
                print('Check file names and paths of climate data.\n')
                
            print('Done!\n')
      
        
    def runPrediction(self):
                
        sql = self.paths['sql_data']
        dnn_model = self.paths['dnn_model']
        load_data = self.paths['load_prep_data']
        save_pred = self.paths['save_dnn_pred']
        input_data = self.input_data
        load_raw_data = self.paths['input_data_raw']
        save_prep_data = self.paths['save_prep_data']
        climate_model = self.paths['climate_model']
        prep_data = self.prep_data
          
        # print('Check for existing preprocessed data...\n')
        
        # files = self.checkExistingFiles(input_data, load_data)
        
        # # pdb.set_trace()
        
        # if len(files) == 0:
        #     print('Prepare data, run preprocessing...\n')
        #     Processor = \
        #         prep.SVD_Preprocessing(spec_scen_year_init = input_data,\
        #                                 load_dir = load_raw_data, \
        #                                 save_dir = save_prep_data,\
        #                                 climate_model = climate_model,\
        #                                 prep_dict = prep_data)
           
        #     Processor.process()
            
        #     print('Done\n')
        # input_files = []
        input_files = self.checkExistingFiles(self.input_data, \
            self.paths['load_prep_data'])
        
        # pdb.set_trace()
        if input_files: 
            dnn = predict.SVD(self.input_data, \
                       simulation_sql = sql, \
                       dnn_model = dnn_model,\
                       load_data = load_data,\
                       save_pred = save_pred,\
                       file_props = self.file_props,\
                       input_files = input_files)
                
            
            print('Run SVD_DNN...\n')    
            dnn.predict()
        else:
            print('Check file names and paths of preprocessed data.\n')
        print('Done!\n')
        
                  
        
if __name__ == '__main__':
    # root_path = 'A:\\'
    # root_path = 'F:\\Projects\\Resonate\\svd_dnn_scripts\\path_config'
    root_path = '/data/public/Projects/Resonate/svd_dnn_scripts/path_config'
    path_config = pathlib.Path(root_path) 
    DNNsetup(path_config = path_config)#, root_path = root_path)
