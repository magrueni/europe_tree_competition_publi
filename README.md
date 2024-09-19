# europe_tree_competition_publi
This repository contains code for the publication "Loss of competitive strength in European conifer species under climate change"  

There are several folders:
1. preprocessing: 
   This folder contains scripts that were used for data preprocessing. Hereby we used the data from the harmonized simulation database: https://zenodo.org/records/12750180
   The first steps are done in R to extract forest state transitions from the simulation database.
   In a second part, this information is then used to extract simulation years with corresponding daily climate information from the harmonized simulation database.
   This second part is done in a python script, as a climate compressor DNN is applied to reduce the dimensions in the training data. See supplementary material for detailed description
   
   
2. dnn: 
  This folder contains the code to train the deep neural network


3. dnn_predictions: 
   This folder contains the code to run dnn predictions with the training dnn for different forest states.


4. Analysis: 
   This folder contains all code to run the statistical analysis and produce the figures.

5. Functions: This folder contains functions for the analysis
