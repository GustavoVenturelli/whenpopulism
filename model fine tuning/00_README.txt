### Replication files

The files are organized to be executed in order. This Readme, for instance, starts with "00" in its name. The script that builds the samples starts with "01", and to train the models, you'll need to use the files that begin with "02", and so on. To make things easier, we recommend to use the files from 'files' folder and start from step 02. 

Below is a short descriptive for files and folders:

files: training and testing data, these files should be in stating point
model: empty folder wehre files will be created. 

01_Acertar_base: This file creates different training samples for models. These files are available in the 'files' folders. 
02_Treinamento-*: These files are for training the models. Note that sometimes we only change batch size or the training samples from the previous step. 
03_Consolida_resultados: A script for unifying training results
04_Modelo_base_teste: Generates results on test dataset
05_Geracao_resultados: Best models' results on the full dataset
 
