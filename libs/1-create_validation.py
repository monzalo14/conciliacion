#!/usr/bin/env python 

import pandas as pd
import numpy as np
import random
import pylab as pl
from scipy import optimize
import time
import feather
import csv

# Generamos el df y los criterios de validación

# Leemos los datos 
df = feather.read_dataframe('../data/seguimiento_audiencias_val.feather')

# Arreglamos manualmente dos errorcitos
df[['erd3_litigios','era3_litigios']] = df[['erd3_litigios','era3_litigios']].apply(pd.to_numeric, errors = 'coerce')
df['id'] = df['junta'].map(str) + '_' + df['expediente'].map(str) + '_' + df['anio'].map(str)

# Creamos un diccionario que contenga todos los tipos de variable, con nombre y rango
valida_rango = {'dummies':{'rango':
                     list(range(0,2)), 
               'vars':['registro_actor',
                       'ficha_p_actora',
                       'ficha_p_dem',
                       'registro_p_actora',
                       'registro_p_dem2',
                       'calcu_p_actora',
                       'calcu_p_dem',
                       'aviso_conciliador',
                       'convenio',
                       'desistimiento',
                       'ea4_compra',
                       'ea6_trabaja',
                      'ea7_busca_trabajo']},
        'cat_3':{'rango':
           list(range(0,4)),
            'vars':['emplazamiento','tratamiento']
           },
         'cat_4':{'rango':
                  list(range(0,5)),
                 'vars':['ea3_enojo', 
                           'ea5_estudios', 
                           'ed3_estabilidad',
                           'ed4_enojo', 
                           'era3_litigios',
                           'erd3_litigios']}}

valida_na = ['convenio', 'desistimiento', 'emplazamiento']

# Hago una lista de variables a checar en el df
var_list = []
for i in valida_rango.values():
	for j in i.keys():
		if j not in var_list: 
			var_list.append(j)
		 else: 
		 	continue

var_list = var_list + valida_na

