# Workflow para la 'base maestra':

- 0-3 load/clean_data.R toma datasets de 4 juntas y junta 7 por separado, junta/limpia y guarda 'observaciones.RDS'
- 4_pini toma 'observaciones.RDS', guarda 'observaciones.RDS'
- 5_expande toma 'observaciones.RDS', selecciona giros, guarda 'observaciones_expandido.RDS'
- 6_select toma 'observaciones_expandido.RDS', selecciona variabels, 'observaciones_selected.RDS'