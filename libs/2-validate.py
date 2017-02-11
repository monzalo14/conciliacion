#!/usr/bin/env python 

# Crear dos funciones de validación. Para cada variable, validar 
# y luego pegarle el nombre de la variable que falló, si no cumple con el criterio
def valida_cat(var, rango):
    df[var][(~df[var].isin(rango)) & (~pd.isnull(df[var]))] = var + '_rango'
    return[df[var]]

def valida_na(var):
    df[var][pd.isnull(df[var])] = var + '_na'
    return[df[var]]

# Corremos ambas validaciones
for val in valida_rango:
    rango_aux = valida_rango[val]['rango']
    for var in valida_rango[val]['vars']:
        valida_cat(var, rango_aux)

for var in valida_na:
    valida_na(var)

# Para cada celda en el df, revisamos si tiene algún error
# En caso de tenerlo, tomamos su id de expediente y su tipo de error
# Estoy SEGURA de que hay una manera mucho más eficiente de hacer esto
# Pendiente: ¿alguna dummy de error y filtrar por esos?
error_list = []
for i in range(len(df.index)):
    for var in var_list:
        if (df[var].iloc[i] == var + '_rango') | (df[var].iloc[i] == var + '_na'):
            error_list.append(df['id'].iloc[i] + ' : ' + df[var].iloc[i])

error_list = [i.split(' : ') for i in error_list]

# Guardamos los errores en un .csv
np.savetxt('validaciones.csv', error_list, delimiter=",", fmt='%s', header = 'validaciones_seguimiento')