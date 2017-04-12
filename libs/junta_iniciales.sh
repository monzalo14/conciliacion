#!/usr/bin/sh

## Script que junta las bases de todos los capturistas

## Requiere:
# 1. Trabajar en el directorio raíz que contenga todos los subdirectorios de los capturistas.
# 2. Que se especifique un archivo en .csv con los nombres (puede ser una misma base)

now=$(date +'%m_%d_%Y')
name_file=$('iniciales_nombres.csv')

for filename in $(ls -R *.{xlsx,xlsm}); do
in2csv -d '|' $filename | awk 'NR > 23 { print }' >> bases_capturistas_$now.csv
done

nombres=$(csvcut -n $name_file | sed -E s/[[:space:]]\{0,4\}[0-9]+:\ //g | sed -E s/[[:blank:]]+/\|/g)

head bases_capturistas_$now.csv.csv | sed '1s/.*/$nombres/g' > bases_capturistas_$now.csv