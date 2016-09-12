#!/bin/bash
set -e

# 1: Especificar el comando default si no existe:
if [ -z "$1" ]; then set -- Rscript start.R "$@"; fi

# 2: Si el comando es el de arranque del servidor de RServe:
if [ "$1" = 'Rscript' ]; then
  # Asegurarse de que existen los directorios de las tablas
	mkdir -p /app/tablas
  # Asegurarse que el directorio de la app solo puede ser accesado por el owner
	chmod 700 /app/tablas
  # Asegurarse que el directorio de la app le pertenece al usuario y grupo de 'pronosticos'
	chown -R pronosticos:pronosticos /app/tablas
  # Ejecutar el comando como el usuario 'pronosticos'
	exec gosu pronosticos "$@"
fi

exec "$@"
