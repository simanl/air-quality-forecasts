# SIMA Pronósticos de Calidad del Aire README

## 1: Setup de Ambiente de Desarrollo

Éste proyecto está configurado para correr e instalar de manera rápida usando [Docker](https://www.docker.com/docker-engine) y [Docker Compose](https://www.docker.com/docker-compose), sin necesidad de instalar otra cosa en el equipo - como R o Ruby.

#### NOTA para usuarios de Mac y Windows

Los usuarios de Mac y Windows deben instalar el ["Docker Toolbox"](https://www.docker.com/toolbox), el cual incluye [Docker Machine](https://www.docker.com/docker-machine) y [Oracle Virtualbox](https://www.virtualbox.org) por default. Una vez instalado, un host virtual de Docker puede ser creado de manera fácil para albergar los contenedors del proyecto, siguiendo los siguientes pasos:

```bash
# Crear el host virtual de Docker - tengan en consideración el número de CPU's y
# la memoria RAM que quieran asignar a la máquina virtual:
docker-machine create --driver virtualbox \
  --virtualbox-cpu-count 2 \
  --virtualbox-memory 4096 my-docker-host

# Revisar (ver) las variables de entorno necesarias para conectarse al host:
docker-machine env my-docker-host

# Exportar las variables de entorno para conectarse al host de Docker:
eval $(docker-machine env my-docker-host)

# En cualquier momento pueden:

# 1: parar el host virtual: en cualquier momento:
docker-machine stop my-docker-host

# 2: iniciar el host virtual:
docker-machine start my-docker-host

# 3: Re-iniciar:
docker-machine restart my-docker-host
```

### Clonar e iniciar el ambiente de desarrollo:

En el primer momento en el que se corre el comando `docker-compose up`, docker descargará las versiones requeridas de R y Ruby, creará  los contenedores aislados del resto del ambiente local y ligará los servicios requeridos entre ambos, e iniciará los contenedores creados.

Posteriormente, cada contenedor instalará dentro de sí las dependencias del proyecto, y en el caso del contenedor de R, permanecerá corriendo:

```bash
# Clonar el proyecto:
git clone git@github.com:vovimayhem/air-quality-forecast-engine.git

# Cambiarse al directorio del proyecto recién clonado:
cd air-quality-forecast-engine

# Crear e iniciar solamente el contenedor de R + Rserve
# Éste quedará corriendo en el fondo:
docker-compose up -d engine

# O bien, si se desea, iniciar una sesión de R con el código del proyecto:
docker-compose run --rm engine R

# Si se desea, correr el contenedor interactivo de ejemplo de cliente de Ruby:
docker-compose run --rm client
```

### Controlando los contenedores activos

En cualquier momento se puede parar, iniciar o re-iniciar los contenedores activos:

```bash
# Parar todos los contenedores activos:
docker-compose stop

# Iniciar los contenedores activos:
docker-compose start
```

## 2: Liberación a un ambiente productivo

Para liberar el proyecto a un host de Docker "productivo", la imagen de la aplicación debe ser compilada y publicada al [Hub de Docker](https://hub.docker.com) (si el repositorio es privado, solo los colaboradores tendrán acceso a él):

```bash
# Revisar previamente el tag de versión!

# Compilar la imagen con el tag de version (en éste ejemplo, la versión 3):
docker build -t vovimayhem/air-quality-forecast-engine:v3 .

# Autenticarse con el hub de Docker:
docker login

# Publicar la imagen compilada en el hub de Docker:
docker push vovimayhem/air-quality-forecast-engine:v3
```

Al terminar, la imagen compilada de la aplicación estará disponible para que el host "de producción" de Docker (ej. un cluster de  [Rancher](http://rancher.com)) pueda descargarla del [Hub de Docker](https://hub.docker.com) y ejecutarla.

## TODO

* How to run the test suite

* Services Description (job queues, cache servers, search engines, etc.)
