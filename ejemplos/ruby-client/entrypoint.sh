#! /bin/bash

set -e

: ${APP_PATH:="/usr/src/app"}
: ${APP_TEMP_PATH:="$APP_PATH/tmp"}
: ${APP_SETUP_LOCK:="$APP_TEMP_PATH/setup.lock"}
: ${APP_SETUP_WAIT:="5"}

# 1: Define the functions lock and unlock our app containers setup processes:
function lock_setup { mkdir -p $APP_TEMP_PATH && touch $APP_SETUP_LOCK; }
function unlock_setup { rm -rf $APP_SETUP_LOCK; }
function wait_setup { echo "Waiting for app setup to finish..."; sleep $APP_SETUP_WAIT; }

# 2: 'Unlock' the setup process if the script exits prematurely:
trap unlock_setup HUP INT QUIT KILL TERM

# 3: Wait until the setup 'lock' file no longer exists:
while [ -f $APP_SETUP_LOCK ]; do wait_setup; done

# 4: 'Lock' the setup process, to prevent a race condition when the project's
# app containers will try to install gems and setup the database concurrently:
lock_setup

# 5: Check or install the app dependencies via Bundler:
bundle check || bundle

# 6: Set 'pry' as the console for bundler:
bundle config console pry

# 7: 'Unlock' the setup process:
unlock_setup

export PATH=${APP_PATH}/bin:${PATH}

# 8: Especificar el comando default:
if [ -z "$1" ]; then set -- bundle console "$@"; fi

# 9: Execute the given or default command:
exec "$@"
