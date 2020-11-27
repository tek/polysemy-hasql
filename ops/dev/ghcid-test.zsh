#!/usr/bin/env zsh

setopt err_exit no_unset

pkg=$1 module=$2 name=$3 type_=$4 runner=$5
base=${0:h:h}
$base/hpack.zsh
$(nix-build --no-link -A integration.ensurePostgresVm)/bin/ensure-postgres-vm
export polysemy_db_test_host=localhost
export polysemy_db_test_port=10000
nix-shell $*[6,$] --pure -A 'ghcid.run' \
  --argstr pkg $pkg \
  --argstr module $module \
  --argstr name $name \
  --argstr 'type' $type_ \
  --argstr 'runner' $runner \
  --keep polysemy_db_test_host \
  --keep polysemy_db_test_port \
  --run exit
