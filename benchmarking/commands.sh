#!/bin/sh

mkdir -p csv
sudo -v

run_koka() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
        --parameter-scan n $3 $4 -D $5 \
        --runs $1 "./benches/koka/$2 {n}" \
        --export-csv csv/$2.csv
        # -N
}

run_unison() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches' \
        --parameter-scan n $3 $4 -D $5 \
        --runs $1 "ucm run.compiled benches/unison/$2 {n}" \
        --export-csv csv/$2.csv
        # -N

}


run_sync() {
    local l=${1:-15}
    local u=${2:-15}
    local step=${3:-1}
    local rep=${4:-1}
    run_koka   "$rep" 'sync_ref.kk.out'   $l $u $step
    run_koka   "$rep" 'sync_param.kk.out'  $l $u $step
    run_koka   "$rep" 'sync_state.kk.out' $l $u $step
    run_unison "$rep" 'fib.uc'            $l $u $step
}

run_fs() {
    local l=${1:-1000}
    local u=${2:-1000}
    local step=${3:-100}
    local rep=${4:-1}
    run_koka   "$rep" 'fs_unified.kk.out' $l $u $step
    run_koka   "$rep" 'fs_fs.kk.out'      $l $u $step
    run_unison "$rep" 'fs.uc'    $l $u $step
}

run_session() {
    local l=${1:-1000}
    local u=${2:-1000}
    local step=${3:-100}
    local rep=${4:-1}
    run_koka   "$rep" 'session_1.kk.out' $l $u $step
    run_koka   "$rep" 'session_2.kk.out' $l $u $step
    #run_koka   "$rep" 'session_3.kk.out' $l $u $step
    run_unison "$rep" 'session.uc'       $l $u $step
}

run_full() {
    local l=${1:-15}
    local u=${2:-15}
    local step=${3:-1}
    local rep=${4:-1}
    run_koka   "$rep" 'full.kk.out' $l $u $step
    run_unison "$rep" 'full.uc'     $l $u $step
}

run_all() {
    local rep=${1:-20}
    run_sync 5 19 1 $rep
    run_fs 1000 5000 100 $rep
    run_session 1000 10000 200 $rep
    run_full 5 100 1 $rep
}

