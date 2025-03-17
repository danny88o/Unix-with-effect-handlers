#!/bin/sh

setup_sync() {
    koka benches/sync_param.kk --stack 16000000 -o benches/out/sync_param.kk.out -O=0
    koka benches/sync_ref.kk   --stack 16000000 -o benches/out/sync_ref.kk.out   -O=0
    koka benches/sync_state.kk --stack 16000000 -o benches/out/sync_state.kk.out -O=0

    chmod u+x benches/out/sync_param.kk.out
    chmod u+x benches/out/sync_ref.kk.out
    chmod u+x benches/out/sync_state.kk.out
}

run_sync() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
        --parameter-scan n $1 $2 -D 1 \
        --runs 5 './benches/out/sync_ref.kk.out {n}' './benches/out/sync_param.kk.out {n}' './benches/out/sync_state.kk.out {n}'
}

