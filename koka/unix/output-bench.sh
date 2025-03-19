#!/bin/sh

setup_sync() {
    # Stack size
    $stack_size = 0 #300000000

    koka benches/sync_param.kk --stack $stack_size -o benches/out/sync_param.kk.out
    koka benches/sync_ref.kk   --stack $stack_size -o benches/out/sync_ref.kk.out
    koka benches/sync_state.kk --stack $stack_size -o benches/out/sync_state.kk.out

    chmod u+x benches/out/sync_param.kk.out
    chmod u+x benches/out/sync_ref.kk.out
    chmod u+x benches/out/sync_state.kk.out
}

run_sync() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
        --parameter-scan n ${2:-15} ${3:-$2} -D 1 \
        --runs ${1:-5} './benches/out/sync_ref.kk.out {n}' './benches/out/sync_param.kk.out {n}' './benches/out/sync_state.kk.out {n}'
}

