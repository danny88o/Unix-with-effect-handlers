#!/bin/sh

setup_sync() {
    STACK_SIZE = 0 #300000000

    koka -c benches/sync_param.kk --stack "$STACK_SIZE" -o benches/out/sync_param.kk.out
    koka -c benches/sync_ref.kk   --stack "$STACK_SIZE" -o benches/out/sync_ref.kk.out
    koka -c benches/sync_state.kk --stack "$STACK_SIZE" -o benches/out/sync_state.kk.out

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


setup_fs() {
    koka benches/fs_fs.kk -o benches/out/fs_fs.kk.out
    koka benches/fs_unified.kk -o benches/out/fs_unified.kk.out

    chmod u+x benches/out/fs_fs.kk.out
    chmod u+x benches/out/fs_unified.kk.out
}

run_fs() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
        --parameter-scan n ${2:-1000} ${3:-$2} -D 100 \
        --runs ${1:-5} './benches/out/fs_unified.kk.out {n}' './benches/out/fs_fs.kk.out {n}'
}

setup_session() {
    koka benches/session_1.kk -o benches/out/session_1.kk.out
    koka benches/session_2.kk -o benches/out/session_2.kk.out
    koka benches/session_3.kk -o benches/out/session_3.kk.out

    chmod u+x benches/out/session_1.kk.out
    chmod u+x benches/out/session_2.kk.out
    chmod u+x benches/out/session_3.kk.out
}

run_session() {
hyperfine \
    --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
    --parameter-scan n ${2:-1000} ${3:-$2} -D 100 \
    --runs ${1:-5} './benches/out/session_1.kk.out {n}' './benches/out/session_2.kk.out {n}' './benches/out/session_3.kk.out {n}' \
    --export-csv benches/out/session.csv

}