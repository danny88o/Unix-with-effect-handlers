#!/bin/sh

setup_sync() {
    STACK_SIZE=300000000

    printf "$STACK_SIZE"

    koka -c benches/sync_param.kk --stack "$STACK_SIZE" -o benches/out/sync_param.kk.out -O3
    koka -c benches/sync_ref.kk   --stack "$STACK_SIZE" -o benches/out/sync_ref.kk.out -O3
    koka -c benches/sync_state.kk --stack "$STACK_SIZE" -o benches/out/sync_state.kk.out -O3

    chmod u+x benches/out/sync_param.kk.out
    chmod u+x benches/out/sync_ref.kk.out
    chmod u+x benches/out/sync_state.kk.out
}

run_sync() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
        --parameter-scan n ${2:-15} ${3:-$2} -D 1 \
        --runs ${1:-5} './benches/out/sync_param.kk.out {n}' './benches/out/sync_ref.kk.out {n}' './benches/out/sync_state.kk.out {n}' \
        --export-csv benches/out/sync.csv
}


setup_fs() {
    koka benches/fs_fs.kk -o benches/out/fs_fs.kk.out -O3
    koka benches/fs_unified.kk -o benches/out/fs_unified.kk.out -O3

    chmod u+x benches/out/fs_fs.kk.out
    chmod u+x benches/out/fs_unified.kk.out
}

run_fs() {
    hyperfine \
        --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
        --parameter-scan n ${2:-1000} ${3:-$2} -D 100 \
        --runs ${1:-5} './benches/out/fs_unified.kk.out {n}' './benches/out/fs_fs.kk.out {n}' \
        --export-csv benches/out/fs.csv
}

setup_session() {
    koka benches/session_1.kk -o benches/out/session_1.kk.out -O3
    koka benches/session_2.kk -o benches/out/session_2.kk.out -O3
    koka benches/session_3.kk -o benches/out/session_3.kk.out -O3
    koka benches/session_4.kk -o benches/out/session_4.kk.out -O3

    chmod u+x benches/out/session_1.kk.out
    chmod u+x benches/out/session_2.kk.out
    chmod u+x benches/out/session_3.kk.out
    chmod u+x benches/out/session_4.kk.out
}

run_session() {
hyperfine \
    --prepare 'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches'  \
    --parameter-scan n ${2:-1000} ${3:-$2} -D 100 \
    --runs ${1:-5} './benches/out/session_1.kk.out {n}' './benches/out/session_2.kk.out {n}' './benches/out/session_3.kk.out {n}' './benches/out/session_4.kk.out {n}' \
    --export-csv benches/out/session.csv
}

setup_pipe() {
    koka benches/pipe_bench.kk -o benches/out/pipe_bench.kk.out -O3

    chmod u+x benches/out/pipe_bench.kk.out
}

all() {
    setup_sync    | grep -E "created|failed"
    setup_fs      | grep -E "created|failed"
    setup_session | grep -E "created|failed"
    setup_pipe    | grep -E "created|failed"
}

test_all() {
    benches/out/sync_param.kk.out
    benches/out/sync_ref.kk.out
    benches/out/sync_state.kk.out

    benches/out/fs_fs.kk.out
    benches/out/fs_unified.kk.out

    benches/out/session_1.kk.out
    benches/out/session_2.kk.out
    benches/out/session_3.kk.out
    benches/out/session_4.kk.out

    benches/out/pipe_bench.kk.out
}