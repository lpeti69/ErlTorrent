NODEID=$1
erl \
    -sname node$NODEID \
    -emu_args \
    -init_debug \
    -setcookie bittorent \
    -run make all 'src/*'