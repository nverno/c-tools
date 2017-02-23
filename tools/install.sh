#!/usr/bin/env bash

. ~/bin/utils.sh

udpated=0
update() {
    if [ -z $updated ]; then
        sudo apt-get -qq update
        updated=1
    fi
}

progs=(clang cmake valgrind boost CUnit check doxygen glibc-doc ncurses-doc)
for prog in "${progs[@]}"; do
    if ! hash $prog 2>/dev/null ; then
        update
        case "$prog" in
            clang) 
                sudo apt-get install -y clang libclang-dev ;;
            boost)
                if ! ls /usr/include | grep boost 2>&1 >/dev/null ; then
                    _log "Installing boost"
                    sudo apt-get install -y libboost-all-dev
                fi;;
            CUnit)
                if ! ls /usr/include | grep CUnit 2>&1 >/dev/null ; then
                    _log "Installing CUnit\n"
                    update
                    sudo apt-get install -y libcunit1 libcunit1-doc libcunit1-dev
                fi;;
            *)
                sudo apt-get install -y $prog ;;
        esac
    fi
done

if [ ! -d ~/.local/include/unity ]; then
    git clone --depth=1 https://github.com/ThrowTheSwitch/Unity \
        ~/.local/include/unity
fi
