#!/usr/bin/env bash

udpated=0
update() {
    if [ -z $updated ]; then
       sudo apt-get -qq update
       updated=1
    fi
}

progs=(clang cmake)
for prog in "${progs[@]}"; do
    if ! hash $prog 2>/dev/null ; then
        update
        case "$prog" in
          clang) 
              printf "Installing clang\n"
              sudo apt-get install -y clang libclang-dev ;;
          cmake)
              printf "Installng cmake\n"
              sudo apt-get install -y cmake ;;
          *) echo $"Unknown program"
             exit 1
        esac
    fi
done

# install boost / CUnit
if ! ls /usr/include | grep boost 2>&1 >/dev/null ; then
    printf "Installing boost\n"
    update
    sudo apt-get install -y libboost-all-dev
fi
if ! ls /usr/include | grep CUnit 2>&1 >/dev/null ; then
    printf "Installing CUnit\n"
    update
    sudo apt-get install -y libcunit1 libcunit1-doc libcunit1-dev
fi
