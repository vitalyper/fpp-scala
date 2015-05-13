#!/bin/bash -e

# Check for scala and sbt
function check_dependencies {
    local deps=(scala sbt)
    local deps_names=('Scala' 'Scala build tool')
    local deps_count=${#deps[*]}

    for (( i=0; i<=$(( $deps_count -1 ));  i++ ))
    do
        d=${deps[$i]}
        dn=${deps_names[$i]}
        hash $d > /dev/null 2>&1  || (echo "$d ($dn) is not installed"; return 1;)
    done
}

check_dependencies

for d in $(find . -type d -maxdepth 1 | grep '\/') ; do
    pushd $d > /dev/null
    echo "*** Running all tests in dir <$d> ***"
    sbt clean test
    popd > /dev/null
    echo -e "\n"
done
