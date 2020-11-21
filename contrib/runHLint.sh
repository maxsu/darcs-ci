#!/bin/bash

hlint --hint=contrib/darcs-errors.hlint --cpp-simple  ./src
hlint --cpp-simple  ./src

# --cpp-simple is to avoid issues with MIN_VERSION
#   see https://github.com/ndmitchell/hlint/issues/53
# using it creates other problems since #if are ignored..

# When the above bug is solved we can use:
# hlint --hint=contrib/darcs-errors.hlint --cpp-include=./src ./src
# hlint --cpp-include=./src ./src
