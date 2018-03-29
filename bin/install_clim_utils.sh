#!/bin/bash
Where=$(readlink -e ${0})
Where=${Where%/bin/*}/src
cd ${Where}
echo "Installing from ${Where}"
[[ "$BASE_ARCH" == "$EC_ARCH" ]] && echo "ERROR: EC_ARCH == BASE_ARCH" && exit 1
