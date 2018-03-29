#!/bin/bash
Where=$(readlink -e ${0})
Where=${Where%/bin/*}/src
cd ${Where}
echo "Installing from ${Where}"
[[ "$BASE_ARCH" == "$EC_ARCH" ]] && echo "ERROR: EC_ARCH == BASE_ARCH" && exit 1
set -x
for Target in *.F90 ; do
  s.f90 -I. -O 2 -o ${Target%.F90}.Abs ${Target} -lrmn
  mv ${Target%.F90}.Abs ../bin
  rm -f *.mod
done
