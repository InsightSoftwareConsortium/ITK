#!/bin/bash

for ff in $( git grep -l "itkStaticConstMacro.*;" |fgrep -v ThirdParty |fgrep -v ITKv5Preparation |fgrep -v itkMacro.h); do
  cat $ff | \
  sed -e 's/itkStaticConstMacro *( *\([^ ,]*\)[ ,]*\([^,]*\)[ ,]*\([^)]*\)) *;/static constexpr \2 \1 = \3;/g' > /tmp/temp_file
  mv  /tmp/temp_file $ff
done

for ff in $( git grep -l "static *constexpr.*;" |fgrep -v ThirdParty |fgrep -v ITKv5Preparation |fgrep -v itkMacro.h); do
  cat $ff | \
  sed -e 's/static *constexpr *\(.*\)  *;/static constexpr \1;/g' > /tmp/temp_file
  mv  /tmp/temp_file $ff
done

cat > /tmp/cmt_msg << EOF
STYLE: Replace itkStaticConstMacro with static constexpr

Use static constexpr directly now that C++11 conformance
is required by all compilers.
EOF

cat /tmp/cmt_msg
