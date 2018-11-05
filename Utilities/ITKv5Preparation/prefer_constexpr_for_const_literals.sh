#!/bin/bash


for ff in $( git grep -l "\(\s*\)const  *\([^=]*\)= *\([0-9\.][0-9\.]*\) *;" |fgrep -v ThirdParty ); do
  cat $ff | \
  sed -e  's/^\( *\)const  *size_t  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr size_t \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *unsigned  *int  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr unsigned int \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *unsigned  *short  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr unsigned short \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *unsigned  *char  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr unsigned char \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *int  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr int \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *short  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr short \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *char  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr char \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *float  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr float \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *double  *\([^ ]*\) *= *\([0-9\.][0-9\.]*\) *;/\1constexpr double \2 = \3;/g' | \
  sed -e  's/^\( *\)const  *\([^=]*\)= *\([0-9\.][0-9\.]*\) *;/\1constexpr \2 = \3;/g' | \
  sed -e  's/constexpr *\([^)]*\)) *= *0 *;/const \1) = 0;/g' | \
  sed -e  's/constexpr *\([^)]*\)) *const *= *0 *;/const \1) const = 0;/g' | \
  sed -e  's/constexpr *\([^)]*\)) *const *override *= *0 *;/const \1) const override = 0;/g' | \
  sed -e  's/constexpr \(.*\)   *=/\1 =/g'  > /tmp/temp_file
  mv  /tmp/temp_file $ff
done

cat > /tmp/cmt_msg << EOF
STYLE: Prefer constexpr for const numeric literals

Use constexpr for constant numeric literals.
EOF

cat /tmp/cmt_msg
