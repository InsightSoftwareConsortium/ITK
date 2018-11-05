#!/bin/bash
# A script to assist with updating vnl_math_XXX function usage.
# \author Hans J. Johnson

git grep  -l "\<vnl_math_min\> *(" |grep -E "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/vnl_math_min_files.list
for ff in $(cat /tmp/vnl_math_min_files.list); do
  sed -i "" 's/vnl_math_min *(/std::min(/g' $ff
done

git grep  -l "\<vnl_math_max\> *(" |grep -E "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/vnl_math_max_files.list
for ff in $(cat /tmp/vnl_math_max_files.list); do
  sed -i "" 's/vnl_math_max *(/std::max(/g' $ff
done

git grep  -l "\<vnl_math_" |grep -E "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/vnl_math_XXX_files.list
for ff in $(cat /tmp/vnl_math_XXX_files.list); do
  sed -i "" 's/vnl_math_/vnl_math::/g' $ff
done

git grep -l "\<vnl_math" |grep -E "(.cxx|.cpp|.cc|.h|.hxx|.hpp|.txx)" > /tmp/vnl_math_namespace_files.list

if [[ "${DO_NOT_UPDATE_FOR_ITKV5}" != "TRUE" ]]; then
  for ff in $(cat /tmp/vnl_math_namespace_files.list); do
    # Alias the vnl_math functions in the itk::Math
    #* namespace. If possible, use the std:: equivalents
    for std_names in isnan isinf isfinite isnormal cbrt hypot; do
      sed -i "" "s/vnl_math::${std_names} *(/std::${std_names}(/g" $ff
    done
    # use ITK aliases instead of vnl_naming to prevent namespace polution
    # eventuallly we may want ot remove vnl from ITK
    for itkMath_names in angle_0_to_2pi angle_minuspi_to_pi rnd_halfinttoeven rnd_halfintup rnd floor ceil sgn sgn0 remainder_truncated remainder_floored abs sqr cube squared_magnitude; do
      sed -i "" "s/vnl_math::${itkMath_names} *(/itk::Math::${itkMath_names} (/g" $ff
    done
    sed -i "" "s/vnl_math::cuberoot *(/std::cbrt(/g" $ff
    for itkMath_constants in e log2e log10e ln2 ln10 pi twopi pi_over_2 pi_over_4 pi_over_180 one_over_pi two_over_pi deg_per_rad sqrt2pi two_over_sqrtpi one_over_sqrt2pi sqrt2 sqrt1_2 sqrt1_3 euler eps sqrteps float_eps float_sqrteps; do
      sed -i "" "s/vnl_math::${itkMath_constants}/itk::Math::${itkMath_constants}/g" $ff
    done
    sed -i "" "s#vnl/vnl_math.h#itkMath.h#g" $ff
  done
fi

echo "FILES CHANGED"
wc -l /tmp/vnl_math_min_files.list /tmp/vnl_math_max_files.list  /tmp/vnl_math_XXX_files.list /tmp/vnl_math_namespace_files.list

cat > /tmp/COMMIT_MESSAGE_vnl_math_XXX <<EOF
COMP: Future proof vnl_math_XXX function usage.

Prefer C++ over aliased names vnl_math_[min|max] -> std::[min|max]
Prefer vnl_math::abs over deprecated alias vnl_math_abs

In all compilers currently supported by VXL, vnl_math_[min|max]
could be replaced with std::[min|max] without loss of
functionality.  This also circumvents part of the backwards
compatibility requirements as vnl_math_ has been recently
replaced with a namespace of vnl_math::.

Since Wed Nov 14 07:42:48 2012:
The vnl_math_* functions use #define aliases to their
vnl_math::* counterparts in the "real" vnl_math:: namespace.

The new syntax should be backwards compatible to
VXL versions as old as 2012.

Prefer to use itk::Math:: over vnl_math:: namespace
EOF

echo "Reference commit messsage in 'git checkout -b replace-vnl_math_XX; git add -p ; git commit -F/tmp/COMMIT_MESSAGE_vnl_math_XXX'"
