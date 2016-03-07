#!/bin/env python
# \author Hans J. Johnson
#
# Prepare for the future by recommending
# use of itk::Math:: functions over
# vnl_math:: functions.
# Rather than converting vnl_math_ to vnl_math::
# this prefers to convert directly to itk::Math::
# namespace.  In cases where vnl_math:: is simply
# an alias to std:: functions, itk::Math directly
# uses the std:: version of the function.

import os
import sys
from collections import OrderedDict

## slight modification from grep command
info_for_conversion="""
XXXX,vnl_math_isnan,itk::Math::isnan
XXXX,vnl_math_isinf,itk::Math::isinf
XXXX,vnl_math_isfinite,itk::Math::isfinite
XXXX,vnl_math_isnormal,itk::Math::isnormal
XXXX,vnl_math_max,std::max
XXXX,vnl_math_min,std::min
XXXX,vnl_math_cuberoot,itk::Math::cbrt
XXXX,vnl_math_hypot,itk::Math::hypot
XXXX,vnl_math_angle_0_to_2pi,itk::Math::angle_0_to_2pi
XXXX,vnl_math_angle_minuspi_to_pi,itk::Math::angle_minuspi_to_pi
XXXX,vnl_math_rnd_halfinttoeven,itk::Math::halfinttoeven
XXXX,vnl_math_rnd_halfintup,itk::Math::rnd_halfintup
XXXX,vnl_math_rnd,itk::Math::rnd
XXXX,vnl_math_floor,itk::Math::floor
XXXX,vnl_math_ceil,itk::Math::ceil
XXXX,vnl_math_abs,itk::Math::abs
XXXX,vnl_math_sqr,itk::Math::sqr
XXXX,vnl_math_cube,itk::Math::cube
XXXX,vnl_math_sgn,itk::Math::sgn
XXXX,vnl_math_sgn0,itk::Math::sgn0
XXXX,vnl_math_squared_magnitude,itk::Math::squared_magnitude
XXXX,vnl_math_remainder_truncated,itk::Math::remainder_truncated
XXXX,vnl_math_remainder_floored,itk::Math::remainder_floored
"""

ITK_replace_head_names = OrderedDict()
ITK_replace_functionnames = OrderedDict()
ITK_replace_manual = OrderedDict()

ITK_replace_manual['"vnl/vnl_math.h"']='"itkMath.h"'
ITK_replace_manual['<vnl/vnl_math.h>']='<itkMath.h>'

for line in info_for_conversion.splitlines():
    linevalues = line.split(",")
    if len(linevalues) != 3:
        #print("SKIPPING: " + str(linevalues))
        continue
    fname=linevalues[0]
    new_name=fname.replace("ITK_","").replace(".h","")
    ITK_replace_head_names['#include "{0}"'.format(fname)]="""#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "{0}"
#endif
#include <{1}>""".format(fname,new_name)
    ITK_replace_head_names['#include <{0}>'.format(fname)]="""#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include <{0}>
#endif
#include <{1}>""".format(fname,new_name)
    ITK_pat=linevalues[1]
    new_pat=linevalues[2]
    ITK_replace_functionnames[ITK_pat]=new_pat
    # Need to fix the fact that both std::ios is a base and a prefix
    if "std::ios::" in new_pat:
        ITK_replace_manual[new_pat.replace("std::ios::","std::ios_")] = new_pat

#print(ITK_replace_head_names)
#print(ITK_replace_functionnames)

cfile=sys.argv[1]

file_as_string=""
with open(cfile,"r") as rfp:
    original_string=rfp.read()
file_as_string=original_string

required_header="" ## For ITK, this is always empty

for searchval,replaceval in ITK_replace_head_names.items():
   file_as_string_new = file_as_string.replace(searchval,required_header+replaceval)
   if file_as_string_new != file_as_string:
       required_header=""
   file_as_string=file_as_string_new


for searchval,replaceval in ITK_replace_functionnames.items():
   file_as_string = file_as_string.replace(searchval,replaceval)
for searchval,replaceval in ITK_replace_manual.items():
   file_as_string = file_as_string.replace(searchval,replaceval)
if file_as_string != original_string:
    print("Processing: {0}".format(cfile))
    with open(cfile,"w") as wfp:
	wfp.write(file_as_string)
else:
     print("SKIPPING: {0}".format(cfile))
