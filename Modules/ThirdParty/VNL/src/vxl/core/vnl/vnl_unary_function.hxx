// This is core/vnl/vnl_unary_function.hxx
#ifndef vnl_unary_function_hxx_
#define vnl_unary_function_hxx_
//:
// \file
// \brief Abstract 1D map
//  vnl_unary_function is an abstract map between two types (read spaces).
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   28 Nov 1998
//
// \verbatim
//  Modifications
//   981128 AWF Initial version.
//   LSB Manchester 19/Mar/2001 Documentation tidied
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Oct.2003 - Ian Scott - Move the use of vcl_limits out of the .h file, to
//                          solve stupid MSVC6.0 problems.
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <limits>
#include "vnl_unary_function.h"
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

//: Return bounding cube of range (outputs)
template <class RETURN, class ARGUMENT>
RETURN vnl_unary_function<RETURN, ARGUMENT>::get_range_min() const
{
  return std::numeric_limits<RETURN>::min();
}

//: Return bounding cube of range (outputs)
template <class RETURN, class ARGUMENT>
RETURN vnl_unary_function<RETURN, ARGUMENT>::get_range_max() const
{
  return std::numeric_limits<RETURN>::max();
}

#define VNL_UNARY_FUNCTION_INSTANTIATE(S,T) \
template class VNL_EXPORT vnl_unary_function<S, T >

#endif // vnl_unary_function_hxx_
