// This is core/vnl/algo/vnl_bracket_minimum.h
#ifndef vnl_bracket_minimum_h_
#define vnl_bracket_minimum_h_
//:
// \file
// \brief Function to bracket a minimum
// \author Tim Cootes
// \date   Feb 2007
//
// \verbatim
//  Modifications
// \endverbatim

#include <vnl/vnl_cost_function.h>
#include <vnl/algo/vnl_algo_export.h>

//: Given initial values a and b, find bracket a<b<c s.t. f(a)>f(b)<f(c)
//  Final function values at a,b,c stored in fa,fb,fc.
//
//  The algorithm takes increasingly large steps in a downhill direction
//  until it starts going up again.  To speed things up, it also fits
//  a parabola to the last three points, which it uses to predict the
//  possible minimum directly ( hopefully automatically choosing a
//  sensible step size).
//
//  Note that there's currently nothing
//  to stop it if it is supplied with a monotonic function - it will just continue
//  forever.
void VNL_ALGO_EXPORT vnl_bracket_minimum(vnl_cost_function& f,
                         double& a, double& b, double& c,
                         double& fa, double& fb, double& fc);

#endif // vnl_bracket_minimum_h_
