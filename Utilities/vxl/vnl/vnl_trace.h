// This is vxl/vnl/vnl_trace.h
#ifndef vnl_trace_h_
#define vnl_trace_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Calculate trace of a matrix
//  \author fsm
//
// \verbatim
// Modifications
// LSB (Manchester) 19/3/01 Documentation tidied
// \endverbatim

template <class T> class vnl_matrix;

//: Calculate trace of a matrix
// \relates vnl_matrix
template <class T>
T vnl_trace(vnl_matrix<T> const &);

#endif // vnl_trace_h_
