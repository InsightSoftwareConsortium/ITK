#ifndef vnl_trace_h_
#define vnl_trace_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_trace.h

//: 
//  \file
//  \brief Calculate trace of a matrix
//  \author fsm@robots.ox.ac.uk

// Modifications
// LSB (Manchester) 19/3/01 Documentation tidied

template <class T> class vnl_matrix;

//: Calculate trace of a matrix
template <class T>
T vnl_trace(vnl_matrix<T> const &);

#endif // vnl_trace_h_
