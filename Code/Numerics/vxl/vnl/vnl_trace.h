#ifndef vnl_trace_h_
#define vnl_trace_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME vnl_trace
// .HEADER vxl package
// .LIBRARY vnl
// .INCLUDE vnl/vnl_trace.h
// .FILE vnl_trace.cxx
// .SECTION Author
//  fsm@robots.ox.ac.uk
//

template <class T> class vnl_matrix;

template <class T>
T vnl_trace(vnl_matrix<T> const &);

#endif // vnl_trace_h_
