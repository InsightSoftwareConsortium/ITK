#ifndef vnl_resize_h_
#define vnl_resize_h_
// .NAME vnl_resize
// .HEADER vxl package
// .LIBRARY vnl
// .INCLUDE vnl/vnl_resize.h
// .FILE vnl_resize.txx
// @author fsm@robots.ox.ac.uk

template <class T> class vnl_vector;
template <class T> class vnl_matrix;
template <class T> class vnl_diag_matrix;

template <class T> void vnl_resize(vnl_vector<T> &v, unsigned newsize);
template <class T> void vnl_resize(vnl_matrix<T> &M, unsigned newrows, unsigned newcols);
template <class T> void vnl_resize(vnl_diag_matrix<T> &D, unsigned newsize);

#endif // vnl_resize_h_
