#include "vnl/vnl_matlab_print.hxx"
#include "vnl/vnl_matrix_fixed.h"

using T=double;

template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,1,1> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,10,10> const&, char const*, vnl_matlab_print_format);
