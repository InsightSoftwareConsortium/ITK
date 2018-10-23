#include "vnl/vnl_matlab_print.hxx"
#include "vnl/vnl_matrix_fixed.h"

using T=float;

template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,1,1> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,5,5> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,6,6> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,7,7> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,8,8> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,9,9> const&, char const*, vnl_matlab_print_format);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<T,10,10> const&, char const*, vnl_matlab_print_format);
