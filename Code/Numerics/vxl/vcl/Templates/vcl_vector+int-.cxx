#include <vcl_vector.txx>

VCL_VECTOR_INSTANTIATE(int);

#ifdef GNU_LIBSTDCXX_V3
#include <vcl_algorithm.txx>
VCL_FIND_INSTANTIATE(vcl_vector<int>::iterator, int);
VCL_FIND_INSTANTIATE(vcl_vector<int>::iterator, unsigned);
namespace std {
  template int * std::fill_n<int *, int, int>(int *, int, int const &);
}
#endif
