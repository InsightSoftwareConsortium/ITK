#include <vcl_functional.h>
#include <vcl_set.txx>

VCL_SET_INSTANTIATE(unsigned, vcl_less<unsigned> );

#if defined(VCL_GCC_27)
typedef vcl_set<unsigned, vcl_less<unsigned> >::reverse_iterator I;
//rBdIt<vcl_rbTcIt<unsigned int>, unsigned int, unsigned int const &, int> I;
VCL_INSTANTIATE_INLINE(bool operator!=(I const &, I const &));
#endif
