#include <vcl_list.txx>
#include <vnl/vnl_double_4.h>

#ifdef VCL_SGI_CC_730
#pragma do_not_instantiate void  std::list<vnl_double_4>::merge(std::list<vnl_double_4> &)
#endif

VCL_DO_NOT_INSTANTIATE(void vcl_list<vnl_double_4>::merge(vcl_list<vnl_double_4> &), VCL_VOID_RETURN);

//template class std::list<vnl_double_4>;
VCL_LIST_INSTANTIATE(vnl_double_4);
