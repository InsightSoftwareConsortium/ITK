// This is vxl/vnl/vnl_vector_ref.txx
#ifndef vnl_vector_ref_txx_
#define vnl_vector_ref_txx_

/*
  fsm
*/
#include "vnl_vector_ref.h"

//--------------------------------------------------------------------------------

#undef VNL_VECTOR_REF_INSTANTIATE
#define VNL_VECTOR_REF_INSTANTIATE(T) \
template class vnl_vector_ref<T >

#endif // vnl_vector_ref_txx_
