// This is vxl/vnl/vnl_vector_fixed_ref.txx
#ifndef vnl_vector_fixed_ref_txx_
#define vnl_vector_fixed_ref_txx_
// Author: Paul P. Smyth, Vicon Motion Systems Ltd. 
// Created: 02 May 2001
//
#include "vnl_vector_fixed_ref.h"
#include <vcl_cassert.h>

template<class T, unsigned n>
vnl_vector<T>& vnl_vector_fixed_ref<T,n>::pre_multiply (vnl_matrix<T> const& ) {
    assert(!"cannot use pre_multiply on vnl_vector_fixed<T,n>, since it deallocates the data storage");
    return *this;
}

template<class T, unsigned n>
vnl_vector<T>& vnl_vector_fixed_ref<T,n>::post_multiply (vnl_matrix<T> const& ) {
    assert(!"cannot use post_multiply on vnl_vector_fixed<T,n>, since it deallocates the data storage");
    return *this;
}

template<class T, unsigned n>
vnl_vector<T>& vnl_vector_fixed_ref<T,n>::operator*= (vnl_matrix<T> const& ) {
    assert(!"cannot use pre_multiply on vnl_vector_fixed_ref<T,n>, since it deallocates the data storage");
    return *this;
}

//------------------------------------------------------------

// instantiation macros for vnl_vector_fixed_ref<T,unsigned> :

#define VNL_VECTOR_FIXED_REF_INSTANTIATE(T,n) \
template class vnl_vector_fixed_ref<T, n >; \
VCL_INSTANTIATE_INLINE(vcl_ostream &operator<<(vcl_ostream & VCL_COMMA vnl_vector_fixed_ref<T VCL_COMMA n > const &))

#endif // vnl_vector_fixed_ref_txx_
