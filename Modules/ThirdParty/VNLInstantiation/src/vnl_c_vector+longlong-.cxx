/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL < 20120316
#include <vnl/vnl_c_vector.txx>

#include <vnl/vnl_complex_traits.h>
// The following macro is a complement to the ones
// in vxl/core/vnl/vnl_complex_traits.h lines 34-49.
#define VCL_DEFINE_SPECIALIZATION_MACRO(T)                                        \
  VCL_DEFINE_SPECIALIZATION struct vnl_complex_traits< T > {                      \
    enum { isreal = true };                                                       \
    static T conjugate(T x) { return x; }                                         \
    static vcl_complex< T > complexify(T x) { return vcl_complex< T >(x, (T)0); } \
  }
// end of macro
VCL_DEFINE_SPECIALIZATION_MACRO(long long);
#undef VCL_DEFINE_SPECIALIZATION_MACRO

VNL_C_VECTOR_INSTANTIATE_ordered(long long);
#else
void vnl_c_vector_longlong_dummy(void) {}
#endif
