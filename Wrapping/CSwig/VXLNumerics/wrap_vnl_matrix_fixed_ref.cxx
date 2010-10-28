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
#include "vnl/vnl_matrix_fixed_ref.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_matrix_fixed_ref);
  namespace wrappers
  {
    typedef vnl_matrix_fixed_ref<double,2,2>  vnl_matrix_fixed_ref_double_2_2;
    typedef vnl_matrix_fixed_ref<double,2,3>  vnl_matrix_fixed_ref_double_2_3;
    typedef vnl_matrix_fixed_ref<double,3,12> vnl_matrix_fixed_ref_double_3_12;
    typedef vnl_matrix_fixed_ref<double,3,3>  vnl_matrix_fixed_ref_double_3_3;
    typedef vnl_matrix_fixed_ref<double,3,4>  vnl_matrix_fixed_ref_double_3_4;
    typedef vnl_matrix_fixed_ref<double,4,3>  vnl_matrix_fixed_ref_double_4_3;
    typedef vnl_matrix_fixed_ref<double,4,4>  vnl_matrix_fixed_ref_double_4_4;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_matrix_fixed_ref_double_2_2);
  sizeof(vnl_matrix_fixed_ref_double_2_3);
  sizeof(vnl_matrix_fixed_ref_double_3_12);
  sizeof(vnl_matrix_fixed_ref_double_3_3);
  sizeof(vnl_matrix_fixed_ref_double_3_4);
  sizeof(vnl_matrix_fixed_ref_double_4_3);
  sizeof(vnl_matrix_fixed_ref_double_4_4);
}

#endif
