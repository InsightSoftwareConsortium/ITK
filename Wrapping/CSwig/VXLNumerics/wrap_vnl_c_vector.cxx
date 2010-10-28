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
#include "vcl_complex.h"
#include "vnl/vnl_c_vector.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"
ITK_WRAP_VNL(vnl_c_vector);

#if 0
// Could add vnl_c_vector_bool, but it is disabled for ITK's
// VXLNumerics library.
namespace _cable_
{
  namespace wrappers
  {
    typedef vnl_c_vector<bool> vnl_c_vector_bool;
  }
}

void force_instantiate2()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_c_vector_bool);
}
#endif

#endif
