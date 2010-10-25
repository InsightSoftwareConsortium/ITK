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
#include "vnl/vnl_fortran_copy.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_fortran_copy);
  typedef vcl_complex<double> double_complex;
  typedef vcl_complex<float> float_complex;
  namespace wrappers
  {
    typedef vnl_fortran_copy<double>         vnl_fortran_copy_double;
    typedef vnl_fortran_copy<double_complex> vnl_fortran_copy_double_complex;
    typedef vnl_fortran_copy<float>          vnl_fortran_copy_float;
    typedef vnl_fortran_copy<float_complex>  vnl_fortran_copy_float_complex;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_fortran_copy_double);
  sizeof(vnl_fortran_copy_double_complex);
  sizeof(vnl_fortran_copy_float);
  sizeof(vnl_fortran_copy_float_complex);
}

#endif
