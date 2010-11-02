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
#include "vnl/vnl_diag_matrix.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_diag_matrix);
  typedef vcl_complex<double> double_complex;
  typedef vcl_complex<float> float_complex;
  namespace wrappers
  {
    typedef vnl_diag_matrix<double>         vnl_diag_matrix_double;
    typedef vnl_diag_matrix<double_complex> vnl_diag_matrix_double_complex;
    typedef vnl_diag_matrix<float>          vnl_diag_matrix_float;
    typedef vnl_diag_matrix<float_complex>  vnl_diag_matrix_float_complex;
    typedef vnl_diag_matrix<int>            vnl_diag_matrix_int;
    typedef vnl_diag_matrix<long double>    vnl_diag_matrix_long_double;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_diag_matrix_double);
  sizeof(vnl_diag_matrix_double_complex);
  sizeof(vnl_diag_matrix_float);
  sizeof(vnl_diag_matrix_float_complex);
  sizeof(vnl_diag_matrix_int);
  sizeof(vnl_diag_matrix_long_double);
}

#endif
