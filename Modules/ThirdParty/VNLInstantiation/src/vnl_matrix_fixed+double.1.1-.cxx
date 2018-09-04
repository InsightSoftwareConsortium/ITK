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
#if VXL_VERSION_DATE_FULL < 20160229
#include "vnl/vnl_matrix_fixed.txx"
#include "vnl/vnl_matlab_print.txx"
#else
#include "vnl/vnl_matrix_fixed.hxx"
#include "vnl/vnl_matlab_print.hxx"
#endif
#include "vnl_export_override.h"
VNL_MATRIX_FIXED_INSTANTIATE(double, 1, 1);
template VNL_EXPORT std::ostream &vnl_matlab_print(std::ostream &, vnl_matrix_fixed<double,1,1> const&, char const*, vnl_matlab_print_format);
