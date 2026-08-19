/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkBridgeMathSVD.h"

// Define once here the Eigen JacobiSVD/BDCSVD specializations that itkBridgeMathSVD.h
// declares extern, so consumer translation units link instead of paying the
// per-TU Eigen instantiation cost.
namespace itk::bridge
{
namespace Math
{
namespace detail
{
ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

#define ITK_MATH_SVD_INST_FIXED(D)                                                                   \
  template ITKCommon_EXPORT void SquareSVDEigen<D, float>(const float *, float *, float *, float *); \
  template ITKCommon_EXPORT void SquareSVDEigen<D, double>(const double *, double *, double *, double *);
ITK_MATH_SVD_FIXED_DIMS(ITK_MATH_SVD_INST_FIXED)
#undef ITK_MATH_SVD_INST_FIXED

template ITKCommon_EXPORT void
DynamicSquareSVDEigen<float>(const float *, unsigned int, float *, float *, float *);
template ITKCommon_EXPORT void
DynamicSquareSVDEigen<double>(const double *, unsigned int, double *, double *, double *);
template ITKCommon_EXPORT void
RectangularSVDEigen<float>(const float *, unsigned int, unsigned int, float *, float *, float *);
template ITKCommon_EXPORT void
RectangularSVDEigen<double>(const double *, unsigned int, unsigned int, double *, double *, double *);

ITK_GCC_PRAGMA_DIAG_POP()
} // namespace detail
} // namespace Math
} // namespace itk::bridge
