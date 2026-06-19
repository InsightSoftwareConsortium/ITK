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
#ifndef itkEigenDecompositionSolverInfo_h
#define itkEigenDecompositionSolverInfo_h

#include "itk_eigen.h"
#include ITK_EIGEN(Dense)

namespace itk::detail
{

/** Human-readable name and likely cause for an Eigen solver status, so a
 * thrown decomposition error names the failure mode instead of a raw code. */
inline const char *
EigenComputationInfoString(Eigen::ComputationInfo info)
{
  switch (info)
  {
    case Eigen::Success:
      return "Success";
    case Eigen::NumericalIssue:
      return "NumericalIssue (a precondition is violated, e.g. the matrix is not finite or not positive-definite)";
    case Eigen::NoConvergence:
      return "NoConvergence (the iteration did not converge)";
    case Eigen::InvalidInput:
      return "InvalidInput (the input is malformed or the options are invalid)";
  }
  return "an unrecognized Eigen::ComputationInfo value";
}

} // namespace itk::detail

#endif // itkEigenDecompositionSolverInfo_h
