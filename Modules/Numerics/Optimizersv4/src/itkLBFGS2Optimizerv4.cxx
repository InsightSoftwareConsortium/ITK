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
#include "itkLBFGS2Optimizerv4.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const LBFGS2Optimizerv4Enums::LineSearchMethod value)
{
  return out << [value] {
    switch (value)
    {
      case LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_DEFAULT:
        return "itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_DEFAULT";
        //                case LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_MORETHUENTE:
        //                    return "itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_MORETHUENTE";
      case LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_ARMIJO:
        return "itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_ARMIJO";
      case LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING:
        return "itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING";
        //                case LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_WOLFE:
        //                    return "itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_WOLFE";
      case LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_STRONG_WOLFE:
        return "itk::LBFGS2Optimizerv4Enums::LineSearchMethod::LINESEARCH_BACKTRACKING_STRONG_WOLFE";
      default:
        return "INVALID VALUE FOR itk::LBFGS2Optimizerv4Enums::LineSearchMethod";
    }
  }();
}

} // end namespace itk
