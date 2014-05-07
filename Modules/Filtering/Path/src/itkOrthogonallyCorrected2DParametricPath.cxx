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
#include "itkOrthogonallyCorrected2DParametricPath.h"

namespace itk
{
OrthogonallyCorrected2DParametricPath::OutputType
OrthogonallyCorrected2DParametricPath
::Evaluate(const InputType & inputValue) const
{
  InputType input = inputValue;                         // we may want to remap
                                                        // the input
  InputType                         inputRange;
  InputType                         normalizedInput;
  OutputType                        output;
  OrthogonalCorrectionTableSizeType numOrthogonalCorrections;
  double                            softOrthogonalCorrectionTableIndex;
  double                            Correction, Correction1, Correction2;
  VectorType                        originalDerivative;

  numOrthogonalCorrections = m_OrthogonalCorrectionTable->Size();

  // If the original path is closed, then tail input is remapped to head input
  if ( m_OriginalPath->EvaluateToIndex( m_OriginalPath->EndOfInput() )  ==
       m_OriginalPath->EvaluateToIndex( m_OriginalPath->StartOfInput() ) )
    {
    if ( input >= m_OriginalPath->EndOfInput() )
      {
      // use the starting input value instead of the ending input value
      input = m_OriginalPath->StartOfInput();
      }
    }

  inputRange = m_OriginalPath->EndOfInput() - m_OriginalPath->StartOfInput();
  normalizedInput = ( input - m_OriginalPath->StartOfInput() ) / inputRange;
  output.Fill(0);

  // Find the linearly interpolated offset error value for this exact time.
  softOrthogonalCorrectionTableIndex = normalizedInput * numOrthogonalCorrections;
  Correction1 = m_OrthogonalCorrectionTable->ElementAt(
    int(softOrthogonalCorrectionTableIndex) );
  Correction2 = m_OrthogonalCorrectionTable->ElementAt(
    int(softOrthogonalCorrectionTableIndex + 1) % numOrthogonalCorrections);
  Correction = Correction1 + ( Correction2 - Correction1 )
               * ( softOrthogonalCorrectionTableIndex
                   - int(softOrthogonalCorrectionTableIndex) );

  // Find the direction of the offset
  originalDerivative = m_OriginalPath->EvaluateDerivative(input);
  originalDerivative.Normalize();

  // Find the actual point along this corrected path
  output = m_OriginalPath->Evaluate(input);
  output[0] -= Correction * originalDerivative[1];
  output[1] += Correction * originalDerivative[0];
  return output;
}

void
OrthogonallyCorrected2DParametricPath
::SetOriginalPath(const OriginalPathType *originalPath)
{
  itkDebugMacro("setting OriginalPath to " << originalPath);
  if ( this->m_OriginalPath != originalPath )
    {
    this->m_OriginalPath = originalPath;
    // This is the important line that is not in itkSetObjectMacro
    this->m_DefaultInputStepSize = m_OriginalPath->GetDefaultInputStepSize();
    this->Modified();
    }
}

/**
 * Constructor
 */
OrthogonallyCorrected2DParametricPath
::OrthogonallyCorrected2DParametricPath()
{
  m_OriginalPath = ITK_NULLPTR;
  m_OrthogonalCorrectionTable = OrthogonalCorrectionTableType::New();
}

/**
 * Standard "PrintSelf" method
 */
void
OrthogonallyCorrected2DParametricPath
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Original Path:  " << m_OriginalPath << std::endl;
  os << indent << "Correction Table:  " << m_OrthogonalCorrectionTable << std::endl;
}
} // end namespaceitk
