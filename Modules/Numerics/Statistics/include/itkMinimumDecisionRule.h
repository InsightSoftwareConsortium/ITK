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
#ifndef __itkMinimumDecisionRule_h
#define __itkMinimumDecisionRule_h

#include "itkWin32Header.h"

#include <vector>
#include "vnl/vnl_matrix.h"
#include "itkDecisionRuleBase.h"

namespace itk
{
/** \class MinimumDecisionRule
 *  \brief A Decision rule that choose the class that has minimum value
 * \ingroup ITKStatistics
 */

class ITK_EXPORT MinimumDecisionRule:
  public DecisionRuleBase
{
public:
  /** Standard class typedefs */
  typedef MinimumDecisionRule       Self;
  typedef DecisionRuleBase          Superclass;
  typedef itk::SmartPointer< Self > Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MinimumDecisionRule, DecisionRuleBase);

  /** Standard New() method support */
  itkNewMacro(Self);

  /** Types for the arguments that are acceptable in the Evaluate() method */
  typedef Superclass::VectorType VectorType;
  typedef Superclass::ArrayType  ArrayType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const VectorType & discriminantScores) const;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const ArrayType & discriminantScores) const;

protected:
  MinimumDecisionRule();
  virtual ~MinimumDecisionRule() {}
}; // end of class
} // namespace itk

#endif
