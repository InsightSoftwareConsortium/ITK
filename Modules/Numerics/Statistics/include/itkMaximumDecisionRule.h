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
#ifndef __itkMaximumDecisionRule_h
#define __itkMaximumDecisionRule_h

#include "itkWin32Header.h"

#include "itkDecisionRuleBase.h"

namespace itk
{
/** \class MaximumDecisionRule
 *  \brief A Decision rule that choose the class of which discriminant
 *  score is the largest.
 * \ingroup ITKStatistics
 */

class ITK_EXPORT MaximumDecisionRule:
  public DecisionRuleBase
{
public:
  /** Standard class typedefs */
  typedef MaximumDecisionRule        Self;
  typedef DecisionRuleBase           Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumDecisionRule, DecisionRuleBase);

  /** Standard New() method support */
  itkNewMacro(Self);

  /** Types for the arguments that are acceptable in the Evaluate() method */
  typedef Superclass::VectorType               VectorType;
  typedef Superclass::ArrayType                ArrayType;
  typedef Superclass::VariableLengthVectorType VariableLengthVectorType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const VectorType & discriminantScores) const;

  virtual unsigned int Evaluate(const ArrayType & discriminantScores) const;

  virtual unsigned int Evaluate(const VariableLengthVectorType & discriminantScores) const;

protected:
  MaximumDecisionRule() {}
  virtual ~MaximumDecisionRule() {}
  void PrintSelf(std::ostream & os, Indent indent) const;
}; // end of class
} // end of namespace itk

#endif
