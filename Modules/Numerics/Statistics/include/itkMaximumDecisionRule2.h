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
#ifndef __itkMaximumDecisionRule2_h
#define __itkMaximumDecisionRule2_h

#include "itkDecisionRule.h"

namespace itk
{
namespace Statistics
{
/** \class MaximumDecisionRule2
 *  \brief A Decision rule that choose the class of which discriminant
 *  score is the largest. This class will replace the MaximumDecisionRule
 *  in Code/Common
 * \ingroup ITK-Statistics
 */

class ITK_EXPORT MaximumDecisionRule2:public DecisionRule
{
public:
  /** Standard class typedefs */
  typedef MaximumDecisionRule2       Self;
  typedef DecisionRule               Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumDecisionRule2, DecisionRule);

  /** Standard New() method support */
  itkNewMacro(Self);

  typedef Superclass::MembershipVectorType MembershipVectorType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const MembershipVectorType & discriminantScores) const;

protected:
  MaximumDecisionRule2() {}
  virtual ~MaximumDecisionRule2() {}
  void PrintSelf(std::ostream & os, Indent indent) const;
};  // end of class
} // end of namespace itk
} // end of namespace Statistics

#endif
