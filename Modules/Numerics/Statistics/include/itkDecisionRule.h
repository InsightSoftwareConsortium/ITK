/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkDecisionRule_h
#define itkDecisionRule_h

#include <vector>
#include "vnl/vnl_matrix.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/**
 *\class DecisionRule
 *  \brief Base class for decision rules that return a class label
 *  based on a set of discriminant scores.
 *
 *  DecisionRule is the base class for decision rules used in
 *  classification. Subclasses of DecisionRule include
 *  MaximumDecisionRule, MinimumDecisionRule, and
 *  MaximumRationDecisionRule, This class has a pure virtual function,
 *  Evaluate(), which implements the particular decision rule, given a
 *  set of discriminant scores. Concrete subclasses of
 *  DecisionRule must implement this method.
 *
 *  \ingroup ITKStatistics
 */

class ITKStatistics_EXPORT DecisionRule : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DecisionRule);

  /** Standard class type aliases */
  using Self = DecisionRule;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(DecisionRule, Object);

  /** Types for discriminant values and vectors. */
  using MembershipValueType = double;
  using MembershipVectorType = std::vector<MembershipValueType>;

  /** Types for class identifiers. */
  using ClassIdentifierType = MembershipVectorType::size_type;

  /**
   * Evaluate the decision rule. The return value of this function is
   * a class label.  Functions returns the best label given the
   * discriminant scores using its internal logic.
   */
  virtual ClassIdentifierType
  Evaluate(const MembershipVectorType & discriminantScores) const = 0;

protected:
  DecisionRule();
  ~DecisionRule() override;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif
