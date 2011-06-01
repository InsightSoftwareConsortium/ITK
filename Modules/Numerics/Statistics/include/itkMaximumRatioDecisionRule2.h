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
#ifndef __itkMaximumRatioDecisionRule2_h
#define __itkMaximumRatioDecisionRule2_h

#include <vector>
#include "vnl/vnl_matrix.h"

#include "itkNumericTraits.h"
#include "itkDecisionRule.h"

namespace itk
{
namespace Statistics
{
/** \class MaximumRatioDecisionRule2
 *  \brief This rule returns  \f$i\f$ if
 *   \f$\frac{f_{i}(\overrightarrow{x})}{f_{j}(\overrightarrow{x})} >
 *   \frac{K_{j}}{K_{i}}\f$ for all \f$j \not= i\f$,
 * where the \f$i\f$ is the index of a class which has
 * membership function \f$f_{i}\f$ and its prior value
 * (usually, the a priori probability or the size of a class) is
 * \f$K_{i}\f$
 *
 * Users should set the a priori values before calling the Evaluate method.
 *
 * \sa MaximumDecisionRule, MinimumDecisionRule
 * \ingroup ITK-Statistics
 */

class ITK_EXPORT MaximumRatioDecisionRule2:
  public DecisionRule
{
public:
  /** Standard class typedefs */
  typedef MaximumRatioDecisionRule2 Self;
  typedef DecisionRule              Superclass;
  typedef SmartPointer< Self >      Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumRatioDecisionRule2, DecisionRule);

  /** Standard New() method support */
  itkNewMacro(Self);

  typedef float                           APrioriValueType;
  typedef std::vector< APrioriValueType > APrioriVectorType;
  typedef APrioriVectorType::size_type    APrioriVectorSizeType;

  typedef Superclass::MembershipVectorType MembershipVectorType;

  /** The return value of this function is a class label.
   * Basically, using its internal logic based on the discriminant
   * scores, this function decides best class label and return it.
   */
  virtual unsigned int Evaluate(const MembershipVectorType & discriminantScores) const;

  /** Sets the a priori probabilities */
  void SetAPriori(APrioriVectorType & values);

protected:
  MaximumRatioDecisionRule2();
  virtual ~MaximumRatioDecisionRule2() {}
private:
  MaximumRatioDecisionRule2(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented

  /** Number of classes */
  APrioriVectorSizeType m_NumberOfClasses;

  /** a priori probability ratio matrix: internal use */
  vnl_matrix< double > m_APrioriRatioMatrix;
};  // end of class
} // end of Statistics namespace
} // end of ITK namespace
#endif
