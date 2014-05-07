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

#include "itkDecisionRule.h"

namespace itk {
namespace Statistics {
namespace DecisionRuleTest {

class MyDecisionRule : public DecisionRule
{
public:
  /** Standard class typedef. */
  typedef MyDecisionRule                   Self;
  typedef DecisionRule                     Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Standard macros */
  itkTypeMacro(MyDecisionRule, DecisionRule);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Types for discriminant values and vectors. */
  typedef Superclass::MembershipValueType  MembershipValueType;
  typedef Superclass::MembershipVectorType MembershipVectorType;

  /** Types for class identifiers. */
  typedef Superclass::ClassIdentifierType ClassIdentifierType;

  /** Evaluate membership score */
  virtual ClassIdentifierType Evaluate(const MembershipVectorType &scoreVector) const ITK_OVERRIDE
    {
    double max = scoreVector[0];

    unsigned int maxIndex = 0;
    unsigned int i;
    for (i = 1; i < scoreVector.size(); i++)
      {
      if (scoreVector[i] > max)
        {
        max = scoreVector[i];
        maxIndex = i;
        }
      }
    return maxIndex;
    }
};

}
}
}
int itkDecisionRuleTest(int, char* [] )
{
  typedef itk::Statistics::DecisionRuleTest::MyDecisionRule DecisionRuleType;

  typedef DecisionRuleType::MembershipVectorType MembershipVectorType;

  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  std::cout << decisionRule->GetNameOfClass() << std::endl;
  std::cout << decisionRule->DecisionRuleType::Superclass::GetNameOfClass() << std::endl;

  decisionRule->Print(std::cout);

  MembershipVectorType membershipScoreVector;

  double membershipScore1;
  membershipScore1 = 0.1;
  membershipScoreVector.push_back( membershipScore1 );

  double membershipScore2;
  membershipScore2 = 0.5;
  membershipScoreVector.push_back( membershipScore2 );

  double membershipScore3;
  membershipScore3 = 1.9;
  membershipScoreVector.push_back( membershipScore3 );

  // the maximum score is the third component. The decision rule should
  // return index ( 2)
  if( decisionRule->Evaluate( membershipScoreVector ) != 2 )
    {
    std::cerr << "Decision rule computation is incorrect!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
