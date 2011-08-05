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

#include "itkMaximumDecisionRule.h"
#include "itkObjectFactory.h"

int itkMaximumDecisionRuleTest(int, char* [] )
{
  typedef itk::Statistics::MaximumDecisionRule      MaximumDecisionRuleType;

  typedef MaximumDecisionRuleType::MembershipVectorType MembershipVectorType;

  MaximumDecisionRuleType::Pointer decisionRule = MaximumDecisionRuleType::New();

  std::cout << decisionRule->GetNameOfClass() << std::endl;
  std::cout << decisionRule->MaximumDecisionRuleType::Superclass::GetNameOfClass() << std::endl;

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
