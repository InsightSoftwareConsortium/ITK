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

#include "itkMaximumRatioDecisionRule.h"
#include "itkObjectFactory.h"

int itkMaximumRatioDecisionRuleTest(int, char* [] )
{
  typedef itk::Statistics::MaximumRatioDecisionRule      MaximumRatioDecisionRuleType;

  typedef MaximumRatioDecisionRuleType::MembershipVectorType MembershipVectorType;

  MaximumRatioDecisionRuleType::Pointer decisionRule = MaximumRatioDecisionRuleType::New();

  std::cout << decisionRule->GetNameOfClass() << std::endl;
  std::cout << decisionRule->MaximumRatioDecisionRuleType::Superclass::GetNameOfClass() << std::endl;

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

  //add discriminantscore with a value of zero
  double membershipScore4;
  membershipScore4 = 0.0;
  membershipScoreVector.push_back( membershipScore4 );


  unsigned int  decisionValue;
  try
    {
    decisionRule->Evaluate( membershipScoreVector);
    std::cerr << "An exception should have been thrown since a priori"
              << " probability is not set yet " << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception= " << excp << std::endl;
    }

  //Set aprior probablity
  typedef MaximumRatioDecisionRuleType::APrioriVectorType APrioriVectorType;
  typedef MaximumRatioDecisionRuleType::APrioriValueType  APrioriValueType;

  APrioriVectorType aprioriProbabilityVector;

  //first class
  APrioriValueType value1 = 0.4;
  aprioriProbabilityVector.push_back( value1 );

  //second class
  APrioriValueType value2 = 0.2;
  aprioriProbabilityVector.push_back( value2 );
  decisionRule->SetAPriori( aprioriProbabilityVector );

  //Evalue the membershipScore vector instantiated above ( 3 classes )
  try
    {
    decisionRule->Evaluate( membershipScoreVector);
    std::cerr << "An exception should have been thrown since the membership"
              << " score vector size doesn't match with the apriori vector" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception= " << excp << std::endl;
    }

  APrioriVectorType aprioriProbabilityVector2;

  value1 = 0.3;
  aprioriProbabilityVector2.push_back( value1 );

  value2 = 0.3;
  aprioriProbabilityVector2.push_back( value2 );

  //Add a third and a fourth class
  APrioriValueType value3 = 0.3;
  aprioriProbabilityVector2.push_back( value3 );

  //Zero priori probability
  APrioriValueType value4 = 0.0;
  aprioriProbabilityVector2.push_back( value4 );

  decisionRule->SetAPriori( aprioriProbabilityVector2 );
  try
    {
    decisionValue = decisionRule->Evaluate( membershipScoreVector);
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception= " << excp << std::endl;
    return EXIT_FAILURE;
    }

  //Check if the computed decision value is correct
  if( decisionValue != 2 )
    {
    std::cerr << "Decision rule computation is incorrect!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
