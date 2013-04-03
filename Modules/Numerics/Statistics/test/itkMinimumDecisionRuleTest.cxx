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

#include <iostream>

#include "itkMinimumDecisionRule.h"


int itkMinimumDecisionRuleTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing MinimumDecionRule " << std::endl << std::endl;

  typedef itk::Statistics::MinimumDecisionRule  DecisionRuleType;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  DecisionRuleType::MembershipVectorType discriminantScores;
  discriminantScores.resize( 3 );

  discriminantScores[0] = 0.0;
  discriminantScores[1] = 1.0;
  discriminantScores[2] = 2.0;

  if ( decisionRule->Evaluate( discriminantScores ) != 0 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  DecisionRuleType::MembershipVectorType discriminantScores2;
  discriminantScores2.resize( 3 );

  discriminantScores2[0] = 0.0;
  discriminantScores2[1] = 1.0;
  discriminantScores2[2] = 2.0;

  if ( decisionRule->Evaluate( discriminantScores2 ) != 0 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }


  DecisionRuleType::MembershipVectorType discriminantScores3;
  discriminantScores3.resize( 3 );

  discriminantScores3[0] = 0.0;
  discriminantScores3[1] = 1.0;
  discriminantScores3[2] = 2.0;

  if ( decisionRule->Evaluate( discriminantScores3 ) != 0 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[SUCCEEDED]" << std::endl;
  return EXIT_SUCCESS;
}
