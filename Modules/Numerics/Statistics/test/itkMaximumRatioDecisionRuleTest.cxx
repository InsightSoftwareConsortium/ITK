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

#include "itkMaximumRatioDecisionRule.h"


int itkMaximumRatioDecisionRuleTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing MaximumRatioDecionRule " << std::endl << std::endl;

  typedef itk::Statistics::MaximumRatioDecisionRule  DecisionRuleType;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  DecisionRuleType::MembershipVectorType discriminantScores;
  discriminantScores.resize( 3 );

  discriminantScores[0] = 0.3;
  discriminantScores[1] = 0.5;
  discriminantScores[2] = 0.2;

  DecisionRuleType::PriorProbabilityVectorType aPrioris;
  aPrioris.resize(3);

  aPrioris[0] = 0.2;
  aPrioris[1] = 0.1;
  aPrioris[2] = 0.6;

  decisionRule->SetPriorProbabilities( aPrioris );

  if ( decisionRule->Evaluate( discriminantScores ) != 2 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  // run with uniform prior
  aPrioris.clear();
  decisionRule->SetPriorProbabilities( aPrioris );

  if ( decisionRule->Evaluate( discriminantScores ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[SUCCEEDED]" << std::endl;
  return EXIT_SUCCESS;
}
