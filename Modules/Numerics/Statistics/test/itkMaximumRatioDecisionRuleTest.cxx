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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include <vector>

#include "itkMaximumRatioDecisionRule.h"


int itkMaximumRatioDecisionRuleTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing MaximumRatioDecionRule " << std::endl << std::endl;

  typedef itk::MaximumRatioDecisionRule  DecisionRuleType;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  std::vector< double > discriminantScores;
  discriminantScores.resize( 3 );

  discriminantScores[0] = 0.3;
  discriminantScores[1] = 0.5;
  discriminantScores[2] = 0.2;

  DecisionRuleType::APrioriVectorType aPrioris;
  aPrioris.resize(3);

  aPrioris[0] = 0.2;
  aPrioris[1] = 0.5;
  aPrioris[2] = 0.3;

  decisionRule->SetAPriori( aPrioris );

  if ( decisionRule->Evaluate( discriminantScores ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  DecisionRuleType::VectorType discriminantScores2;
  discriminantScores2.resize( 3 );

  discriminantScores2[0] = 0.3;
  discriminantScores2[1] = 0.5;
  discriminantScores2[2] = 0.2;

  if ( decisionRule->Evaluate( discriminantScores2 ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  DecisionRuleType::ArrayType discriminantScores3(3);

  discriminantScores3[0] = 0.3;
  discriminantScores3[1] = 0.5;
  discriminantScores3[2] = 0.2;

  if ( decisionRule->Evaluate( discriminantScores3 ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[SUCCEEDED]" << std::endl;
  return EXIT_SUCCESS;
}
