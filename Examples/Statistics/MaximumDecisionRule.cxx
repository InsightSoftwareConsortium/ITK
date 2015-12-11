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

// Software Guide : BeginLatex
//
// \index{itk::Statistics::Maximum\-Decision\-Rule}
//
// The \doxygen{MaximumDecisionRule} returns the index of the largest
// discriminant score among the discriminant scores in the vector of
// discriminant scores that is the input argument of the \code{Evaluate()}
// method.
//
// To begin the example, we include the header files for the class and the
// MaximumDecisionRule. We also include the header file for the
// \code{std::vector} class that will be the container for the discriminant
// scores.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMaximumDecisionRule.h"
#include <vector>
// Software Guide : EndCodeSnippet

int main(int, char*[])
{
  // Software Guide : BeginLatex
  //
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::MaximumDecisionRule DecisionRuleType;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We create the discriminant score vector and fill it with three
  // values. The \code{Evaluate( discriminantScores )} will return 2
  // because the third value is the largest value.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  DecisionRuleType::MembershipVectorType discriminantScores;
  discriminantScores.push_back( 0.1 );
  discriminantScores.push_back( 0.3 );
  discriminantScores.push_back( 0.6 );

  std::cout << "MaximumDecisionRule: The index of the chosen = "
            << decisionRule->Evaluate( discriminantScores )
            << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
