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
// \index{itk::Statistics::Maximum\-Ratio\-Decision\-Rule}
//
// MaximumRatioDecisionRule returns the class label using a Bayesian
// style decision rule. The discriminant scores are evaluated in the
// context of class priors. If the discriminant scores are actual
// conditional probabilites (likelihoods) and the class priors are
// actual a priori class probabilities, then this decision rule operates
// as Bayes rule, returning the class $i$ if
// \begin{equation}
//     p(x|i) p(i) > p(x|j) p(j)
// \end{equation}
// for all class $j$. The discriminant scores and priors are not
// required to be true probabilities.
//
// This class is named the MaximumRatioDecisionRule as it can be
// implemented as returning the class $i$ if
// \begin{equation}
//     \frac{p(x|i)}{p(x|j)} > \frac{p(j)}{p(i)}
// \end{equation}
// for all class $j$.
//
// We include the header files for the class as well as the header file for
// the \code{std::vector} class that will be the container for the
// discriminant scores.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMaximumRatioDecisionRule.h"
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
  typedef itk::Statistics::MaximumRatioDecisionRule DecisionRuleType;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We create the discriminant score vector and fill it with three
  // values. We also create a vector (\code{aPrioris}) for the \emph{a
  // priori} values. The \code{Evaluate( discriminantScores )} will
  // return 1.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  DecisionRuleType::MembershipVectorType discriminantScores;
  discriminantScores.push_back( 0.1 );
  discriminantScores.push_back( 0.3 );
  discriminantScores.push_back( 0.6 );

  DecisionRuleType::PriorProbabilityVectorType aPrioris;
  aPrioris.push_back( 0.1 );
  aPrioris.push_back( 0.8 );
  aPrioris.push_back( 0.1 );

  decisionRule->SetPriorProbabilities( aPrioris );
  std::cout << "MaximumRatioDecisionRule: The index of the chosen = "
            << decisionRule->Evaluate( discriminantScores )
            << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
