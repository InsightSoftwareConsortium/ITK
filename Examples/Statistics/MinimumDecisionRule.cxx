/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MinimumDecisionRule.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

     Copyright (c) Insight Software Consortium. All rights reserved.
     See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
// \index{itk::Statistics::MinimumDecisionRule}
//
// The \code{Evaluate()} method of the \doxygen{MinimumDecisionRule}
// returns the index of the smallest disciminant score among the
// vector of discriminant scores that it receives as input.
//
// To begin this example, we include the class header file.  We also include
// the header file for the \code{std::vector} class that will be the
// container for the discriminant scores.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkMinimumDecisionRule.h"
#include <vector>
// Software Guide : EndCodeSnippet

int main(int, char**)
{
  // Software Guide : BeginLatex
  //
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MinimumDecisionRule DecisionRuleType;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We create the discriminant score vector and fill it with three
  // values. The call \code{Evaluate( discriminantScores )} will return 0
  // because the first value is the smallest value.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::vector< double > discriminantScores;
  discriminantScores.push_back( 0.1 );
  discriminantScores.push_back( 0.3 );
  discriminantScores.push_back( 0.6 );

  std::cout << "MinimumDecisionRule: The index of the chosen = " 
            << decisionRule->Evaluate( discriminantScores )
            << std::endl;
  // Software Guide : EndCodeSnippet

  return 0;
}
