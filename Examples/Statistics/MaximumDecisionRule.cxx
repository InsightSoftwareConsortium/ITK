/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MaximumDecisionRule.cxx
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
// \index{itk::Statistics::Maximum\-Decision\-Rule}
//
// The \doxygen{MaximumDecisionRule} returns the index of the largest
// disciminant score among the discriminant scores in the vector of
// discriminat scores that is the input argument of the \code{Evaluate()}
// method.
//
// To begin the example, we include the header files for the class and the
// MaximumDecisionRule. We also include the header file for thte
// \code{std::vector} class that will be the container for the discriminant
// scores.  
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMaximumDecisionRule.h"
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
  typedef itk::MaximumDecisionRule DecisionRuleType;
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
  std::vector< double > discriminantScores;
  discriminantScores.push_back( 0.1 );
  discriminantScores.push_back( 0.3 );
  discriminantScores.push_back( 0.6 );

  std::cout << "MaximumDecisionRule: The index of the chosen = " 
            << decisionRule->Evaluate( discriminantScores )
            << std::endl;
  // Software Guide : EndCodeSnippet

  return 0;
}
