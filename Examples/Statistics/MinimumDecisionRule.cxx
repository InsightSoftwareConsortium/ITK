/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MinimumDecisionRule.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

     Copyright (c) 2002 Insight Consortium. All rights reserved.
     See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
// \index{itk::Statistics::MinimumDecisionRule|textbf}
//
// The \code{Eavaluate} method of the \doxygen{MinimumDecisionRule}
// returns the index of the smallest disciminant score among the
// discriminant scores in the vector of discriminat scores that is the
// input argument of the \code{Evaluate} method.
//
// We include the header files for the class and the
// \doxygen{MinimumDecisionRule}. We also include the header file for
// thte \code{std::vector} class that will be the container for the
// discriminant scores.
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkMinimumDecisionRule.h"
#include <vector>
// Software Guide : EndCodeSnippet

int main()
{

  // Software Guide : BeginLatex
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MinimumDecisionRule DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create the discriminant score vector and fill it with three
  // values. The \code{Evaluate( discriminantScores )} will return 0
  // because the first value is the smallest value.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::vector< double > discriminantScores ;
  discriminantScores.push_back( 0.1 ) ;
  discriminantScores.push_back( 0.3 ) ;
  discriminantScores.push_back( 0.6 ) ;

  std::cout << "MinimumDecisionRule: The index of the chosen = " 
            << decisionRule->Evaluate( discriminantScores )
            << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
