/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MaximumRatioDecisionRule.cxx
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
// \index{itk::Statistics::MaximumRatioDecisionRule|textbf}
//
// The \code{Evaluate} method of the \doxygen{MaximumRatioDecisionRule}
// returns the index, $i$ if 
// \begin{equation}
//   \frac{f_{i}(\overrightarrow{x})}{f_{j}(\overrightarrow{x})} >
//   \frac{K_{j}}{K_{i}} \textrm{ for all } j \not= i 
// \end{equation} 
//where the $i$ is the index of a class which has membership function
//$f_{i}$ and its prior value (usually, the \textit{a priori}
// probability of the class) is $K_{i}$ 
//
// We include the header files for the class and the
// \doxygen{MaximumRatioDecisionRule}. We also include the header file
// for the \code{std::vector} class that will be the container for the
// discriminant scores.  
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMaximumRatioDecisionRule.h"
#include <vector>
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MaximumRatioDecisionRule DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create the discriminant score vector and fill it with three
  // values. We also create a vector (\code{aPrioris}) for the \textit{a
  // priori} values. The \code{Evaluate( discriminantScores )} will
  // return 1. 
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::vector< double > discriminantScores ;
  discriminantScores.push_back( 0.1 ) ;
  discriminantScores.push_back( 0.3 ) ;
  discriminantScores.push_back( 0.6 ) ;

  DecisionRuleType::APrioriVectorType aPrioris ;
  aPrioris.push_back( 0.1 ) ;
  aPrioris.push_back( 0.8 ) ;
  aPrioris.push_back( 0.1 ) ;

  decisionRule->SetAPriori( aPrioris ) ;
  std::cout << "MaximumRatioDecisionRule: The index of the chosen = " 
            << decisionRule->Evaluate( discriminantScores )
            << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
