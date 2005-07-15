/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumRatioDecisionRuleTest.cxx
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

#include <iostream>
#include <vector>

#include "itkMaximumRatioDecisionRule.h"


int itkMaximumRatioDecisionRuleTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing MaximumRatioDecionRule " << std::endl << std::endl;

  typedef itk::MaximumRatioDecisionRule  DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  std::vector< double > discriminantScores ;
  discriminantScores.resize( 3 ) ;
  
  discriminantScores[0] = 0.3 ;
  discriminantScores[1] = 0.5 ;
  discriminantScores[2] = 0.2 ;

  DecisionRuleType::APrioriVectorType aPrioris ;
  aPrioris.resize(3) ;
  
  aPrioris[0] = 0.2 ;
  aPrioris[1] = 0.5 ;
  aPrioris[2] = 0.3 ;

  decisionRule->SetAPriori( aPrioris ) ;

  if ( decisionRule->Evaluate( discriminantScores ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }


  DecisionRuleType::VectorType discriminantScores2;
  discriminantScores2.resize( 3 ) ;
  
  discriminantScores2[0] = 0.3 ;
  discriminantScores2[1] = 0.5 ;
  discriminantScores2[2] = 0.2 ;

  if ( decisionRule->Evaluate( discriminantScores2 ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }




  DecisionRuleType::ArrayType discriminantScores3(3);
  
  discriminantScores3[0] = 0.3 ;
  discriminantScores3[1] = 0.5 ;
  discriminantScores3[2] = 0.2 ;

  if ( decisionRule->Evaluate( discriminantScores3 ) != 1 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }




  std::cout << "[SUCCEEDED]" << std::endl ;
  return EXIT_SUCCESS;
}
