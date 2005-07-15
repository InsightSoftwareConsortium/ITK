/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumDecisionRuleTest.cxx
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

#include "itkMaximumDecisionRule.h"


int itkMaximumDecisionRuleTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing MaximumDecionRule " << std::endl << std::endl;

  typedef itk::MaximumDecisionRule  DecisionRuleType ;
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();

  std::vector< double > discriminantScores ;
  discriminantScores.resize( 3 ) ;
  
  discriminantScores[0] = 0.0 ;
  discriminantScores[1] = 1.0 ;
  discriminantScores[2] = 2.0 ;

  if ( decisionRule->Evaluate( discriminantScores ) != 2 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }



  DecisionRuleType::VectorType discriminantScores2 ;
  discriminantScores2.resize( 3 ) ;
  
  discriminantScores2[0] = 0.0 ;
  discriminantScores2[1] = 1.0 ;
  discriminantScores2[2] = 2.0 ;

  if ( decisionRule->Evaluate( discriminantScores2 ) != 2 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }


  DecisionRuleType::ArrayType discriminantScores3(3) ;
  
  discriminantScores3[0] = 0.0 ;
  discriminantScores3[1] = 1.0 ;
  discriminantScores3[2] = 2.0 ;

  if ( decisionRule->Evaluate( discriminantScores3 ) != 2 )
    {
    std::cout << "[FAILED]" << std::endl ;
    return EXIT_FAILURE ;
    }



  std::cout << "[SUCCEEDED]" << std::endl ;
  return EXIT_SUCCESS;
}
