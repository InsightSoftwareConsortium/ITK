/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NormalVariateGenerator.cxx
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
//
// \index{itk::Statistics::NormalVariateGenerator|textbf}
// \index{Statistics!Random number generation!Normal (Gaussian) distribution|textbf}
//
// The \subdoxygen{Statistics}{NormalVariateGenerator} generates random
// variables according to the standard normal distribution (mean = 0,
// standard deviation = 1).
//
// To use the class in a project, we must link the \code{itkStatistics}
// library to the project.
//
// We include the header file for the class.
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkNormalVariateGenerator.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The \code{NormalVariateGenerator} is not templated. We simply call
  // the \code{New} method to create an instance. Then we provide the
  // seed value using the \code{Initialize(seed value)}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NormalVariateGenerator GeneratorType ;
  GeneratorType::Pointer generator = GeneratorType::New() ;

  generator->Initialize( (int) 2003 ) ;

  for ( unsigned int i = 0 ; i < 50 ; ++i )
    {
    std::cout << i << " : \t" << generator->GetVariate() << std::endl ;
    }
  // Software Guide : EndCodeSnippet

  return 0 ;
}
