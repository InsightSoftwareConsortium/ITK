/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ConfidenceConnected.cxx
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
// This example illustrates the use of the \code{itk::ConfidenceConnectedImageFilter}
//
// \index{itk::ConfidenceConnectedImageFilter|textbf}
// \index{itk::ConfidenceConnectedImageFilter!header}
//
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkConfidenceConnectedImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"
#include "itkCastImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char **argv )
{


  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY " << std::endl;
    return 1;
    }
  

  return 0;

}

