/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Image2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkImage.h"


  /* Software Guide : BeginLatex

  The first thing required for reading files is to include
  the header file of the ImageFileReader class

  Software Guide : EndLatex */



// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{

  /* Software Guide : BeginLatex

  Then a typical declaration of types for the pixel type 
  and the dimension of the image should be done

  Software Guide : EndLatex */

 
  // Software Guide : BeginCodeSnippet
  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, 3 >         ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  reader->Update();

  ImageType::Pointer image = reader->GetOutput();

  return 0;

}

