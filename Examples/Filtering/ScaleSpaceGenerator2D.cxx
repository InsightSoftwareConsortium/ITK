/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ScaleSpaceGenerator2D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  We now use the previous example for building the ScaleSpace of a 2D image.
//  Since most of the code is the same, we will focus only on the extra lines
//  needed for generating the Scale Space.
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.h"

#include <stdio.h>


int main( int argc, char * argv[] )
{
  if( argc < 3 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFileBase " << std::endl;
    return 1;
    }

  
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::LaplacianRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  FilterType::Pointer laplacian = FilterType::New();

  laplacian->SetNormalizeAcrossScale( true );

  laplacian->SetInput( reader->GetOutput() );


  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( laplacian->GetOutput() );


  //  Software Guide : BeginLatex
  //  
  //  Interestingly, all comes down to looping over several scales,
  //  by setting different sigma values and selecting the filename
  //  of the slice corresponding to that scale value.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  char filename[2000];

  for( unsigned int slice=0; slice < 100; slice++ )
    {
    sprintf( filename, "%s%03d.mhd", argv[2], slice );

    writer->SetFileName( filename );

    const float sigma = static_cast< float >( slice ) / 10.0 + 1.0;  

    laplacian->SetSigma( sigma );
    writer->Update();
    }
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The set of images can now be loaded in a Viewer, such as VolView or
  //  ParaView, and iso-surfaces can be traced at the zero value. These
  //  surfaces will correspond to the zero-crossings of the laplacian and
  //  therefore their stability along Scales will represent the significance of
  //  these features as edges in the original image.
  //
  //  Software Guide : EndLatex 


  return 0;
}

