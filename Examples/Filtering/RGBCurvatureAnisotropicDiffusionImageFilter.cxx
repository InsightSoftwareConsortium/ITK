/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RGBCurvatureAnisotropicDiffusionImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The vector anisotropic diffusion approach can equally well be applied to
//  color images. As in the vector case, each RGB component is diffused
//  independently. The following example illustrates the use of the Vector
//  curvature anisotropic diffusion filter on an image with RGB pixel type.
//
//  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter|RGB Images}
//
//  Software Guide : EndLatex 




//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//
//  Also the headers for \code{Image} and \code{RGBPixel} type are required.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//
//  It is desirable to perform the computation on the RGB image using
//  \code{float} representation. However for input and output purposes
//  \code{unsigned char} RGB components are commonly used. It is then necessary
//  to cast the type of color components along the pipeline before writing them
//  to a file. The \code{itk::VectorCastImageFilter} is used to achieve this
//  goal.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVectorCastImageFilter.h"
// Software Guide : EndCodeSnippet



int main( int argc, char ** argv )
{


  if( argc < 5 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputRGBImageFile  outputRGBImageFile ";
    std::cerr << "numberOfIterations  timeStep  " << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  The image type is defined using the pixel type and the dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   itk::RGBPixel< float >     InputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated and a filter object is created by the
  //  \code{New()} method.
  //
  //  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!instantiation}
  //  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!New()}
  //  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::VectorCurvatureAnisotropicDiffusionImageFilter<
                       InputImageType, InputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  const unsigned int numberOfIterations = atoi( argv[3] );


  const double       timeStep = atof( argv[4] );



  //  Software Guide : BeginLatex
  //
  //  This filter requires two parameters, the number of iterations to be
  //  performed and the time step used in the computation of the level set
  //  evolution. These parameters are set using the methods
  //  \code{SetIterations()} and \code{SetTimeStep()} respectively.  The filter
  //  can be executed by invoking \code{Update()}.
  //
  //  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!Update()}
  //  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!SetTimeStep()}
  //  \index{itk::VectorCurvatureAnisotropicDiffusionImageFilter!SetIterations()}
  //  \index{SetTimeStep()!itk::VectorCurvatureAnisotropicDiffusionImageFilter}
  //  \index{SetIterations()!itk::VectorCurvatureAnisotropicDiffusionImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  
  filter->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filter output is now casted to \code{unsigned char} RGB components by
  //  using the \code{VectorCastImageFilter}
  //
  //  \index{itk::VectorCastImageFilter!instantiation}
  //  \index{itk::VectorCastImageFilter!New()}
  //  \index{itk::VectorCastImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< unsigned char >   WritePixelType;
  typedef itk::Image< WritePixelType, 2 >  WriteImageType;

  typedef itk::VectorCastImageFilter< 
                InputImageType, WriteImageType >  CasterType;

  CasterType::Pointer caster = CasterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, the writer type can be instantiated. One writer is created and
  //  connected to the output of the caster.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  caster->SetInput( filter->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  // \begin{figure} \center
  // \includegraphics[width=6cm]{VisibleWomanHeadSlice.eps}
  // \includegraphics[width=6cm]{RGBCurvatureAnisotropicDiffusionImageFilterOutput.eps}
  // \caption{Effect of the VectorCurvatureAnisotropicDiffusionImageFilter on
  // a RGB image from a cryogenic section of the Visible Woman data set.}
  // \label{fig:VectorCurvatureAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure
  //  \ref{fig:VectorCurvatureAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a RGB image from a cryogenic
  //  section of the Visible Woman data set.  In this example the filter was
  //  run with a time step of 0.25, and 5 iterations.  The input image has
  //  570x670 pixels and the processing took 4 minutes on a Pentium 4 2Ghz. 
  //
  //  Software Guide : EndLatex 


  return 0;

}

