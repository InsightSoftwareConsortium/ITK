/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GradientAnisotropicDiffusionImageFilter.cxx
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
//  The Gradient anisotropic diffusion image filter  implements an
//  N-dimensional version of the classic Perona-Malik anisotropic diffusion
//  equation for scalar-valued images.  
//
//  The conductance term for this implementation is chosen as a function of the
//  gradient magnitude of the image at each point, reducing the strength of
//  diffusion at edge pixels.
// 
//  \begin{equation}
//  C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}
//  \end{equation}
//
//  The numerical implementation of this equation is similar to that described
//  in the Perona-Malik paper \cite{Perona1990}, but uses a more robust technique
//  for gradient magnitude estimation and has been generalized to N-dimensions.
//
//  \index{itk::GradientAnisotropicDiffusionImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::GradientAnisotropicDiffusionImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGradientAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 5 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile ";
    std::cerr << "numberOfIterations  timeStep  " << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //  The image types are defined using the pixel type and the dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet



  typedef itk::ImageFileReader< InputImageType >  ReaderType;




  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types. The filter object is created by the \code{New()}
  //  method.
  //
  //  \index{itk::GradientAnisotropicDiffusionImageFilter!instantiation}
  //  \index{itk::GradientAnisotropicDiffusionImageFilter!New()}
  //  \index{itk::GradientAnisotropicDiffusionImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientAnisotropicDiffusionImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );



  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
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
  //  \index{itk::GradientAnisotropicDiffusionImageFilter!Update()}
  //  \index{itk::GradientAnisotropicDiffusionImageFilter!SetTimeStep()}
  //  \index{itk::GradientAnisotropicDiffusionImageFilter!SetIterations()}
  //  \index{SetTimeStep()!itk::GradientAnisotropicDiffusionImageFilter}
  //  \index{SetIterations()!itk::GradientAnisotropicDiffusionImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are 0.25 in $2D$ images and 0.125 in
  //  $3D$ images. The number of iterations can be usually around 5, more
  //  iterations will result in further smoothing and will increase linearly
  //  the computing time.
  //
  //  Software Guide : EndLatex 



  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a writer filter could have been used
  //  after the curvatur flow filter.
  //

  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
               OutputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
 

  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  



  //  Software Guide : BeginLatex
  //  
  // \begin{figure} \center
  // \includegraphics[width=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=6cm]{GradientAnisotropicDiffusionImageFilterOutput.eps}
  // \caption{Effect of the GradientAnisotropicDiffusionImageFilter on a slice
  // from a MRI Proton Density image  of the brain.}
  // \label{fig:GradientAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:GradientAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain. In this example the filter was run with a time step of 0.25,
  //  and 5 iterations.  The figure shows how homogeneous regions are smoothed
  //  and edges are preserved.
  //
  //
  //  Software Guide : EndLatex 


  return 0;

}

