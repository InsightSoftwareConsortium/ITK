/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CurvatureFlowImageFilter.cxx
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
//  The CurvatureFlow image filter performs anisotropic diffusion on the
//  image. Diffusion is controled by a factor dependent on the image gradient.
//  This prevents diffusion from happening in regions of high gradients that are
//  typically associated with edges on the image. The result is to smooth pixel
//  values on the homogeneous regions while leaving the contours unmodified.
//
//  This filter assimilates each iso-contour of the graylevel image as a level
//  set. The level sets evolved under the control of a diffusion equation where
//  the speed is proportional to the curvature of the contour. The speed term
//  is given by 
//
//  \begin{equation}
//  I_t = \kappa |\nabla I|
//  \end{equation}
//
//  where $ \kappa $ is the curvature.
//
//  \index{itk::CurvatureFlowImageFilter|textbf}
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
//  \index{itk::CurvatureFlowImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 5 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  numberOfIterations  timeStep" << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  With them, the input and output image types can be instantiated.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet



  typedef itk::ImageFileReader< InputImageType >  ReaderType;




  //  Software Guide : BeginLatex
  //
  //  The CurvatureFlow filter type is now instantiated using both the
  //  input image and the output image types.
  //
  //  \index{itk::CurvatureFlowImageFilter!instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CurvatureFlowImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a SmartPointer.
  //
  //  \index{itk::CurvatureFlowImageFilter!New()}
  //  \index{itk::CurvatureFlowImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet




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
  //  The CurvatureFlow filter requires two parameters, the number of
  //  iterations to be performed and the time step used in the computation of
  //  the level set evolution. These two parameters are set using the methods
  //  \code{SetNumberOfIterations()} and \code{SetTimeStep()} respectively.
  //  Then the filter can be executed by invoking \code{Update()}.
  //
  //  \index{itk::CurvatureFlowImageFilter!Update()}
  //  \index{itk::CurvatureFlowImageFilter!SetTimeStep()}
  //  \index{itk::CurvatureFlowImageFilter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::CurvatureFlowImageFilter}
  //  \index{SetNumberOfIterations()!itk::CurvatureFlowImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are 0.25 in $2D$ images and 0.125 in
  //  $3D$ images. The number of iterations can be usually around 10, more
  //  iterations will result in further smoothing and will increase linearly
  //  the computing time. The edge-preserving is not an absolute on this
  //  filter, some degradation will ocurr on the edges and will accentuate as
  //  the number of iterations is increased. 
  //
  //  Software Guide : EndLatex 



  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a writer filter could have been used
  //  after the curvatur flow filter.
  //
  //  Software Guide : EndLatex 

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
 
  // Software Guide : BeginCodeSnippet
  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet
  



  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=6cm]{CurvatureFlowImageFilterOutput.eps}
  // \caption{Effect of the CurvatureFlowImageFilter on a slice from a MRI
  // Proton Density image  of the brain.}
  // \label{fig:CurvatureFlowImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:CurvatureFlowImageFilterInputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain. In this
  //  example the filter was run with a time step of 0.25 and 10 iterations.
  //  The figure shows how homogeneous regions are smoothed and edges are
  //  preserved.
  //
  //
  //  Software Guide : EndLatex 


  return 0;

}

