/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MinMaxCurvatureFlowImageFilter.cxx
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
//  The MinMaxCurvatureFlow image filter applies a variant of the CurvatureFlow
//  algorithm. The basic difference is that the term speed is choosen as
//  $\min(\kappa,0)$ or $\max(\kappa,0)$ depending on the average intensity
//  of the pixel neighborhood. This prevents small oscilations from happening
//  on regions of the contour containing wiggeling sections. The speed is given by
//
//  \begin{equation}
//  I_t = F |\nabla I|
//  \end{equation}
//
//  where $F$ is defined as
//
//  \begin{equation}
//  F = \left\{ \begin{array} {r@{\quad:\quad}l}
//         \min(\kappa,0) & \mbox{Average} < Threshold \\ \max(\kappa,0) & \mbox{Average} \ge Threshold 
//             \end{array} \right.
//  \end{equation}
//
// The average is computed over a neighborhood of the pixel and the threshold
// is calculated by the algorithm as some average pixel value in the gradient
// direction. The integer radius of this neighborhood is selected by the user.
//
//  \index{itk::MinMaxCurvatureFlowImageFilter|textbf}
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
//  \index{itk::MinMaxCurvatureFlowImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkMinMaxCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 6 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  ";
    std::cerr << "numberOfIterations  timeStep  stencilRadius" << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images and
  //  with them the image types are instantiated.
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
  //  The MinMaxCurvatureFlow filter type is now instantiated using both the
  //  input image and the output image types. The filter is then created using
  //  the \code{New()} method.
  //
  //  \index{itk::MinMaxCurvatureFlowImageFilter!instantiation}
  //  \index{itk::MinMaxCurvatureFlowImageFilter!New()}
  //  \index{itk::MinMaxCurvatureFlowImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MinMaxCurvatureFlowImageFilter<
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

  typedef FilterType::RadiusValueType RadiusType;

  const RadiusType radius = atol( argv[5] );

  //  Software Guide : BeginLatex
  //
  //  The MinMaxCurvatureFlow filter requires the two normal parameters of the
  //  CurvatureFlow image, the number of iterations to be performed and the
  //  time step used in the computation of the level set evolution. In addition
  //  the them, the radius of the neighborhood is also required. This last
  //  parameter is passes using the \code{SetStencilRadius()} method. Note that
  //  the radius is provided as an integer number since it is refering to a
  //  number of pixels from the center to the border of the neighborhood. Then
  //  the filter can be executed by invoking \code{Update()}.
  //
  //  \index{itk::MinMaxCurvatureFlowImageFilter!Update()}
  //  \index{itk::MinMaxCurvatureFlowImageFilter!SetTimeStep()}
  //  \index{itk::MinMaxCurvatureFlowImageFilter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::MinMaxCurvatureFlowImageFilter}
  //  \index{SetNumberOfIterations()!itk::MinMaxCurvatureFlowImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetTimeStep( timeStep );
  filter->SetNumberOfIterations( numberOfIterations );

  filter->SetStencilRadius( radius );
  
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are 0.25 in $2D$ images and 0.125 in
  //  $3D$ images. The number of iterations can be usually around 10, more
  //  iterations will result in further smoothing and will increase linearly
  //  the computing time. The radius of the stencil can be typically 1. The
  //  edge-preserving is not an absolute on this filter, some degradation will
  //  ocurr on the edges and will accentuate as the number of iterations are
  //  increased. 
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
  // \includegraphics[width=6cm]{MinMaxCurvatureFlowImageFilterOutput.eps}
  // \caption{Effect of the MinMaxCurvatureFlowImageFilter on a slice from a MRI
  // Proton Density image  of the brain.}
  // \label{fig:MinMaxCurvatureFlowImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:MinMaxCurvatureFlowImageFilterInputOutput} illustrates
  //  the effect of this filter on a MRI proton density image of the brain. In
  //  this example the filter was run with a time step of 0.25, 10 iterations
  //  and a radius of 1.  The figure shows how homogeneous regions are smoothed
  //  and edges are preserved.
  //
  //
  //  Software Guide : EndLatex 


  return 0;

}

