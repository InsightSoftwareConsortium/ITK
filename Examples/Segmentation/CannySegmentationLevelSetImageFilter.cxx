/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CannySegmentationLevelSetImageFilter.cxx
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
// \index{itk::CannySegmentationLevelSetImageFilter}
//
// The \doxygen{CannySegmentationLevelSetImageFilter} defines a speed term that
// minimizes distance to Canny edges in an image.  The initial level-set
// model moves through to a gradient advection field until it locks onto those edges.
// This filter is more suitable for refining existing segmentations than as a
// region-growing algorithm.
//
// The two terms defined for the \doxygen{CannySegmentationLevelSetImageFilter}
// are the advection term and the propagation term from
// equation~\ref{eqn:LevelSetEquation}.  The advection term is constructed by
// minimizing the squared distance transform from the Canny edges.
//
// \begin{equation}
// \label{eqn:CannySegmentationLevelSetImageFilterAdvection}
// \mbox{min} \int D^2 \Rightarrow D \nabla D
// \end{equation}
//
// where the distance transform $D$ is calculated using
// \doxygen{DanielssonDistanceMapImageFilter} applied to the output of the
// \doxygen{CannyEdgeDetectionImageFilter}.
//
// For cases where you want to allow for some surface expansion, you may set a
// non-zero value for the propagation term.  The propagation term is simply
// $D$.  As with all ITK level-set segmentation filters, the curvature term
// controls the smoothness of the surface.
//
// \doxygen{CannySegmentationLevelSetImageFilter} expects two inputs.  The
// first is an initial level set in the form of an \doxygen{Image}. The second
// input is the feature image $g$ from which propagation and advection terms
// are calculated.  It is generally a good idea to do some preprocessing of the
// feature image to remove noise.
// 
// The following example illustrates the use of the
// \doxygen{CannySegmentationLevelSetImageFilter}.
// Figure~\ref{fig:CannySegmentationLevelSetImageFilterDiagram} shows how the
// image processing pipeline is constructed.  We read two images: the image to
// segment and the image that contains the initial implicit surface.  The goal
// is to refine the initial model from the second input and not to grow a new
// segmentation from seed points.. The \code{feature} image is preprocessed
// with a few iterations of an anisotropic diffusion filter.
//
// \begin{figure} \center
// \includegraphics[width=15cm]{CannySegmentationLevelSetImageFilterCollaborationDiagram1.eps}
// \caption[CannySegmentationLevelSetImageFilter collaboration
// diagram]{Collaboration diagram of the CannySegmentationLevelSetImageFilter
// applied to a segmentation task.}
// \label{fig:CannySegmentationLevelSetImageFilterDiagram}
// \end{figure}
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 


#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkCannySegmentationLevelSetImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet
#include "itkFastMarchingImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkZeroCrossingImageFilter.h"


int main( int argc, char *argv[] )
{

  if( argc < 9 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  InitialModel OutputImage";
    std::cerr << " CannyThreshold ";
    std::cerr << " CannyVariance ";
    std::cerr << " AdvectionWeight";
    std::cerr << " InitialModelIsovalue";
    std::cerr << " MaximumIterations" << std::endl;
    return 1;
    }

  //  Software Guide : BeginLatex
  //  
  //  We declare the image type using a pixel type and a particular
  //  dimension. In this case we will use 2D \code{float} images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet

  typedef unsigned char OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::BinaryThresholdImageFilter< 
                        InternalImageType, 
                        OutputImageType    >    ThresholdingFilterType;

  ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();
                        
  thresholder->SetUpperThreshold( 10.0 );
  thresholder->SetLowerThreshold( 0.0 );

  thresholder->SetOutsideValue(  0  );
  thresholder->SetInsideValue(  255 );

  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );
  writer->SetFileName(  argv[3] );

  //  Software Guide : BeginLatex
  //
  //  The input image will be processed with a few iterations of
  //  feature-preserving diffusion.  We create a filter and set the parameters.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientAnisotropicDiffusionImageFilter< InternalImageType,
    InternalImageType> DiffusionFilterType;
  DiffusionFilterType::Pointer diffusion = DiffusionFilterType::New();
  diffusion->SetNumberOfIterations(5);
  diffusion->SetTimeStep(0.20);
  diffusion->SetConductanceParameter(1.0);
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //  
  //  The following lines instantiate a
  //  \doxygen{ThresholdSegmentationLevelSetImageFilter} and create an object of this type
  //  using the \code{New()} method.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef  itk::CannySegmentationLevelSetImageFilter< 
                              InternalImageType, 
                              InternalImageType >    CannySegmentationLevelSetImageFilterType;

  CannySegmentationLevelSetImageFilterType::Pointer cannySegmentation = 
                                     CannySegmentationLevelSetImageFilterType::New();
  // Software Guide : EndCodeSnippet

  
  //  Software Guide : BeginLatex
  //  
  // As with the other ITK level-set segmentation filters, the terms of the
  // \code{CannySegmentationLevelSetImageFilter} level-set equation can be
  // weighted by scalars.  For this application we will modify the relative
  // weight of the advection term.  The propagation and curvature term weights
  // are set to their defaults of $0$ and $1$, respectively.
  //
  //  \index{itk::CannySegmentationLevelSetImageFilter!SetAdvectionScaling()}
  //  \index{itk::SegmentationLevelSetImageFilter!SetAdvectionScaling()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  cannySegmentation->SetAdvectionScaling( ::atof(argv[6]) );
  cannySegmentation->SetCurvatureScaling( 1.0 );
  cannySegmentation->SetPropagationScaling( 0.0 );
  //  Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //
  //  The maximum number of iterations is set from the command line.  It may
  //  not be desireable in some applications to run the filter until
  //  convergence.  Only a few iterations may be required.
  //  
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  cannySegmentation->SetMaximumRMSError( 0.002 );
  cannySegmentation->SetMaximumIterations( ::atoi(argv[8]) );
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //
  //  There are two parameters in the
  //  \doxygen{CannySegmentationLevelSetImageFilter} to control the behavior of
  //  the Canny edge detection.  The \em{variance} parameter controls the
  //  amount of Gaussian smoothing on the input image.  The \em{threshold}
  //  parameter indicates the lowest allowed value in the output image.
  //  Thresholding is used to suppress Canny edges whose gradient magnitudes
  //  fall below a certain value.
  //  
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  cannySegmentation->SetThreshold( ::atof(argv[4]) );
  cannySegmentation->SetVariance(  ::atof(argv[5]) );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Finally, it is very important to specify the isovalue of the surface in
  // the initial model input image. In a binary image, for example, the
  // isosurface is found midway between the foreground and background values.
  //
  // Software Guide : EndLatex
  
  // Software Guide : BeginCodeSnippet
  cannySegmentation->SetIsoSurfaceValue( ::atof(argv[7]) );
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //  
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:CannyLevelSetSegmentationCollaborationDiagram1}.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  diffusion->SetInput( reader1->GetOutput() );
  cannySegmentation->SetInput( reader2->GetOutput() );
  cannySegmentation->SetFeatureImage( diffusion->GetOutput() );
  thresholder->SetInput( cannySegmentation->GetOutput() );
  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet

  
  //  Software Guide : BeginLatex
  //  
  //  Invoking the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  As usual, the call is placed in a
  //  \code{try/catch} block to handle any exceptions that may be thrown.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  try
    {
      writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet

  // Print out some useful information 
  std::cout << std::endl;
  std::cout << "Max. no. iterations: " << cannySegmentation->GetMaximumIterations() << std::endl;
  std::cout << "Max. RMS error: " << cannySegmentation->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << cannySegmentation->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << cannySegmentation->GetRMSChange() << std::endl;

  //  Software Guide : BeginLatex
  //
  //  We can use this filter to make some subtle refinements to the ventricle
  //  segmentation from the previous example of
  //  \doxygen{ThresholdSegmentationLevelSetImageFilter}.  The application was
  //  run using \code{Insight/Examples/Data/BrainProtonDensitySlice.png} and
  //  \code{Insight/Examples/Data/VentricleModel.png} as inputs, a
  //  \code{threshold} of $7.0$, \code{variance} of $0.1$, \code{advection
  //  weight} of $10.0$, and an initial isosurface value of $127.5$.  One case
  //  was run for $15$ iterations and the second was run to convergence.
  //  Compare the results in the two, right-most images of
  //  figure~\ref{CannyLevelSetSegmentationImageFilter} with the ventricle
  //  segmentation from figure~\ref{ThresholdSegmentationLevelSetImageFilter}
  //  shown in the middle.  Jagged edges are straightened and the small spur at
  //  the upper right-hand side of the mask has been removed.
  //
  //  \begin{figure}
  //  \includegraphics[width=4cm]{BrainProtonDensitySlice.eps}
  //  \includegraphics[width=4cm]{ThresholdSegmentationLevelSetImageFilterVentricle.eps}
  //  \includegraphics[width=4cm]{CannySegmentationLevelSetImageFilterVentricle1.eps}
  //  \includegraphics[width=4cm]{CannySegmentationLevelSetImageFilterVentricle2.eps}
  //  \caption[Segmentation results of CannyLevelSetImageFilter]{Results of
  //  applying CannySegmentationLevelSetImageFilter to a prior ventricle
  //  segmentation.  Shown from left to right are the original image, the
  //  prior segmentation of the ventricle from
  //  figure~\ref{ThresholdSegmentationLevelSetImageFilter}, $15$ iterations of
  //  the \code{CannySegmentationLevelSetImageFilter}, and the
  //  \code{CannySegmentationLevelSetImageFilter} run to convergence.}
  //  \label{fig:CannySegmentationLevelSetImageFilter}
  //  \end{figure}
  //
  //  The free parameters of this filter can be adjusted to achieve a wide
  //  range of shape variations from the original model.  Finding the right
  //  parameters for your particular application is usually a process of trial
  //  and error.  As with most ITK level-set segmentation filters, examining
  //  the propagation (speed) and advection images can help the process of
  //  tuning parameters.  These images are available using \code{Set/Get}
  //  methods from the filter after it has been updated.
  //
  //  Software Guide : EndLatex 

  return 0;
}
