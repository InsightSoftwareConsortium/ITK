/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    LaplacianSegmentationLevelSetImageFilter.cxx
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
// \index{itk::LaplacianSegmentationLevelSetImageFilter}
//
// The \doxygen{LaplacianSegmentationLevelSetImageFilter} defines a speed term
// based on second derivative features in the image.  The speed term is
// calculated as the Laplacian of the image values.  The goal is to attract the
// the evolving level-set surface to local zero-crossings in the Laplacian
// image.  Like \doxygen{CannySegmentationLevelSetImageFilter}, this filter is
// more suitable for refining existing segmentations than as a stand-alone,
// region growing algorithm.  It is possible to perform region growing
// segmentation, but be aware that the growing surface may tend to become
// ``stuck'' at local edges.
//
// The propagation (speed) term for the
// \doxygen{LaplacianSegmentationLevelSetImageFilter} is constructed by
// applying \doxygen{LaplacianImageFilter} to the input feature image.  One
// nice property of using the Laplacian is that there are no free parameters in
// the calculation.
//
// \doxygen{LaplacianSegmentationLevelSetImageFilter} expects two inputs.  The
// first is an initial level set in the form of an \doxygen{Image}. The second
// input is the feature image $g$ from which the propagation term is calculated
// (see equation~\ref{eqn:LevelSetEquation}).  Because the filter performs a
// second derivative calculation, it is generally a good idea to do some
// preprocessing of the feature image to remove noise.
//
// The following example illustrates the use of the
// \doxygen{LaplacianSegmentationLevelSetImageFilter}.
// Figure~\ref{fig:LaplacianSegmentationLevelSetImageFilterDiagram} shows how
// the image processing pipeline is constructed.  We read two images: the image
// to segment and the image that contains the initial implicit surface.  The
// goal is to refine the initial model from the second input to better match
// the structure represented by the initial implicit surface (a prior
// segmentation).  The \code{feature} image is preprocessed using an
// anisotropic diffusion filter.
//
// \begin{figure} \center
// \includegraphics[width=\textwidth]{LaplacianSegmentationLevelSetImageFilterCollaborationDiagram1.eps}
// \caption[LaplacianSegmentationLevelSetImageFilter collaboration
// diagram]{An image processing pipeline using
// LaplacianSegmentationLevelSetImageFilter for segmentation.}
// \label{fig:LaplacianSegmentationLevelSetImageFilterDiagram}
// \end{figure}
//
// Let's start by including the appropriate header files.
//
// Software Guide : EndLatex 

#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkLaplacianSegmentationLevelSetImageFilter.h"
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
    std::cerr << " DiffusionIterations ";
    std::cerr << " DiffusionConductance ";
    std::cerr << " PropagationWeight";
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
  //  feature-preserving diffusion.  We create a filter and set the
  //  parameters. The number of iterations and the conductance parameter are
  //  taken from the command line.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientAnisotropicDiffusionImageFilter< InternalImageType,
    InternalImageType> DiffusionFilterType;
  DiffusionFilterType::Pointer diffusion = DiffusionFilterType::New();
  diffusion->SetNumberOfIterations( atoi(argv[4]) );
  diffusion->SetTimeStep(0.20);
  diffusion->SetConductanceParameter( atof(argv[5]) );
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //  
  //  The following lines define and instantiate a \doxygen{LaplacianSegmentationLevelSetImageFilter}.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef  itk::LaplacianSegmentationLevelSetImageFilter< 
                              InternalImageType, 
                              InternalImageType >    LaplacianSegmentationLevelSetImageFilterType;

  LaplacianSegmentationLevelSetImageFilterType::Pointer laplacianSegmentation = 
                                     LaplacianSegmentationLevelSetImageFilterType::New();
  // Software Guide : EndCodeSnippet

  
  //  Software Guide : BeginLatex
  //  
  // As with the other ITK level-set segmentation filters, the terms of the
  // \code{LaplacianSegmentationLevelSetImageFilter} level-set equation can be
  // weighted by scalars.  For this application we will modify the relative
  // weight of the propagation term.  The curvature term weight is set to its
  // default of $1$.  The advection term is not used in this filter.
  //
  //  \index{itk::LaplacianSegmentationLevelSetImageFilter!SetPropagationScaling()}
  //  \index{itk::SegmentationLevelSetImageFilter!SetPropagationScaling()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  laplacianSegmentation->SetCurvatureScaling( 1.0 );
  laplacianSegmentation->SetPropagationScaling( ::atof(argv[6]) );
  //  Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //
  //  The maximum number of iterations is set from the command line.  It may
  //  not be desirable in some applications to run the filter to
  //  convergence.  Only a few iterations may be required.
  //  
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  laplacianSegmentation->SetMaximumRMSError( 0.002 );
  laplacianSegmentation->SetMaximumIterations( ::atoi(argv[8]) );
  // Software Guide : EndCodeSnippet
  laplacianSegmentation->SetUseNegativeFeaturesOn();
  
  // Software Guide : BeginLatex
  //
  // Finally, it is very important to specify the isovalue of the surface in
  // the initial model input image. In a binary image, for example, the
  // isosurface is found midway between the foreground and background values.
  //
  // Software Guide : EndLatex
  
  // Software Guide : BeginCodeSnippet
  laplacianSegmentation->SetIsoSurfaceValue( ::atof(argv[7]) );
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //  
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:LaplacianLevelSetSegmentationCollaborationDiagram}.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  diffusion->SetInput( reader1->GetOutput() );
  laplacianSegmentation->SetInput( reader2->GetOutput() );
  laplacianSegmentation->SetFeatureImage( diffusion->GetOutput() );
  thresholder->SetInput( laplacianSegmentation->GetOutput() );
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
  std::cout << "Max. no. iterations: " << laplacianSegmentation->GetMaximumIterations() << std::endl;
  std::cout << "Max. RMS error: " << laplacianSegmentation->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << laplacianSegmentation->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << laplacianSegmentation->GetRMSChange() << std::endl;


  // Write out the speed (propagation) image for parameter tuning purposes.
  itk::ImageFileWriter< InternalImageType >::Pointer speedWriter
    = itk::ImageFileWriter< InternalImageType >::New();
  speedWriter->SetInput( laplacianSegmentation->GetSpeedImage() );
  speedWriter->SetFileName( "speedImage.mha" );
  speedWriter->Update();
  
  //  Software Guide : BeginLatex
  //
  //  We can use this filter to make some subtle refinements to the ventricle
  //  segmentation from the example of
  //  \doxygen{ThresholdSegmentationLevelSetImageFilter}.  The application was
  //  run using \code{Insight/Examples/Data/BrainProtonDensitySlice.png} and
  //  \code{Insight/Examples/Data/VentricleModel.png} as inputs.  We used $10$
  //  iterations of the diffusion filter with a conductance of 2.0.  The
  //  propagation scaling was set to $1.0$ and the filter was run until
  //  convergence.
  //  Compare the results in the rightmost images of
  //  figure~\ref{LaplacianLevelSetSegmentationImageFilter} with the ventricle
  //  segmentation from figure~\ref{ThresholdSegmentationLevelSetImageFilter}
  //  shown in the middle.  Jagged edges are straightened and the small spur at
  //  the upper right-hand side of the mask has been removed.
  //
  //  \begin{figure}
  //  \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice.eps}
  //  \includegraphics[width=0.32\textwidth]{ThresholdSegmentationLevelSetImageFilterVentricle.eps}
  //  \includegraphics[width=0.32\textwidth]{LaplacianSegmentationLevelSetImageFilterVentricle.eps}
  //  \caption[Segmentation results of LaplacianLevelSetImageFilter]{Results of
  //  applying LaplacianSegmentationLevelSetImageFilter to a prior ventricle
  //  segmentation.  Shown from left to right are the original image, the
  //  prior segmentation of the ventricle from
  //  figure~\ref{ThresholdSegmentationLevelSetImageFilter}, and the refinement of the
  //  prior using \code{LaplacianSegmentationLevelSetImageFilter} }
  //  \label{fig:LaplacianSegmentationLevelSetImageFilter}
  //  \end{figure}
  //
  //  Software Guide : EndLatex 

  return 0;
}
