/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ScalarImageMarkovRandomField1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example shows how to use the Markov Random Field approach for
// classifying the pixel of a scalar image.
//
// The  \subdoxygen{Statistics}{MRFImageFilter} is used for refining an initial
// classification by introducing the spatial coherence of the labels. The user
// should provide two images as input. The first image is the one to be
// classified while the second image is an image of labels representing an
// initial classification.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkFixedArray.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarToArrayCastImageFilter.h"
#include "itkMRFImageFilter.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << " inputScalarImage inputLabeledImage";
    std::cerr << " outputLabeledImage numberOfClasses" << std::endl;
    return EXIT_FAILURE;
    }

  const char * inputImageFileName      = argv[1];
  const char * inputLabelImageFileName = argv[2];
  const char * outputImageFileName     = argv[3];

  const unsigned int numberOfClasses   = atoi( argv[4] );


// Software Guide : BeginLatex
//
// First we define the pixel type and dimension of the image that we intend to
// classify. With this image type we can also declare the
// \doxygen{ImageFileReader} needed for reading the input image, create one and
// set its input filename. In this particular case we choose to use
// \code{signed short} as pixel type, which is typical for MicroMRI and CT data
// sets.
//
// Software Guide : EndLatex 
  
// Software Guide : BeginCodeSnippet
  typedef signed short        PixelType;
  const unsigned int          Dimension = 3;

  typedef itk::Image<PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputImageFileName );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// As a second step we define the pixel type and dimension of the image of
// labels that provides the initial classification of the pixels from the first
// image. This initial labeled image can be the output of a K-Means method like
// the one illustrated in section \ref{sec:KMeansClassifier}.
//
// Software Guide : EndLatex 
  
// Software Guide : BeginCodeSnippet
  typedef unsigned char       LabelPixelType;

  typedef itk::Image<LabelPixelType, Dimension > LabelImageType;

  typedef itk::ImageFileReader< LabelImageType > LabelReaderType;
  LabelReaderType::Pointer labelReader = LabelReaderType::New();
  labelReader->SetFileName( inputLabelImageFileName );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// Since the Markov Random Field algorithm is defined in general for images
// whose pixels have multiple components, that is, images of vector type, we
// must adapt our scalar image in order to satisfy the interface expected by
// the \code{MRFImageFilter}. We do this by using the
// \doxygen{ScalarToArrayCastImageFilter}. With this filter we will present our
// scalar image as a vector image whose vector pixels contain a single
// component. 
// 
// Software Guide : EndLatex 
  
// Software Guide : BeginCodeSnippet
  typedef itk::FixedArray<LabelPixelType,1>  ArrayPixelType;

  typedef itk::Image< ArrayPixelType, Dimension > ArrayImageType;

  typedef itk::ScalarToArrayCastImageFilter< 
                     ImageType, ArrayImageType > ScalarToArrayFilterType;

  ScalarToArrayFilterType::Pointer scalarToArrayFilter = ScalarToArrayFilterType::New();

  scalarToArrayFilter->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// With the input image type \code{ImageType} and labeled image type
// \code{LabelImageType} we instantiate the type of the
// \doxygen{MRFImageFilter} that will apply the Markov Random Field algorithm
// in order to refine the pixel classification.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::MRFImageFilter< ArrayImageType, LabelImageType > MRFFilterType;

  MRFFilterType::Pointer mrfFilter = MRFFilterType::New();

  mrfFilter->SetInput( scalarToArrayFilter->GetOutput() );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// We set now some of the parameters for the MRF filter. In particular, the
// number of classes to be used during the classification, the maximum number
// of iterations to be run in this filter and the error tolerance that will be
// used as a criterion for convergence.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  mrfFilter->SetNumberOfClasses( numberOfClasses );
  mrfFilter->SetMaximumNumberOfIterations( 200 );
  mrfFilter->SetErrorTolerance( 1e-7 );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// We also set the Smoothing factor. This factor will multiply the weights that
// define the influecnce of neighbors on the classification of a given pixel.
// The higher the value, the more uniform will be the regions resulting from
// the classification refinement.
// 
// Software Guide : EndLatex 
 
// Software Guide : BeginCodeSnippet
  mrfFilter->SetSmoothingFactor( 1.0 );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// and we set the neighborhood radius that will define the size of the clique
// to be used in the computation of the neighbors' influence in the
// classification of any given pixel. Note that despite the fact that we call
// this a radius, it is actually the half size of an hypercube. That is, the
// actual region of influence will not be circular but rather an N-Dimensional
// box. For example, a neighborhood radius of 2 in a 3D image will result in a
// clique of size 5x5x5 pixels.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  mrfFilter->SetNeighborhoodRadius( 2 );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The output image profuced by the \doxygen{MRFImageFilter} has the same pixel
// type as the labeled input image. In the following lines we use the
// \code{OutputImageType} in order to instantiate the type of a
// \doxygen{ImageFileWriter}. Then create one, and connect it to the output of
// the classification filter.
//
// Software Guide : EndLatex 

// Software Guide : EndCodeSnippet
  typedef MRFFilterType::OutputImageType  OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  
  writer->SetInput( mrfFilter->GetOutput() );

  writer->SetFileName( outputImageFileName );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We are now ready for triggering the execution of the pipeline. This is done
// by simply invoking the \code{Update()} method in the writer. This call will
// propagate the update request to the reader and then to the MRF filter.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while writing image file : " << argv[2] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
// Software Guide : EndCodeSnippet

  


// Software Guide : BeginLatex
//
// The execution of this example in one the input image ####, produces the
// result illustrated in figure ####
//
// Software Guide : EndLatex 


  return 0;
  
}


