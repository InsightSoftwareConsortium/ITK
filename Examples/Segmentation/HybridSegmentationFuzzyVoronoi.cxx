/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    HybridSegmentationFuzzyVoronoi.cxx
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
// This example illustrates the use of the \code{itk::SimpleFuzzyConnectednessScalarImageFilter}
// and \code{itk::VoronoiSegmentationImageFilter} to build a hybrid segmentation framework.
//
// First, the header files of the filters must be included.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"
#include "itkVoronoiSegmentationImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char **argv )
{


  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY estimateMean estimateVariance" << std::endl;
    return 1;
    }




  //  Software Guide : BeginLatex
  //  
  //  Then, we declare the pixel type and image dimension.  With them we
  //  declare the image type to be used as input.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char     InputPixelType;
  const    unsigned int       Dimension = 2;
  
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  A Fuzzy Connectedness segmentation will be performed first to generate a
  //  rough segmentation, and the binary result will be used as a prior for the
  //  next step.  The \code{SimpleFuzzyConnectednessScalarImageFilter} is used
  //  here, but any of the image segmentation filters could do it.  The result
  //  of the segmentation produced by the fuzzy segmentation filter will be
  //  stored in a binary image.  Here we declare the type of this image using a
  //  pixel type and a spatial dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   bool      BinaryPixelType;
  
  typedef itk::Image< BinaryPixelType, Dimension >      BinaryImageType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The fuzzy segmentation filter type is instantiated here using the input
  //  and binary image types as template parameters.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   itk::SimpleFuzzyConnectednessScalarImageFilter< 
                               InputImageType, 
                               BinaryImageType >        FuzzySegmentationFilterType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The fuzzy connectedness segmentation filter is created by invoking the 
  //  \code{New()} method and assigning the result to a \code{SmartPointer}.
  //
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!New()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FuzzySegmentationFilterType::Pointer fuzzysegmenter = 
                                               FuzzySegmentationFilterType::New();
  // Software Guide : EndCodeSnippet




  
  //  Software Guide : BeginLatex
  //  
  //  The second step of this hybrid method involve using the prior generated
  //  from the fuzzy segmentation to build a homogeneity measurement for the
  //  object.  A \code{VoronoiSegmentationImageFilter} is applied based on this
  //  measurement to give the final segmentation result.  In this example, the
  //  result of the \code{VoronoiSegmentationImageFilter} will be sent to a
  //  writer. It is convenient to declare its output type as one compatible with the
  //  writers.  
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned char OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  // Software Guide : EndCodeSnippet

                        

  //  Software Guide : BeginLatex
  //
  //  The following lines declare the type of the voronoi segmentation filter
  //  and create one filter object.
  //
  //  \index{itk::VoronoiSegmentationImageFilter!New()}
  //  \index{itk::VoronoiSegmentationImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::VoronoiSegmentationImageFilter< 
                                    InputImageType, 
                                    OutputImageType > VoronoiSegmentationFilterType;

  VoronoiSegmentationFilterType::Pointer voronoisegmenter = 
                                     VoronoiSegmentationFilterType::New();
  // Software Guide : EndCodeSnippet
  




  //
  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< InputImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );




  //  Software Guide : BeginLatex
  //  
  //  The input to the FuzzySegmentationFilter is taken from the reader.
  //
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetInput()}
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  
  InputImageType::IndexType index;

  index[0] = atoi(argv[3]);
  index[1] = atoi(argv[4]);


  const float  mean     = atof(argv[5]);
  const float  variance = atof(argv[6]);

  


  //  Software Guide : BeginLatex
  //  
  //  The parameters of the FuzzySegmentationFilter are defined here. A seed
  //  point is provided in order to initialize the region to be grown.
  //  Estimated values for the mean and variance of the object intensities are
  //  also provided, a threshold value for generate the binary object is preset.
  //
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetObjectsSeed()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetMean()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetVariance()}
  //  \index{itk::SimpleFuzzyConnectednessScalarImageFilter!SetThreshold()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->SetObjectsSeed( index );
  fuzzysegmenter->SetMean( mean );
  fuzzysegmenter->SetVariance( variance );
  fuzzysegmenter->SetThreshold(0.5);
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The execution of the FuzzySegmentationFilter is triggered with the
  //  \code{Update()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fuzzysegmenter->Update();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The input to the voronoi filter is taken from the reader and the prior is
  //  taken from the fuzzy segmentation filter.
  //
  //  \index{itk::VoronoiSegmentationImageFilter!SetInput()}
  //  \index{itk::VoronoiSegmentationImageFilter!TakeAPrior()}
  //
  //  The tolerence level for testing mean and STD,as well as the "resolution" of segmentation
  //  need to be set.
  //
  //  \index{itk::VoronoiSegmentationImageFilter!SetMeanPercentError()}
  //  \index{itk::VoronoiSegmentationImageFilter!SetSTDPercentError()}
  //  \index{itk::VoronoiSegmentationImageFilter!SetMinRegion()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  voronoisegmenter->SetMeanPercentError(0.2);
  voronoisegmenter->SetSTDPercentError(2.0);
  voronoisegmenter->SetMinRegion(5);
  voronoisegmenter->SetInput( reader->GetOutput() );
  voronoisegmenter->TakeAPrior( fuzzysegmenter->GetOutput());
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  The execution of the VoronoiSegmentationFilter is triggered with the
  //  \code{Update()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  voronoisegmenter->Update();
  // Software Guide : EndCodeSnippet




  
  //  Software Guide : BeginLatex
  //
  //  The segmented image is finally passed to a writer. The invokation of the
  //  \code{Update()} method on the writer will trigger the execution of the
  //  pipeline.
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer->SetInput( voronoisegmenter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet




  return 0;

}




