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
  //  First, We declare the pixel type and image dimension.
  //  With them we declare the image type.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned short       InputPixelType;
  const unsigned int  Dimension = 2;
  
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;
  // Software Guide : EndCodeSnippet

  typedef bool BinaryPixelType;
  typedef itk::Image< BinaryPixelType, Dimension > BinaryImageType;

  typedef unsigned short OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

                        
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
  //  A Fuzzy Connectedness segmentation was performed first to generate a rough 
  //  segmentation, and the binary result was used as a prior for the next step.
  //  \code{SimpleFuzzyConnectednessScalarImageFilter} is used here, but any of the
  //  image segmentation filters could do it.
  //  The filter type is instantiated using the image type as template
  //  parameter.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   itk::SimpleFuzzyConnectednessScalarImageFilter< 
                 InputImageType, 
                 BinaryImageType >     FuzzySegmentationFilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The fuzzy connectedness segmentation filter is created by invoking the 
  //  \code{New()} method and assigning the result to a \code{SmartPointer}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FuzzySegmentationFilterType::Pointer fuzzysegmenter = 
                         FuzzySegmentationFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The fuzzy connectedness segmentation filter is created by invoking the 
  //  \code{New()} method and assigning the result to a \code{SmartPointer}.
  //
  //  Software Guide : EndLatex 
  fuzzysegmenter->SetInput( reader->GetOutput() );
  InputImageType::IndexType index;
  index.Fill(0);
  index[0] = atoi(argv[3]);
  index[1] = atoi(argv[4]);
  fuzzysegmenter->SetObjectsSeed(index);
  fuzzysegmenter->SetMean(atof(argv[5]));
  fuzzysegmenter->SetVariance(atof(argv[6]));
  fuzzysegmenter->Update();


  //  Software Guide : BeginLatex
  //  
  //  The second step of hybrid segmentation involved using the a prior generated
  //  from last step to build a homogeneity measurement for the object and 
  //  a \code{VoronoiSegmentationImageFilter} is applied based on this measurement
  //  to give the final segmentation result. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::VoronoiSegmentationImageFilter< 
                                    InputImageType, 
                                    OutputImageType > VoronoiSegmentationFilterType;
  // Software Guide : EndCodeSnippet

  VoronoiSegmentationFilterType::Pointer voronoisegmenter = 
                                 VoronoiSegmentationFilterType::New();
  
  voronoisegmenter->SetInput( reader->GetOutput() );
  voronoisegmenter->TakeAPrior( fuzzysegmenter->GetOutput());
  voronoisegmenter->Update();

  
  //  Software Guide : BeginLatex
  //
  //  The segmentation result was then write to the output file.
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer->SetInput( voronoisegmenter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet

  return 0;

}




