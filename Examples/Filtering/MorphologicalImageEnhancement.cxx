/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Language:  C++

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  This example illustrates the use of Mathematical Morphology filters for
//  image enhancement. One of the difficulties of image enhancement is that it
//  is defined based on human visual perception and it is related to a
//  particular set of features that are of interest in the image. In this
//  context, what is considered enhancement for one person, may be seen as
//  image degradation by another person.
//  
//  \index{itk::AntiAliasBinaryImageFilter|textbf}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h" 

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

#include "itkRescaleIntensityImageFilter.h"

int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile radius " << std::endl;
    return 1;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;
  
  typedef unsigned char   InputPixelType;
  typedef unsigned char   OutputPixelType;
  typedef unsigned char   WritePixelType;
  typedef signed int      InternalPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >    WriteImageType;
  typedef itk::Image< InternalPixelType, Dimension>  InternalImageType;

  typedef itk::ImageRegionConstIterator< OutputImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< InternalImageType>       IteratorType;

  // readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  // structuring element
  typedef itk::BinaryBallStructuringElement< 
                    InputPixelType,
                    Dimension  >             StructuringElementType;

// define the erosion and dilation types
    typedef itk::GrayscaleErodeImageFilter<
                            InputImageType, 
                            OutputImageType,
                            StructuringElementType >  ErodeFilterType;

  typedef itk::GrayscaleDilateImageFilter<
                            InputImageType, 
                            OutputImageType, 
                            StructuringElementType >  DilateFilterType;

  // define rescaling filter
  typedef itk::RescaleIntensityImageFilter<
                          InternalImageType,
              WriteImageType>    RescaleFilterType;

  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  
  // Creation of rescale filter
  RescaleFilterType::Pointer rescaleFilter = RescaleFilterType::New();

  // Create the erosion and dilation filters
  ErodeFilterType::Pointer  grayscaleErodeOpening  = ErodeFilterType::New();
  ErodeFilterType::Pointer  grayscaleErodeClosing  = ErodeFilterType::New();
  DilateFilterType::Pointer grayscaleDilateOpening = DilateFilterType::New();
  DilateFilterType::Pointer grayscaleDilateClosing = DilateFilterType::New();

  // Create structuring element
  StructuringElementType  structuringElement;

  structuringElement.SetRadius( atoi(argv[3]) );  // argv[3]xargv[3] structuring element

  structuringElement.CreateStructuringElement();

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );

  // reading input image
  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
  std::cout<<"Problems reading input image"<<std::endl;
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    }
  
  // Setup the erosion and dilation methods
  grayscaleErodeOpening->SetKernel(  structuringElement );
  grayscaleErodeClosing->SetKernel(  structuringElement );
  grayscaleDilateOpening->SetKernel( structuringElement );
  grayscaleDilateClosing->SetKernel( structuringElement );

  // morphological opening
  grayscaleErodeOpening->SetInput(  reader->GetOutput() );
  grayscaleDilateOpening->SetInput( grayscaleErodeOpening->GetOutput() );
  
  try
    {
    grayscaleDilateOpening->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
  std::cout<<"Problems applying morphological opening"<<std::endl;
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    }

  // morphological closing
  grayscaleDilateClosing->SetInput(  reader->GetOutput() );
  grayscaleErodeClosing->SetInput(   grayscaleDilateClosing->GetOutput() );

  try
    {
    grayscaleErodeClosing->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
  std::cout<<"Problems applying morphological closing"<<std::endl;
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    }

   // Setup internal image of the same size as input image
  InternalImageType::Pointer internalImage = InternalImageType::New();
  internalImage->SetRegions( reader->GetOutput()->GetRequestedRegion() );
  internalImage->CopyInformation( reader->GetOutput() );
  internalImage->Allocate();

  ConstIteratorType originalImageIt(   reader->GetOutput(), reader->GetOutput()->GetRequestedRegion()  );
  ConstIteratorType openingImageIt(    grayscaleDilateOpening->GetOutput(), reader->GetOutput()->GetRequestedRegion()  );
  ConstIteratorType closingImageIt(    grayscaleErodeClosing->GetOutput(),  reader->GetOutput()->GetRequestedRegion()  );
  
  IteratorType      outputIt(  internalImage,  internalImage->GetRequestedRegion() );

  for ( originalImageIt.GoToBegin(), openingImageIt.GoToBegin(), 
    closingImageIt.GoToBegin(), outputIt.GoToBegin(); !originalImageIt.IsAtEnd();
        ++originalImageIt, ++openingImageIt, ++closingImageIt, ++outputIt)
    {
    // top hat
    signed int topHat = originalImageIt.Get() - openingImageIt.Get();
    if (topHat<0)
    {
      topHat = 0;
    }
    // bottom hat
    signed int bottomHat = closingImageIt.Get() - originalImageIt.Get();
    if (bottomHat<0)
    {
      bottomHat = 0;
    }
    // adding original image to top hat
    signed int addOriginalImageToTopHat = originalImageIt.Get() + topHat;
    if (addOriginalImageToTopHat>255)
    {
      addOriginalImageToTopHat = 255;
    }

    // substracting oi+th-bh
    signed int value = addOriginalImageToTopHat - bottomHat;
    if (value<0)
    {
      value = 0;
    }
    outputIt.Set( value  );
    }


  // rescaling
  rescaleFilter->SetInput( internalImage );
  rescaleFilter->SetOutputMinimum( 0 );
  rescaleFilter->SetOutputMaximum( 255 );

  // writing output

  writer->SetInput( rescaleFilter->GetOutput() );
  
  try
  {
      writer->Update();
  }
  catch( itk::ExceptionObject & err )
  {
    std::cout<<"ExceptionObject caught !"<<std::endl;
    std::cout<< err <<std::endl;
    return -1;
  }
  
  return 0;

// Software Guide : EndCodeSnippet
}

