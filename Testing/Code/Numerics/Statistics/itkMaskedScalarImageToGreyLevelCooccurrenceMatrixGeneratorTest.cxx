/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
// Insight classes
#include "itkImage.h"
#include "itkImageRegionIterator.h"


#include "itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator.h"

// Un-comment to run this test standalone:
//int itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest(int, char* [] );
//int main(int c, char * v[])
//  {
//  return itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest(c, v);
//  }

int itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest(int, char* [] )
{

  //Data definitions 
  const unsigned int  IMGWIDTH         =  5;
  const unsigned int  IMGHEIGHT        =  5;
  const unsigned int  NDIMENSION       =  2;


  //------------------------------------------------------
  //Create a simple test images
  //------------------------------------------------------
  typedef itk::Image<unsigned char, NDIMENSION> InputImageType;

  typedef itk::ImageRegionIterator< InputImageType > InputImageIterator;

   
  InputImageType::Pointer image = InputImageType::New();
  InputImageType::Pointer mask = InputImageType::New();

  
  InputImageType::SizeType inputImageSize = {{ IMGWIDTH, IMGHEIGHT }};

  InputImageType::IndexType index;
  index.Fill(0);
  InputImageType::RegionType region;

  region.SetSize( inputImageSize );
  region.SetIndex( index );

  //--------------------------------------------------------------------------
  // Set up the image first. It looks like:
  //  1 2 1 2 1
  //  1 2 1 2 1
  //  1 2 1 2 1
  //  1 2 1 2 1
  //  1 2 1 2 1
  //--------------------------------------------------------------------------

  image->SetRegions( region );
  image->Allocate();

  // setup the iterator
  InputImageIterator imageIt( image, image->GetBufferedRegion() );
  imageIt.GoToBegin();
  for(int i = 0; i < 5; i++)
    for(int j = 0; j < 5; j++, ++imageIt)
      {
      imageIt.Set(j % 2 + 1);
      }

  //--------------------------------------------------------------------------
  // Set up the mask next. It looks like:
  //  0 0 0 0 0
  //  0 0 1 0 0
  //  0 0 1 0 0
  //  0 0 1 0 0
  //  0 0 0 0 0
  //--------------------------------------------------------------------------
      
  mask->SetRegions( region );
  mask->Allocate();
  
  // setup the iterator
  InputImageIterator maskIt( mask, mask->GetBufferedRegion() );
  maskIt.GoToBegin();
  for(int i = 0; i < 5; i++)
    for(int j = 0; j < 5; j++, ++maskIt)
      {
      if (j == 2 && i > 0 && i < 4)
        {
        maskIt.Set(1);
        }
      else
        {
        maskIt.Set(0);
        }
      }
  
  //--------------------------------------------------------------------------
  // Generate the histogram. It should look like this:
  // 
  //     0 1 2 ...
  //     -----
  //  0 |0 0 0
  //  1 |0 4 0
  //  2 |0 0 0
  //  .
  //  .
  //  .
  // with zeroes elsewhere.
  //--------------------------------------------------------------------------
  
  try {
  
  typedef itk::Statistics::MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator< 
    InputImageType> GLCMGeneratorType;
  
  GLCMGeneratorType::Pointer glcmGen = GLCMGeneratorType::New();
  
  glcmGen->SetInput(image);
  InputImageType::OffsetType offset1 = {{0, 1}};
  InputImageType::OffsetType offset2 = {{1, 0}};
  GLCMGeneratorType::OffsetVectorPointer offsetV = 
  GLCMGeneratorType::OffsetVector::New();
  offsetV->push_back(offset1);
  offsetV->push_back(offset2);
  
  glcmGen->SetOffsets(offsetV);
  glcmGen->SetImageMask(mask);
  glcmGen->Compute();
  GLCMGeneratorType::HistogramPointer hist = glcmGen->GetOutput();
  
  //--------------------------------------------------------------------------
  // Test the histogram.
  //--------------------------------------------------------------------------
  bool passed = true;

  typedef GLCMGeneratorType::HistogramType::IndexType IndexType;
  IndexType one_one = {{1, 1}}, one_two= {{1, 2}}, two_one= {{2, 1}}, two_two= {{2, 2}};
  float ooF, otF, toF, ttF, totalF;
  ooF = hist->GetFrequency(one_one);
  otF = hist->GetFrequency(one_two);
  toF = hist->GetFrequency(two_one);
  ttF = hist->GetFrequency(two_two);
  totalF = hist->GetTotalFrequency();
  
  if( ooF != 4 || ttF != 0 || otF != 0 || toF != 0 || ooF != totalF)
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 4, 0, 0, 0, 4 got " << ooF << ", " << ttF  << ", " <<
    otF  << ", " << toF  << ", " << totalF << std::endl << std::endl;
    passed = false;
    }
  
  //--------------------------------------------------------------------------
  // Test the histogram with "0" as the "inside" value instead of "1"
  // It should look like this:
  // 
  //     0 1  2 ...
  //     ------
  //  0 |0 0  0
  //  1 |0 18 14
  //  2 |0 14 18
  //  .
  //  .
  //  .
  // with zeroes elsewhere.
  //--------------------------------------------------------------------------
  glcmGen = GLCMGeneratorType::New();
  
  glcmGen->SetInput(image);  
  glcmGen->SetOffsets(offsetV);
  glcmGen->SetImageMask(mask);
  glcmGen->SetInsidePixelValue(0);
  glcmGen->Compute();
  hist = glcmGen->GetOutput();
  
  ooF = hist->GetFrequency(one_one);
  otF = hist->GetFrequency(one_two);
  toF = hist->GetFrequency(two_one);
  ttF = hist->GetFrequency(two_two);
  totalF = hist->GetTotalFrequency();
  
  if( ooF != 16 || ttF != 16 || otF != 14 || toF != 14 || ooF + ttF + otF + toF != totalF)
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 16, 16, 14, 14, 64 got " << ooF << ", " << ttF  << ", " <<
    otF  << ", " << toF  << ", " << totalF << std::endl << std::endl;
    passed = false;
    }
  
  
  //--------------------------------------------------------------------------
  // Generate the histogram with no mask. The un-normalized, un-masked histogram
  // should look like this:
  // 
  //     0 1  2 ...
  //     ------
  //  0 |0 0  0
  //  1 |0 24 20
  //  2 |0 20 16
  //  3 |0 0  0
  //  .
  //  .
  // with zeroes elsewhere.
  //-------------------------------------------------------------------------- 
  
  glcmGen = GLCMGeneratorType::New();
  
  glcmGen->SetInput(image);  
  glcmGen->SetOffsets(offsetV);
  glcmGen->Compute();
  hist = glcmGen->GetOutput();
  
  ooF = hist->GetFrequency(one_one);
  otF = hist->GetFrequency(one_two);
  toF = hist->GetFrequency(two_one);
  ttF = hist->GetFrequency(two_two);
  totalF = hist->GetTotalFrequency();
  
  
  if( ooF != 24 || ttF != 16 || otF != 20 || toF != 20 || ooF + ttF + otF + toF != totalF)
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 24, 16, 20, 20, 80 got " << ooF << ", " << ttF  << ", " <<
    otF  << ", " << toF  << ", " << totalF << std::endl << std::endl;
    passed = false;
    }
  
  
  
  
  if (!passed)
    {
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "Test succeeded" << std::endl;
    return EXIT_SUCCESS;
    }
  
  } catch( itk::ExceptionObject & err ) { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }
}

