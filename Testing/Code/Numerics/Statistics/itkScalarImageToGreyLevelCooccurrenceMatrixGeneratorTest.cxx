/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest.cxx
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


#include "itkScalarImageToGreyLevelCooccurrenceMatrixGenerator.h"

// Un-comment to run this test standalone:
//int itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest(int, char* [] );
//int main(int c, char * v[])
//  {
//  return itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest(c, v);
//  }

int itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest(int, char* [] )
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

  for(int i = 0; i < 5; i++)
    for(int j = 0; j < 5; j++, ++imageIt)
      {
      imageIt.Set(j % 2 + 1);
      }

  
  //--------------------------------------------------------------------------
  // Generate the histogram. The un-normalized histogram should look like this:
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
  
  try {
  
  typedef itk::Statistics::ScalarImageToGreyLevelCooccurrenceMatrixGenerator< 
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
  glcmGen->Compute();
  GLCMGeneratorType::HistogramPointer hist = glcmGen->GetOutput();
  
  //--------------------------------------------------------------------------
  // Test the histogram.
  //--------------------------------------------------------------------------
  bool passed = true;
  
  // First make sure the bins are sized properly:
  
  float max = hist->GetBinMax(0,255);
  float min = hist->GetBinMin(0,255);
  
  if(max != 256 || min != 255)
    {
    std::cerr << "Error" << std::endl;
    std::cerr << "The calculated bin sizes are incorrect" << std::endl;
    std::cerr << "Expected [255, 256), got [" << min << ", " << max << ")" << std::endl << std::endl;
    passed = false;
    }
  
  // Now make sure the contents of the bins are correct:
  typedef GLCMGeneratorType::HistogramType::IndexType IndexType;
  IndexType one_one = {{1, 1}}, one_two= {{1, 2}}, two_one= {{2, 1}}, two_two= {{2, 2}};
  float ooF, otF, toF, ttF, totalF;
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
  
  //--------------------------------------------------------------------------
  // Test the histogram with normalization
  //--------------------------------------------------------------------------
  

  GLCMGeneratorType::Pointer glcmGen0 = GLCMGeneratorType::New();
  
  glcmGen0->SetInput(image);
  glcmGen0->SetOffsets(offsetV);
  glcmGen0->NormalizeOn();
  glcmGen0->Compute();
  GLCMGeneratorType::HistogramPointer hist0 = glcmGen0->GetOutput();

  ooF = hist0->GetFrequency(one_one);
  otF = hist0->GetFrequency(one_two);
  toF = hist0->GetFrequency(two_one);
  ttF = hist0->GetFrequency(two_two);
  
  if( (ooF - 24/80.) > 0.001 || (ttF - 16/80.) > 0.001 || (otF - 20/80.) > 0.001 || 
      (toF - 20/80.) > 0.001 || (ooF + ttF + otF + toF - 1) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 0.3, 0.2, 0.25, 0.25 got " << ooF << ", " << ttF  << ", " <<
    otF  << ", " << toF << std::endl << std::endl;
    passed = false;
    }
  
  
  //--------------------------------------------------------------------------
  // Generate some variant histograms and test them
  //--------------------------------------------------------------------------
  
  // First a histogram with 2 bins per axis
  GLCMGeneratorType::Pointer glcmGen2 = GLCMGeneratorType::New();
  
  glcmGen2->SetInput(image);
  InputImageType::OffsetType offset3 = {{0, 1}};

  glcmGen2->SetOffset(offset3);
  glcmGen2->SetNumberOfBinsPerAxis( 2 );
  glcmGen2->Compute();
  GLCMGeneratorType::HistogramPointer hist2 = glcmGen2->GetOutput();
  
  IndexType zero_zero = {{0, 0}};
  float zzF;
  zzF = hist2->GetFrequency(zero_zero);
  totalF = hist2->GetTotalFrequency();
  
  if( zzF != 40 || zzF != totalF)
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The degenerate histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 40, 40 got " << zzF  << ", " << totalF << std::endl << std::endl;
    passed = false;
    }
  
  
  // Next a histogram with a smaller range.
  GLCMGeneratorType::Pointer glcmGen3 = GLCMGeneratorType::New();
  
  glcmGen3->SetInput(image);
  InputImageType::OffsetType offset4 = {{1, 1}};
  
  glcmGen3->SetOffset(offset4);
  
  glcmGen3->SetPixelValueMinMax(1, 2);
  glcmGen3->SetNumberOfBinsPerAxis( 2 );

  glcmGen3->Compute();
  GLCMGeneratorType::HistogramPointer hist3 = glcmGen3->GetOutput();

  IndexType zero_one = {{0, 1}}, one_zero= {{1, 0}};
  float zoF, ozF;
  zzF = hist3->GetFrequency(zero_zero);
  zoF = hist3->GetFrequency(zero_one);
  ozF = hist3->GetFrequency(one_zero);
  ooF = hist3->GetFrequency(one_one);
  totalF = hist3->GetTotalFrequency();
  
  if( zzF != 0 || zoF != 16 || ozF != 16 || ooF != 0 || zzF + zoF + ozF + ooF != totalF)
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The small size histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 0, 16, 16, 0, 32 got " << zzF << ", " << zoF  << ", " <<
    ozF  << ", " << ooF  << ", " << totalF << std::endl << std::endl;
    passed = false;
    }

  // Next a histogram with a truncated range.
  GLCMGeneratorType::Pointer glcmGen4 = GLCMGeneratorType::New();
  
  glcmGen4->SetInput(image);
  glcmGen4->SetOffsets(offsetV);
  
  glcmGen4->SetPixelValueMinMax(0, 1);
  glcmGen4->SetNumberOfBinsPerAxis( 2 );
  
  glcmGen4->Compute();
  GLCMGeneratorType::HistogramPointer hist4 = glcmGen4->GetOutput();

  zzF = hist4->GetFrequency(zero_zero);
  zoF = hist4->GetFrequency(zero_one);
  ozF = hist4->GetFrequency(one_zero);
  ooF = hist4->GetFrequency(one_one);
  totalF = hist4->GetTotalFrequency();
  
  if( zzF != 0 || zoF != 0 || ozF != 0 || ooF != 24 || zzF + zoF + ozF + ooF != totalF)
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "The truncated range histogram was calculated incorrectly" << std::endl;
    std::cerr << "Expected 0, 0, 0, 24, 24 got " << zzF << ", " << zoF  << ", " <<
    ozF  << ", " << ooF  << ", " << totalF << std::endl << std::endl;
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

