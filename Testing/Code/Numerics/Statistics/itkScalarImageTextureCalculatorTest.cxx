/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageTextureCalculatorTest.cxx
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
#include "vnl/vnl_math.h"

#include "itkScalarImageTextureCalculator.h"

// Un-comment to run this test standalone:
//int itkScalarImageTextureCalculatorTest(int, char* [] );
//int main(int c, char * v[])
//  {
//  return itkScalarImageTextureCalculatorTest(c, v);
//  }

int itkScalarImageTextureCalculatorTest(int, char* [] )
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
  // Test the calculator
  //--------------------------------------------------------------------------
  
  try {
  
  typedef itk::Statistics::ScalarImageTextureCalculator< 
    InputImageType> TextureCalcType;
  
  // First test: just use the defaults.
  TextureCalcType::Pointer texCalc = TextureCalcType::New();
  texCalc->SetInput(image);
  texCalc->Compute();
  TextureCalcType::FeatureValueVectorPointer means, stds;
  means = texCalc->GetFeatureMeans();
  stds = texCalc->GetFeatureStandardDeviations();
  
  double expectedMeans[6] = {0.238227, 3.15834, 0.394845, 3416.24, -731.492, 271393};
  double expectedDeviations[6] = {0.213011, 1.93978, 0.421773, 3415.98, 775.869, 272258};

  bool passed = true;
  TextureCalcType::FeatureValueVector::ConstIterator mIt, sIt;
  int counter;
  for (counter = 0, mIt = means->Begin(); mIt != means->End(); ++mIt, counter++)
    {
    if ( vnl_math_abs(1 - expectedMeans[counter] / mIt.Value()) > 0.0001 ) 
      // strange form of float comparison because large dynamic range of the means
      // makes a fixed "subtraction epsilon" infeasable. So we take a ratio epsilon
      // instead.
      {
      std::cout << "Error. Mean for feature " << counter << " is " << mIt.Value() <<
      ", expected " << expectedMeans[counter] << "." << std::endl;
      passed = false;
      }
    }

  for (counter = 0, sIt = stds->Begin(); sIt != stds->End(); ++sIt, counter ++)
    {
    if ( vnl_math_abs(1 - expectedDeviations[counter] / sIt.Value()) > 0.0001 )
      {
      std::cout << "Error. Deiviation for feature " << counter << " is " << sIt.Value() <<
      ", expected " << expectedDeviations[counter] << "." << std::endl;
      passed = false;
      }
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

