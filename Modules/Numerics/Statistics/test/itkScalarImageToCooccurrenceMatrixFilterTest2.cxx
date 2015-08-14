/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// Insight classes
#include "itkImageRegionIterator.h"


#include "itkScalarImageToCooccurrenceMatrixFilter.h"

int itkScalarImageToCooccurrenceMatrixFilterTest2(int, char* [] )
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

  for(unsigned int i = 0; i < 5; i++)
    {
    for(unsigned int j = 0; j < 5; j++, ++imageIt)
      {
      imageIt.Set(j % 2 + 1);
      }
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

  try
    {

    typedef itk::Statistics::ScalarImageToCooccurrenceMatrixFilter<
      InputImageType> FilterType;

    FilterType::Pointer filter = FilterType::New();

    filter->SetInput(image);

    InputImageType::OffsetType offset1 = {{0, 1}};
    InputImageType::OffsetType offset2 = {{1, 0}};
    FilterType::OffsetVectorPointer offsetV =
    FilterType::OffsetVector::New();
    offsetV->push_back(offset1);
    offsetV->push_back(offset2);

    filter->SetOffsets(offsetV);
    filter->SetMaskImage(mask);
    filter->Update();
    const FilterType::HistogramType * hist = filter->GetOutput();

    //--------------------------------------------------------------------------
    // Test the histogram.
    //--------------------------------------------------------------------------
    bool passed = true;

    typedef FilterType::HistogramType::IndexType IndexType;
    IndexType one_one( hist->GetMeasurementVectorSize() );
    IndexType one_two( hist->GetMeasurementVectorSize() );
    IndexType two_one( hist->GetMeasurementVectorSize() );
    IndexType two_two( hist->GetMeasurementVectorSize() );

    one_one[0] = 1;
    one_one[1] = 1;

    one_two[0] = 1;
    one_two[1] = 2;

    two_one[0] = 2;
    two_one[1] = 1;

    two_two[0] = 2;
    two_two[1] = 2;

    float ooF, otF, toF, ttF, totalF;
    ooF = hist->GetFrequency(one_one);
    otF = hist->GetFrequency(one_two);
    toF = hist->GetFrequency(two_one);
    ttF = hist->GetFrequency(two_two);
    totalF = hist->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(ooF, 4.0f)
     || itk::Math::NotAlmostEquals(ttF, 0.0f)
     || itk::Math::NotAlmostEquals(otF, 0.0f)
     || itk::Math::NotAlmostEquals(toF, 0.0f)
     || itk::Math::NotAlmostEquals(ooF, totalF))
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
    filter = FilterType::New();

    filter->SetInput(image);
    filter->SetOffsets(offsetV);
    filter->SetMaskImage(mask);
    filter->SetInsidePixelValue(0);
    filter->Update();
    hist = filter->GetOutput();

    ooF = hist->GetFrequency(one_one);
    otF = hist->GetFrequency(one_two);
    toF = hist->GetFrequency(two_one);
    ttF = hist->GetFrequency(two_two);
    totalF = hist->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(ooF, 16.0f)
     || itk::Math::NotAlmostEquals(ttF, 16.0f)
     || itk::Math::NotAlmostEquals(otF, 14.0f)
     || itk::Math::NotAlmostEquals(toF, 14.0f)
     || itk::Math::NotAlmostEquals(ooF + ttF + otF + toF, totalF))
      {
      std::cerr << "Error:" << std::endl;
      std::cerr << "The histogram was calculated incorrectly" << std::endl;
      std::cerr << "Expected 16, 16, 14, 14, 64 got " << ooF << ", " << ttF  << ", " <<
      otF  << ", " << toF  << ", " << totalF << std::endl << std::endl;
      passed = false;
      }

    if ( filter->GetInsidePixelValue() != 0 )
      {
      std::cerr << "Error: " << std::endl;
      std::cerr << "GetInsidePixelValue() is not returning the expected value" << std::endl;
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

    filter = FilterType::New();

    filter->SetInput(image);
    filter->SetOffsets(offsetV);
    filter->Update();
    hist = filter->GetOutput();

    ooF = hist->GetFrequency(one_one);
    otF = hist->GetFrequency(one_two);
    toF = hist->GetFrequency(two_one);
    ttF = hist->GetFrequency(two_two);
    totalF = hist->GetTotalFrequency();


    if( itk::Math::NotAlmostEquals(ooF, 24.0f)
     || itk::Math::NotAlmostEquals(ttF, 16.0f)
     || itk::Math::NotAlmostEquals(otF, 20.0f)
     || itk::Math::NotAlmostEquals(toF, 20.0f)
     || itk::Math::NotAlmostEquals(ooF + ttF + otF + toF, totalF) )
      {
      std::cerr << "Error:" << std::endl;
      std::cerr << "The histogram was calculated incorrectly" << std::endl;
      std::cerr << "Expected 24, 16, 20, 20, 80 got " << ooF << ", " << ttF  << ", " <<
      otF  << ", " << toF  << ", " << totalF << std::endl << std::endl;
      passed = false;
      }

    FilterType::Pointer filter2 = FilterType::New();

    filter2->SetInput(image);
    filter2->SetMaskImage(mask);
    filter2->SetPixelValueMinMax(0,1);
    filter2->SetOffsets(offsetV);
    filter2->Update();
    hist = filter2->GetOutput();

    ooF = hist->GetFrequency(one_one);
    otF = hist->GetFrequency(one_two);
    toF = hist->GetFrequency(two_one);
    ttF = hist->GetFrequency(two_two);
    totalF = hist->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(ooF, 0.0f)
     || itk::Math::NotAlmostEquals(ttF, 0.0f)
     || itk::Math::NotAlmostEquals(otF, 0.0f)
     || itk::Math::NotAlmostEquals(toF, 0.0f)
     || itk::Math::NotAlmostEquals(totalF, 4.0f))
      {
      std::cerr << "Error:" << std::endl;
      std::cerr << "The histogram was calculated incorrectly" << std::endl;
      std::cerr << "Expected 0, 0, 0, 0, 4 got " << ooF << ", " << ttF  << ", " <<
      otF  << ", " << toF  << ", " << totalF << std::endl << std::endl;
      passed = false;
      }

    // fill the mask buffer to one
    mask->FillBuffer( 1 );
    filter2->SetInsidePixelValue( 1 );
    filter2->SetPixelValueMinMax(0,1);
    filter2->Update();

    ooF = hist->GetFrequency(one_one);
    otF = hist->GetFrequency(one_two);
    toF = hist->GetFrequency(two_one);
    ttF = hist->GetFrequency(two_two);
    totalF = hist->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(ooF, 0.0f)
     || itk::Math::NotAlmostEquals(ttF, 0.0f)
     || itk::Math::NotAlmostEquals(otF, 0.0f)
     || itk::Math::NotAlmostEquals(toF, 0.0f)
     || itk::Math::NotAlmostEquals(totalF, 24.0f))
      {
      std::cerr << "Error:" << std::endl;
      std::cerr << "The histogram was calculated incorrectly" << std::endl;
      std::cerr << "Expected 0, 0, 0, 0, 24 got " << ooF << ", " << ttF  << ", " <<
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

    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
}
