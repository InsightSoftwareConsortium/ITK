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
#include "itkMath.h"

#include "itkScalarImageToCooccurrenceMatrixFilter.h"

int itkScalarImageToCooccurrenceMatrixFilterTest(int, char* [] )
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

  imageIt.GoToBegin();

  for(unsigned int i = 0; i < 5; i++)
    {
    for(unsigned int j = 0; j < 5; j++, ++imageIt)
      {
      imageIt.Set(j % 2 + 1);
      }
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

  bool passed = true;

  try
    {

    typedef itk::Statistics::ScalarImageToCooccurrenceMatrixFilter< InputImageType> FilterType;

    FilterType::Pointer filter = FilterType::New();

    //Invoke update before adding an input. An exception should be
    //thrown.
    try
      {
      filter->Update();
      passed = false;
      std::cerr << "Failed to throw expected exception due to ITK_NULLPTR input: " << std::endl;
      return EXIT_FAILURE;
      }
    catch ( itk::ExceptionObject & excp )
      {
      std::cout << "Expected exception caught: " << excp << std::endl;
      }

    filter->ResetPipeline();

    if ( filter->GetInput() != ITK_NULLPTR )
      {
      std::cerr << "GetInput() should return ITK_NULLPTR since the input is\
                    not set yet " << std::endl;
      passed = false;
      }

    if ( filter->GetMaskImage() != ITK_NULLPTR )
      {
      std::cerr << "GetMaskImage() should return ITK_NULLPTR since the mask image is\
                    not set yet " << std::endl;
      passed = false;
      }


    //Invoke update with a ITK_NULLPTR input. An exception should be
    //thrown.
    filter->SetInput( ITK_NULLPTR );
    try
      {
      filter->Update();
      passed = false;
      std::cerr << "Failed to throw expected exception due to ITK_NULLPTR input: " << std::endl;
      return EXIT_FAILURE;
      }
    catch ( itk::ExceptionObject & excp )
      {
      std::cout << "Expected exception caught: " << excp << std::endl;
      }

    filter->ResetPipeline();

    if ( filter->GetInput() != ITK_NULLPTR )
      {
      passed = false;
      }


    filter->SetInput(image);

    InputImageType::OffsetType offset1 = {{0, 1}};
    InputImageType::OffsetType offset2 = {{1, 0}};
    FilterType::OffsetVectorPointer offsetV =
    FilterType::OffsetVector::New();
    offsetV->push_back(offset1);
    offsetV->push_back(offset2);

    filter->SetOffsets(offsetV);


    filter->Update();


    const FilterType::HistogramType * hist = filter->GetOutput();

    //--------------------------------------------------------------------------
    // Test the histogram.
    //--------------------------------------------------------------------------

    // First make sure the bins are sized properly:

    float max = hist->GetBinMax(0,255);
    float min = hist->GetBinMin(0,255);

    if(itk::Math::NotAlmostEquals(max, 256)
    || itk::Math::NotAlmostEquals(min, 255))
      {
      std::cerr << "Error" << std::endl;
      std::cerr << "The calculated bin sizes are incorrect" << std::endl;
      std::cerr << "Expected [255, 256), got [" << min << ", " << max << ")" << std::endl << std::endl;
      passed = false;
      }

    // Now make sure the contents of the bins are correct:
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

    //--------------------------------------------------------------------------
    // Test the histogram with normalization
    //--------------------------------------------------------------------------


    FilterType::Pointer filter0 = FilterType::New();

    filter0->SetInput(image);
    filter0->SetOffsets(offsetV);
    filter0->NormalizeOn();

    if ( filter0->GetNormalize() != true )
      {
      std::cerr << "Normalize boolean is not set correctly";
      passed = false;
      }

    filter0->Update();
    const FilterType::HistogramType * hist0 = filter0->GetOutput();

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
    FilterType::Pointer filter2 = FilterType::New();

    filter2->SetInput(image);
    InputImageType::OffsetType offset3 = {{0, 1}};

    filter2->SetOffset(offset3);
    filter2->SetNumberOfBinsPerAxis( 2 );

    if ( filter2->GetNumberOfBinsPerAxis() != 2 )
      {
      std::cerr << "GetNumberOfBinsPerAxis() is not returning the expected value" << std::endl;
      passed = false;
      }

    filter2->Update();
    const FilterType::HistogramType * hist2 = filter2->GetOutput();

    IndexType zero_zero( hist2->GetMeasurementVectorSize() );
    zero_zero[0] = 0;
    zero_zero[1] = 0;

    float zzF;
    zzF = hist2->GetFrequency(zero_zero);
    totalF = hist2->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(zzF, 40.0f) || itk::Math::NotAlmostEquals(zzF, totalF))
      {
      std::cerr << "Error:" << std::endl;
      std::cerr << "The degenerate histogram was calculated incorrectly" << std::endl;
      std::cerr << "Expected 40, 40 got " << zzF  << ", " << totalF << std::endl << std::endl;
      passed = false;
      }


    // Next a histogram with a smaller range.
    FilterType::Pointer filter3 = FilterType::New();

    filter3->SetInput(image);
    InputImageType::OffsetType offset4 = {{1, 1}};

    filter3->SetOffset(offset4);

    filter3->SetPixelValueMinMax(1, 2);

    if ( filter3->GetMin() != 1 )
      {
      std::cerr << "Error: " << std::endl;
      std::cerr << "GetMin() is not returning the value that is expected: 1" << std::endl;
      passed = false;
      }

    if ( filter3->GetMax() != 2 )
      {
      std::cerr << "Error: " << std::endl;
      std::cerr << "GetMin() is not returning the value that is expected: 2" << std::endl;
      passed = false;
      }


    filter3->SetNumberOfBinsPerAxis( 2 );

    filter3->Update();
    const FilterType::HistogramType * hist3 = filter3->GetOutput();

    IndexType zero_one( hist3->GetMeasurementVectorSize() );
    IndexType one_zero( hist3->GetMeasurementVectorSize() );

    zero_one[0] = 0;
    zero_one[1] = 1;

    one_zero[0] = 1;
    one_zero[1] = 0;

    float zoF, ozF;
    zzF = hist3->GetFrequency(zero_zero);
    zoF = hist3->GetFrequency(zero_one);
    ozF = hist3->GetFrequency(one_zero);
    ooF = hist3->GetFrequency(one_one);
    totalF = hist3->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(zzF, 0.0f)
     || itk::Math::NotAlmostEquals(zoF, 16.0f)
     || itk::Math::NotAlmostEquals(ozF, 16.0f)
     || itk::Math::NotAlmostEquals(ooF, 0.0f)
     || itk::Math::NotAlmostEquals(zzF + zoF + ozF + ooF, totalF) )
      {
      std::cerr << "Error:" << std::endl;
      std::cerr << "The small size histogram was calculated incorrectly" << std::endl;
      std::cerr << "Expected 0, 16, 16, 0, 32 got " << zzF << ", " << zoF  << ", " <<
      ozF  << ", " << ooF  << ", " << totalF << std::endl << std::endl;
      passed = false;
      }

    // Next a histogram with a truncated range.
    FilterType::Pointer filter4 = FilterType::New();

    filter4->SetInput(image);
    filter4->SetOffsets(offsetV);

    if ( filter4->GetOffsets() != offsetV )
      {
      std::cerr << "Error: " << std::endl;
      std::cerr << "GetOffsets() is not returning the expected offset vector: " << std::endl;
      passed = false;
      }

    filter4->SetPixelValueMinMax(0, 1);
    filter4->SetNumberOfBinsPerAxis( 2 );

    filter4->Update();
    const FilterType::HistogramType * hist4 = filter4->GetOutput();

    zzF = hist4->GetFrequency(zero_zero);
    zoF = hist4->GetFrequency(zero_one);
    ozF = hist4->GetFrequency(one_zero);
    ooF = hist4->GetFrequency(one_one);
    totalF = hist4->GetTotalFrequency();

    if( itk::Math::NotAlmostEquals(zzF, 0.0f)
     || itk::Math::NotAlmostEquals(zoF, 0.0f)
     || itk::Math::NotAlmostEquals(ozF, 0.0f)
     || itk::Math::NotAlmostEquals(ooF, 24.0f)
     || itk::Math::NotAlmostEquals(zzF + zoF + ozF + ooF, totalF) )
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

    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
}
