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

#include "itkScalarImageToRunLengthFeaturesFilter.h"

int itkScalarImageToRunLengthFeaturesFilterTest(int, char* [] )
{

  //Data definitions
  const unsigned int  IMGWIDTH         =  5;
  const unsigned int  IMGHEIGHT        =  5;
  const unsigned int  NDIMENSION       =  2;


  //------------------------------------------------------
  //Create a simple test images
  //------------------------------------------------------
  typedef itk::Image<uint8_t, NDIMENSION> InputImageType;

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
  // Set up the mask next. It looks like:
  //  1 1 1 1 1
  //  1 1 1 1 1
  //  1 1 1 1 1
  //  1 1 1 1 1
  //  1 1 1 1 1
  //--------------------------------------------------------------------------

  InputImageType::Pointer mask = InputImageType::New();
  mask->SetRegions( region );
  mask->Allocate();

  // setup the iterator
  InputImageIterator maskIt( mask, mask->GetBufferedRegion() );
  maskIt.GoToBegin();
  for(int i = 0; i < 5; i++)
    for(int j = 0; j < 5; j++, ++maskIt)
      {
      maskIt.Set(1);
      }

  //--------------------------------------------------------------------------
  // Test the texFilter
  //--------------------------------------------------------------------------
  bool passed = true;

  try
    {

    typedef itk::Statistics::ScalarImageToRunLengthFeaturesFilter<
      InputImageType> RunLengthFilterType;

    // First test: just use the defaults.
    RunLengthFilterType::Pointer texFilter = RunLengthFilterType::New();

    //Invoke update before adding an input. An exception should be
    //thrown.
    try
      {
      texFilter->Update();
      passed = false;
      std::cerr << "Failed to throw expected exception due to NULL input: " << std::endl;
      return EXIT_FAILURE;
      }
    catch ( itk::ExceptionObject & excp )
      {
      std::cout << "Expected exception caught: " << excp << std::endl;
      }

    texFilter->ResetPipeline();

    if ( texFilter->GetInput() != NULL )
      {
      std::cerr << "GetInput() should return NULL since the input is "
                << " not set yet " << std::endl;
      passed = false;
      }

    if ( texFilter->GetMaskImage() != NULL )
      {
      std::cerr << "GetMaskImage() should return NULL since the mask image is "
                << "not set yet " << std::endl;
      passed = false;
      }

    //Invoke update with a NULL input. An exception should be
    //thrown.
    texFilter->SetInput( NULL );
    try
      {
      texFilter->Update();
      passed = false;
      std::cerr << "Failed to throw expected exception due to NULL input: " << std::endl;
      return EXIT_FAILURE;
      }
    catch ( itk::ExceptionObject & excp )
      {
      std::cout << "Expected exception caught: " << excp << std::endl;
      }

    texFilter->ResetPipeline();

    if ( texFilter->GetInput() != NULL )
      {
      passed = false;
      }

     //Test the Use_PixelContainer boolean
    texFilter->SetFastCalculations( false );
    if ( texFilter->GetFastCalculations() != false )
      {
      std::cerr << "Error in Set/Get FastCalculations methods" << std::endl;
      return EXIT_FAILURE;
      }

    texFilter->FastCalculationsOn(  );
    if ( texFilter->GetFastCalculations() != true )
      {
      std::cerr << "Error in Set/Get FastCalculationsOn method" << std::endl;
      return EXIT_FAILURE;
      }

    texFilter->FastCalculationsOff();
    texFilter->SetInput( image );
    texFilter->SetPixelValueMinMax( 0, 2 );
    texFilter->SetDistanceValueMinMax( 0, 5.0 );
    texFilter->SetNumberOfBinsPerAxis( 5 );
    texFilter->SetMaskImage( mask );
    texFilter->Update();

    texFilter->Print( std::cout );

    //Test GetInput
    if ( texFilter->GetInput() != image )
      {
      std::cerr << "Error in GetInput() method " << std::endl;
      passed = false;
      }

    //Test GetMaskImage
    if ( texFilter->GetMaskImage() != mask )
      {
      std::cerr << "Error in GetMaskImage() method " << std::endl;
      passed = false;
      }

    RunLengthFilterType::FeatureValueVectorPointer means, stds;
    means = texFilter->GetFeatureMeans();
    stds = texFilter->GetFeatureStandardDeviations();

    double expectedMeans[10] = { 0.556, 5.8, 10.4, 10.4, 0.466667, 5.8, 0.150667,
        4.204, 2.06667, 39.4 };
    double expectedDeviations[10] = { 0.249415, 6.23538, 4.50333, 4.50333, 0, 0, 0.0277128, 2.24474,
        0.69282, 56.1184 };
    RunLengthFilterType::FeatureValueVector::ConstIterator mIt;
    RunLengthFilterType::FeatureValueVector::ConstIterator sIt;

    int counter;
    for (counter = 0, mIt = means->Begin(); mIt != means->End(); ++mIt, counter++)
      {
      if ( vnl_math_abs(expectedMeans[counter] - mIt.Value()) > 0.0001 )
        {
        std::cerr << "Error. Mean for feature " << counter << " is " << mIt.Value() <<
        ", expected " << expectedMeans[counter] << "." << std::endl;
        passed = false;
        }
      }

    for (counter = 0, sIt = stds->Begin(); sIt != stds->End(); ++sIt, counter ++)
      {
      if ( vnl_math_abs(expectedDeviations[counter] - sIt.Value()) > 0.0001 )
        {
        std::cerr << "Error. Deviation for feature " << counter << " is " << sIt.Value() <<
        ", expected " << expectedDeviations[counter] << "." << std::endl;
        passed = false;
        }
      }

    //Rerun the feature generation with fast calculations on
    texFilter->FastCalculationsOn();
    texFilter->Update();
    means = texFilter->GetFeatureMeans();
    stds = texFilter->GetFeatureStandardDeviations();

    double expectedMeans2[10] = { 0.7, 2.2, 13, 13, 0.466667, 5.8, 0.166667,
        5.5, 1.66667, 7 };
    double expectedDeviations2[10] = { 0 };

    for (counter = 0, mIt = means->Begin(); mIt != means->End(); ++mIt, counter++)
      {
      if ( vnl_math_abs(expectedMeans2[counter] - mIt.Value()) > 0.0001 )
        {
        std::cerr << "Error2. Mean for feature " << counter << " is "
          << mIt.Value() << ", expected " << expectedMeans2[counter] <<
          "." << std::endl;
        passed = false;
        }
      }

    for (counter = 0, sIt = stds->Begin(); sIt != stds->End(); ++sIt, counter ++)
      {
      if ( vnl_math_abs( expectedDeviations2[counter] - sIt.Value() ) > 0.0001 )
        {
        std::cerr << "Error2. Deviation for feature " << counter << " is "
          << sIt.Value() << ", expected " << expectedDeviations2[counter]
          << "." << std::endl;
        passed = false;
        }
      }

//    //Rerun the feature generation setting an offset
    RunLengthFilterType::OffsetType   offset;
    offset[0] = -1;
    offset[1] =  -1;

    RunLengthFilterType::OffsetVectorPointer   offsets;

    offsets = RunLengthFilterType::OffsetVector::New();

    offsets->push_back( offset );
    texFilter->SetOffsets( offsets );

    const RunLengthFilterType::OffsetVector* offsets2 = texFilter->GetOffsets();

    RunLengthFilterType::OffsetVector::ConstIterator vIt;
    RunLengthFilterType::OffsetVector::ConstIterator vIt2;

    for (vIt = offsets->Begin(), vIt2 = offsets2->Begin(); vIt != offsets->End();
      ++vIt,++vIt2)
      {
      if ( vIt.Value() != vIt2.Value() )
        {
        std::cerr << "Offsets not properly set" << std::endl;
        passed = false;
        }
      }

    texFilter->Update();

    if ( texFilter->GetMaskImage() == NULL )
      {
      std::cerr << "Error: " << std::endl;
      std::cerr << "Mask should not be null." << std::endl;
      passed = false;
      }

    means = texFilter->GetFeatureMeans();
    stds = texFilter->GetFeatureStandardDeviations();

    double expectedMeans3[10] = { 0.7, 2.2, 13, 13, 0.466667, 5.8, 0.166667,
        5.5, 1.66667, 7 };
    double expectedDeviations3[10] = { 0 };

    for (counter = 0, mIt = means->Begin(); mIt != means->End(); ++mIt, counter++)
      {
      if ( vnl_math_abs(expectedMeans3[counter] - mIt.Value()) > 0.0001 )
        {
        std::cerr << "Error3. Mean for feature " << counter << " is " << mIt.Value() <<
        ", expected " << expectedMeans3[counter] << "." << std::endl;
        passed = false;
        }
      }

    for (counter = 0, sIt = stds->Begin(); sIt != stds->End(); ++sIt, counter ++)
      {
      if ( vnl_math_abs(expectedDeviations3[counter] - sIt.Value() ) > 0.0001 )
        {
        std::cerr << "Error3. Deviation for feature " << counter << " is " << sIt.Value() <<
        ", expected " << expectedDeviations3[counter] << "." << std::endl;
        passed = false;
        }
      }

    //Test Set/Get Requested features
    typedef RunLengthFilterType::RunLengthFeaturesFilterType   RunLengthFeaturesFilterType;

    RunLengthFilterType::FeatureNameVectorPointer requestedFeatures =
                                                RunLengthFilterType::FeatureNameVector::New();

    requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunEmphasis);
    requestedFeatures->push_back(RunLengthFeaturesFilterType::GreyLevelNonuniformity);
    texFilter->SetRequestedFeatures(requestedFeatures);

    texFilter->Print( std::cout, 3 );

    const RunLengthFilterType::FeatureNameVector* requestedFeatures2 =
                                                texFilter->GetRequestedFeatures();

    RunLengthFilterType::FeatureNameVector::ConstIterator fIt;

    fIt = requestedFeatures2->Begin();
    if ( fIt.Value() != RunLengthFeaturesFilterType::ShortRunEmphasis )
      {
      std::cerr << "Requested feature name not correctly set" << std::endl;
      passed = false;
      }
    fIt++;

    if ( fIt.Value() != RunLengthFeaturesFilterType::GreyLevelNonuniformity)
      {
      std::cerr << "Requested feature name not correctly set" << std::endl;
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
