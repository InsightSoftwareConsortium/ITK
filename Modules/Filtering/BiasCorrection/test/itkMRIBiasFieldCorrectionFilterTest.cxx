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

#include <iostream>

#include "itkMRIBiasFieldCorrectionFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageFileWriter.h"
#include "itkSphereSpatialFunction.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkTestingMacros.h"

int itkMRIBiasFieldCorrectionFilterTest( int , char* [] )
{
  const unsigned int ImageDimension = 3;

  typedef float InputImagePixelType;

  typedef itk::Image< InputImagePixelType, ImageDimension >            ImageType;
  typedef itk::Image< InputImagePixelType, ImageDimension >            MaskType;
  typedef itk::ImageRegionIteratorWithIndex< ImageType > ImageIteratorType;

  bool SaveImages = false;
  ImageType::SizeType   imageSize;
  ImageType::IndexType  imageIndex;
  ImageType::RegionType imageRegion;
  imageSize[0] = 30;
  imageSize[1] = 30;
  imageSize[2] = 15;
  std::cout << "Random Test image size: " << imageSize[0] << "x"
       << imageSize[1] << "x" << imageSize[2] << std::endl;

  imageIndex.Fill( 0 );
  float spacing[ImageDimension] = {1.0, 1.0, 1.0};
  float origin[ImageDimension] = {0, 0, 0};

  imageRegion.SetSize( imageSize );
  imageRegion.SetIndex( imageIndex );

  // creates an image that will stores the Gaussian pixel * bias values
  ImageType::Pointer imageWithBias = ImageType::New();
  imageWithBias->SetBufferedRegion( imageRegion );
  imageWithBias->SetLargestPossibleRegion( imageRegion );
  imageWithBias->SetSpacing( spacing );
  imageWithBias->SetOrigin( origin );
  imageWithBias->Allocate();

  // creates the image source with a sphere.
  ImageType::Pointer image = ImageType::New();
  image->SetBufferedRegion( imageRegion );
  image->SetLargestPossibleRegion( imageRegion );
  image->SetSpacing( spacing );
  image->SetOrigin( origin );
  image->Allocate();

  // creates an image for bias
  ImageType::Pointer biasImage = ImageType::New();
  biasImage->SetBufferedRegion( imageRegion );
  biasImage->SetLargestPossibleRegion( imageRegion );
  biasImage->SetSpacing( spacing );
  biasImage->SetOrigin( origin );
  biasImage->Allocate();

  // class statistics for two classes: a bright sphere and background
  itk::Array<double> classMeans(2);
  itk::Array<double> classSigmas(2);

  classMeans[0] = 10.0;
  classMeans[1] = 200.0;

  classSigmas[0] = 10.0;
  classSigmas[1] = 20.0;

  // creats a normal random variate generator
  itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer randomGenerator =
    itk::Statistics::MersenneTwisterRandomVariateGenerator::New();

  // fills the image with a sphere filled with intensity values from a
  // normal distribution.
  typedef itk::SphereSpatialFunction<ImageDimension> SphereType;
  SphereType::Pointer sphere = SphereType::New();

  SphereType::InputType center;
  center[0] = imageSize[0]/2;
  center[1] = imageSize[1]/2;
  center[2] = imageSize[2]/2;
  sphere->SetCenter( center );
  sphere->SetRadius( 5.0 );

  randomGenerator->SetSeed ( 2003 );
  ImageIteratorType i_iter( image , imageRegion );
  SphereType::InputType point;
  while ( !i_iter.IsAtEnd() )
    {
    image->TransformIndexToPhysicalPoint( i_iter.GetIndex() , point );
    if ( sphere->Evaluate( point ) == 1 ) // inside or on surface
      {
      i_iter.Set( randomGenerator->GetNormalVariate() * classSigmas[1]
                  + classMeans[1] );
      }
    else
      {
      i_iter.Set( classMeans[0] );
      }
    ++i_iter;
    }

  // creates a bias field
  typedef itk::MultivariateLegendrePolynomial BiasFieldType;
  BiasFieldType::DomainSizeType biasSize(3);
  int biasDegree = 3;
  biasSize[0] = imageSize[0];
  biasSize[1] = imageSize[1];
  biasSize[2] = imageSize[2];
  BiasFieldType bias(static_cast<unsigned int> ( biasSize.size() ),
                     biasDegree, // bias field degree
                     biasSize);

  // generates the coefficients using the normal random variate generator.
  BiasFieldType::CoefficientArrayType
    coefficients(bias.GetNumberOfCoefficients());
  BiasFieldType::CoefficientArrayType
    initCoefficients(bias.GetNumberOfCoefficients());

  randomGenerator->SetSeed( 2003 );
  for ( unsigned int i = 0; i < bias.GetNumberOfCoefficients(); ++i )
    {
    coefficients[i] = ( randomGenerator->GetNormalVariate() + 1 ) * 0.1;
    initCoefficients[i] = 0;
    }
  bias.SetCoefficients(coefficients);

  // set the imageWithBias pixel values with imageSource pixel value +
  // bias.
  ImageIteratorType ib_iter( imageWithBias,
                             imageWithBias->GetLargestPossibleRegion() );

  BiasFieldType::SimpleForwardIterator b_iter( &bias );
  i_iter.GoToBegin();
  ib_iter.GoToBegin();
  float temp;
  while ( !i_iter.IsAtEnd() )
    {
    temp = i_iter.Get() * (2 + b_iter.Get()); // this is a multiplicative bias field
    ib_iter.Set( temp );
    ++i_iter;
    ++ib_iter;
    ++b_iter;
    }

  // creates a bias correction filter and run it.
  typedef itk::MRIBiasFieldCorrectionFilter<ImageType, ImageType, MaskType>
    FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, MRIBiasFieldCorrectionFilter, ImageToImageFilter );

  // To see the debug output for each iteration, uncomment the
  // following line.
  // filter->DebugOn();

  double sumOfError = 0.0;
  i_iter.GoToBegin();
  ib_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += itk::Math::abs( ib_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++ib_iter;
    }
  std::cout << "Absolute Avg. error before correction = "
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  double origSumError = sumOfError;

  std::cout << "Computing bias correction without mask, 2 classes 10,10 - 200,20" << std::endl;

  filter->SetInput( imageWithBias.GetPointer() );

  int slicingDirection = 2;
  bool isBiasFieldMultiplicative = true;
  bool usingSlabIdentification = true;
  bool usingBiasFieldCorrection = true;
  bool generatingOutput = true;
  unsigned int slabNumberOfSamples = 10;
  InputImagePixelType slabBackgroundMinimumThreshold = 0;
  double slabTolerance = 0.0;
  int volumeCorrectionMaximumIteration = 200;
  int interSliceCorrectionMaximumIteration = 100;
  double optimizerInitialRadius = 0.02;
  double optimizerGrowthFactor = 1.01;
  bool usingInterSliceIntensityCorrection = true;

  filter->SetBiasFieldMultiplicative( isBiasFieldMultiplicative );
  filter->SetUsingSlabIdentification( usingSlabIdentification );
  filter->SetUsingBiasFieldCorrection( usingBiasFieldCorrection );
  filter->SetGeneratingOutput( generatingOutput );
  filter->SetSlabNumberOfSamples( slabNumberOfSamples );
  filter->SetSlabBackgroundMinimumThreshold( slabBackgroundMinimumThreshold );
  filter->SetSlabTolerance( slabTolerance );
  filter->SetBiasFieldDegree( biasDegree );
  filter->SetVolumeCorrectionMaximumIteration( volumeCorrectionMaximumIteration );
  filter->SetInterSliceCorrectionMaximumIteration( interSliceCorrectionMaximumIteration );
  filter->SetOptimizerInitialRadius( optimizerInitialRadius );
  filter->SetOptimizerGrowthFactor( optimizerGrowthFactor );
  filter->SetUsingInterSliceIntensityCorrection( usingInterSliceIntensityCorrection );

  TEST_SET_GET_VALUE( isBiasFieldMultiplicative, filter->GetBiasFieldMultiplicative() );
  TEST_SET_GET_VALUE( usingSlabIdentification, filter->GetUsingSlabIdentification() );
  TEST_SET_GET_VALUE( usingBiasFieldCorrection, filter->GetUsingBiasFieldCorrection() );
  TEST_SET_GET_VALUE( generatingOutput, filter->GetGeneratingOutput() );
  TEST_SET_GET_VALUE( slabNumberOfSamples, filter->GetSlabNumberOfSamples() );
  TEST_SET_GET_VALUE( slabBackgroundMinimumThreshold, filter->GetSlabBackgroundMinimumThreshold() );
  TEST_SET_GET_VALUE( slabTolerance, filter->GetSlabTolerance() );
  TEST_SET_GET_VALUE( biasDegree, filter->GetBiasFieldDegree() );
  TEST_SET_GET_VALUE( volumeCorrectionMaximumIteration, filter->GetVolumeCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( interSliceCorrectionMaximumIteration, filter->GetInterSliceCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( optimizerInitialRadius, filter->GetOptimizerInitialRadius() );
  TEST_SET_GET_VALUE( optimizerGrowthFactor, filter->GetOptimizerGrowthFactor() );
  TEST_SET_GET_VALUE( usingInterSliceIntensityCorrection, filter->GetUsingInterSliceIntensityCorrection() );

  filter->SetBiasFieldMultiplicative( true ); // correct with multiplicative bias
  filter->SetBiasFieldDegree( biasDegree ); // default value = 3
  filter->SetTissueClassStatistics( classMeans, classSigmas );
  //TEST_SET_GET_VALUE( classMeans, classSigmas, filter->GetTissueClassStatistics() );

  filter->SetSlicingDirection( slicingDirection );
  //TEST_SET_GET_VALUE( slicingDirection, filter->GetSlicingDirection() );

  filter->SetInitialBiasFieldCoefficients( initCoefficients );
  //TEST_SET_GET_VALUE( initCoefficients, filter->GetInitialBiasFieldCoefficients() );

  //timing
  long int t1 = time(ITK_NULLPTR);
  filter->Update();
  long int t2 = time(ITK_NULLPTR);
  std::cout << "Run time (in s)" << t2-t1  << std::endl;

  sumOfError = 0.0;
  ImageIteratorType o_iter( filter->GetOutput(),
                            filter->GetOutput()->GetLargestPossibleRegion() );
  i_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += itk::Math::abs( o_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++o_iter;
    }

  if ( SaveImages )
    {
    typedef itk::ImageFileWriter< ImageType > WriterType;
    WriterType::Pointer writer = WriterType::New();

    writer->SetInput( image );
    writer->SetFileName( "MRISource.mhd" );
    writer->Update();

    WriterType::Pointer writer2 = WriterType::New();
    writer2->SetInput(imageWithBias);
    writer2->SetFileName( "MRISourceWithBias.mhd" );
    writer2->Update();

    WriterType::Pointer writer3 = WriterType::New();
    writer3->SetInput(filter->GetOutput());
    writer3->SetFileName( "MRICorrected.mhd" );
    writer3->Update();

    }

  std::cout << "Absolute Avg. error without input and output mask = "
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  if (origSumError < sumOfError)
    {
    std::cout << "ERROR: sumOfError: " << sumOfError
              << " is less than origSumError: " << origSumError
              << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Computing bias correction with mask" << std::endl;
  filter->SetInput( imageWithBias.GetPointer() );
  filter->SetInputMask( image.GetPointer() );
  filter->SetOutputMask( image.GetPointer() );

  volumeCorrectionMaximumIteration = 200;
  interSliceCorrectionMaximumIteration = 100;
  optimizerInitialRadius = 0.02;

  filter->SetVolumeCorrectionMaximumIteration( volumeCorrectionMaximumIteration );
  filter->SetInterSliceCorrectionMaximumIteration(interSliceCorrectionMaximumIteration);
  filter->SetOptimizerInitialRadius( optimizerInitialRadius );

  TEST_SET_GET_VALUE( volumeCorrectionMaximumIteration, filter->GetVolumeCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( interSliceCorrectionMaximumIteration, filter->GetInterSliceCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( optimizerInitialRadius, filter->GetOptimizerInitialRadius() );

  filter->SetInitialBiasFieldCoefficients( initCoefficients );
  //TEST_SET_GET_VALUE( initCoefficients, filter->GetInitialBiasFieldCoefficients() );

  t1 = time(ITK_NULLPTR);
  filter->Update();
  t2 = time(ITK_NULLPTR);
  std::cout << "Run time (in s)" << t2-t1  << std::endl;

  sumOfError = 0.0;
  ImageIteratorType o2_iter( filter->GetOutput(),
                             filter->GetOutput()->GetLargestPossibleRegion() );
  i_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += itk::Math::abs( o2_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++o2_iter;
    }

  std::cout << "Absolute Avg. error with input and output mask = "
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  if (origSumError < sumOfError)
    {
    std::cout << "ERROR: sumOfError: " << sumOfError
              << " is less than origSumError: " << origSumError
              << std::endl;
    return EXIT_FAILURE;
    }

  // default schedule is 2 2 2 - 1 1 1, let's change this
  std::cout << "Computing bias correction only with 2,2,2 resolution & no interSlice/Slab" << std::endl;


  usingInterSliceIntensityCorrection = false;
  usingSlabIdentification = false;
  optimizerInitialRadius = 0.02;
  volumeCorrectionMaximumIteration = 200;
  interSliceCorrectionMaximumIteration = 100;
  optimizerInitialRadius = 0.02;

  filter->SetUsingInterSliceIntensityCorrection( usingInterSliceIntensityCorrection );
  filter->SetUsingSlabIdentification( usingSlabIdentification );
  filter->SetOptimizerInitialRadius( optimizerInitialRadius );
  filter->SetVolumeCorrectionMaximumIteration( volumeCorrectionMaximumIteration );
  filter->SetInterSliceCorrectionMaximumIteration( interSliceCorrectionMaximumIteration );
  filter->SetOptimizerInitialRadius( optimizerInitialRadius );

  TEST_SET_GET_VALUE( usingInterSliceIntensityCorrection, filter->GetUsingInterSliceIntensityCorrection() );
  TEST_SET_GET_VALUE( usingSlabIdentification, filter->GetUsingSlabIdentification() );
  TEST_SET_GET_VALUE( optimizerInitialRadius, filter->GetOptimizerInitialRadius() );
  TEST_SET_GET_VALUE( volumeCorrectionMaximumIteration, filter->GetVolumeCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( interSliceCorrectionMaximumIteration, filter->GetInterSliceCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( optimizerInitialRadius, filter->GetOptimizerInitialRadius() );

  unsigned int numberOfLevels = 1;

  FilterType::ScheduleType schedule( 1, ImageDimension );
  schedule.Fill( 2 );

  // It is important to set the number of levels first, otherwise the filter rejects
  // the new schedule
  filter->SetNumberOfLevels( numberOfLevels );
  filter->SetSchedule( schedule );

  TEST_SET_GET_VALUE( numberOfLevels, filter->GetNumberOfLevels() );
  TEST_SET_GET_VALUE( schedule, filter->GetSchedule() );

  filter->SetInitialBiasFieldCoefficients( initCoefficients );
  //TEST_SET_GET_VALUE( initCoefficients, filter->GetInitialBiasFieldCoefficients() );

  t1 = time(ITK_NULLPTR);
  filter->Update();
  t2 = time(ITK_NULLPTR);
  std::cout << "Run time (in s)" << t2-t1  << std::endl;

  sumOfError = 0.0;
  ImageIteratorType o3_iter( filter->GetOutput(),
                             filter->GetOutput()->GetLargestPossibleRegion() );
  i_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += itk::Math::abs( o3_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++o3_iter;
    }

  std::cout << "Absolute Avg. error with input and output mask = "
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  if (origSumError < sumOfError)
    {
    std::cout << "ERROR: sumOfError: " << sumOfError
              << " is less than origSumError: " << origSumError
              << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Computing bias correction only with 4,4,4 resolution & no interSlice/Slab" << std::endl;

  schedule.Fill( 4 );

  usingInterSliceIntensityCorrection = false;
  usingSlabIdentification = false;
  numberOfLevels = 1;
  optimizerInitialRadius = 0.02;
  volumeCorrectionMaximumIteration = 200;
  interSliceCorrectionMaximumIteration = 100;

  filter->SetUsingInterSliceIntensityCorrection( usingInterSliceIntensityCorrection );
  filter->SetUsingSlabIdentification( usingSlabIdentification );
  filter->SetOptimizerInitialRadius( optimizerInitialRadius );
  filter->SetVolumeCorrectionMaximumIteration( volumeCorrectionMaximumIteration );
  filter->SetInterSliceCorrectionMaximumIteration( interSliceCorrectionMaximumIteration );

  // It is important to set the number of levels first, otherwise the filter rejects
  // the new schedule
  filter->SetNumberOfLevels( numberOfLevels );
  filter->SetSchedule( schedule );

  TEST_SET_GET_VALUE( usingInterSliceIntensityCorrection, filter->GetUsingInterSliceIntensityCorrection() );
  TEST_SET_GET_VALUE( usingSlabIdentification, filter->GetUsingSlabIdentification() );
  TEST_SET_GET_VALUE( numberOfLevels, filter->GetNumberOfLevels() );
  TEST_SET_GET_VALUE( optimizerInitialRadius, filter->GetOptimizerInitialRadius() );
  TEST_SET_GET_VALUE( schedule, filter->GetSchedule() );
  TEST_SET_GET_VALUE( volumeCorrectionMaximumIteration, filter->GetVolumeCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( interSliceCorrectionMaximumIteration, filter->GetInterSliceCorrectionMaximumIteration() );

  filter->SetInitialBiasFieldCoefficients( initCoefficients );
  //TEST_SET_GET_VALUE( initCoefficients, filter->GetInitialBiasFieldCoefficients() );

  t1 = time(ITK_NULLPTR);
  filter->Update();
  t2 = time(ITK_NULLPTR);
  std::cout << "Run time (in s)" << t2-t1  << std::endl;

  sumOfError = 0.0;
  ImageIteratorType o4_iter( filter->GetOutput(),
                             filter->GetOutput()->GetLargestPossibleRegion() );
  i_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += itk::Math::abs( o4_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++o4_iter;
    }

  std::cout << "Absolute Avg. error with input and output mask = "
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  if (origSumError < sumOfError)
    {
    std::cout << "ERROR: sumOfError: " << sumOfError
              << " is less than origSumError: " << origSumError
              << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Computing bias correction only with 4,4,4 resolution & no interSlice/Slab & more iterations" << std::endl;

  initCoefficients = filter->GetEstimatedBiasFieldCoefficients();

  volumeCorrectionMaximumIteration = 2000;
  interSliceCorrectionMaximumIteration = 100;

  filter->SetVolumeCorrectionMaximumIteration( volumeCorrectionMaximumIteration );
  filter->SetInterSliceCorrectionMaximumIteration( interSliceCorrectionMaximumIteration );

  TEST_SET_GET_VALUE( volumeCorrectionMaximumIteration, filter->GetVolumeCorrectionMaximumIteration() );
  TEST_SET_GET_VALUE( interSliceCorrectionMaximumIteration, filter->GetInterSliceCorrectionMaximumIteration() );

  filter->SetInitialBiasFieldCoefficients( initCoefficients );
  //TEST_SET_GET_VALUE( initCoefficients, filter->GetInitialBiasFieldCoefficients() );

  t1 = time(ITK_NULLPTR);
  filter->Update();
  t2 = time(ITK_NULLPTR);
  std::cout << "Run time (in s)" << t2-t1  << std::endl;

  double sumOfErrorFinal = 0.0;
  ImageIteratorType o5_iter( filter->GetOutput(),
                             filter->GetOutput()->GetLargestPossibleRegion() );
  i_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfErrorFinal += itk::Math::abs( o5_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++o5_iter;
    }

  std::cout << "Absolute Avg. error with input and output mask = "
            << sumOfErrorFinal / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  if (sumOfError < sumOfErrorFinal)
    {
    std::cout << "ERROR: sumOfError: " << sumOfError
              << " is less than sumOfErrorFinal: " << sumOfErrorFinal
              << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
