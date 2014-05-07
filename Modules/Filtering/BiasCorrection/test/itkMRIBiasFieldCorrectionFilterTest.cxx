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

int itkMRIBiasFieldCorrectionFilterTest ( int , char* [] )
{
  const unsigned int ImageDimension = 3;

  typedef itk::Image< float, ImageDimension >            ImageType;
  typedef itk::Image< float, ImageDimension >            MaskType;
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
  BiasFieldType bias(biasSize.size(),
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


  // To see the debug output for each iteration, uncomment the
  // following line.
  // filter->DebugOn();

  double sumOfError = 0.0;
  i_iter.GoToBegin();
  ib_iter.GoToBegin();
  while ( !i_iter.IsAtEnd() )
    {
    sumOfError += vnl_math_abs( ib_iter.Get() - i_iter.Get() );
    ++i_iter;
    ++ib_iter;
    }
  std::cout << "Absolute Avg. error before correction = "
            << sumOfError / (imageSize[0] * imageSize[1] * imageSize[2])
            << std::endl;
  double origSumError = sumOfError;

  std::cout << "Computing bias correction without mask, 2 classes 10,10 - 200,20" << std::endl;
  filter->SetInput( imageWithBias.GetPointer() );
  filter->IsBiasFieldMultiplicative( true ); // correct with multiplicative bias
  filter->SetBiasFieldDegree( biasDegree ); // default value = 3
  filter->SetTissueClassStatistics( classMeans, classSigmas );
  //filter->SetOptimizerGrowthFactor( 1.01 ); // default value
  //filter->SetOptimizerInitialRadius( 0.02 ); // default value
  filter->SetUsingInterSliceIntensityCorrection( true ); // default value
  filter->SetVolumeCorrectionMaximumIteration( 200 ); // default value = 100
  filter->SetInterSliceCorrectionMaximumIteration( 100 ); // default value = 100
  filter->SetUsingSlabIdentification( true ); // default value = false
  filter->SetSlabBackgroundMinimumThreshold( 0 ); // default value
  filter->SetSlabNumberOfSamples( 10 ); // default value
  filter->SetSlabTolerance(0.0); // default value
  filter->SetSlicingDirection( 2 ); // default value
  filter->SetUsingBiasFieldCorrection( true ); // default value
  filter->SetGeneratingOutput( true ); // default value

  filter->SetInitialBiasFieldCoefficients(initCoefficients); //default value is all zero

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
    sumOfError += vnl_math_abs( o_iter.Get() - i_iter.Get() );
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
  filter->SetInitialBiasFieldCoefficients(initCoefficients);
  filter->SetVolumeCorrectionMaximumIteration( 200 ); // default value = 100
  filter->SetInterSliceCorrectionMaximumIteration( 100 ); // default value = 100
  //filter->SetOptimizerInitialRadius( 0.02 ); // default value
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
    sumOfError += vnl_math_abs( o2_iter.Get() - i_iter.Get() );
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
  filter->SetUsingInterSliceIntensityCorrection( false ); // default value
  filter->SetUsingSlabIdentification( false ); // default value = false
  FilterType::ScheduleType schedule ( 1, ImageDimension);
  schedule.Fill( 2 );
  filter->SetNumberOfLevels( 1 ); // Important to set this first, otherwise the filter rejects the new schedule
  filter->SetSchedule( schedule );
  filter->SetInitialBiasFieldCoefficients(initCoefficients);
  filter->SetVolumeCorrectionMaximumIteration( 200 ); // default value = 100
  filter->SetInterSliceCorrectionMaximumIteration( 100 ); // default value = 100
  //filter->SetOptimizerInitialRadius( 0.02 ); // default value
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
    sumOfError += vnl_math_abs( o3_iter.Get() - i_iter.Get() );
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
  filter->SetUsingInterSliceIntensityCorrection( false ); // default value
  filter->SetUsingSlabIdentification( false ); // default value = false
  schedule.Fill( 4 );
  filter->SetNumberOfLevels( 1 );
  //filter->SetOptimizerInitialRadius( 0.02 ); // default value
  filter->SetSchedule( schedule );
  filter->SetVolumeCorrectionMaximumIteration( 200 ); // default value = 100
  filter->SetInterSliceCorrectionMaximumIteration( 100 ); // default value = 100
  filter->SetInitialBiasFieldCoefficients(initCoefficients);
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
    sumOfError += vnl_math_abs( o4_iter.Get() - i_iter.Get() );
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
  filter->SetInitialBiasFieldCoefficients(initCoefficients);
  filter->SetVolumeCorrectionMaximumIteration( 2000 ); // default value = 100
  filter->SetInterSliceCorrectionMaximumIteration( 100 ); // default value = 100
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
    sumOfErrorFinal += vnl_math_abs( o5_iter.Get() - i_iter.Get() );
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

  std::cout << "Using slab identification: "
            << filter->GetUsingSlabIdentification() << std::endl;
  std::cout << "Slab identification background minimum intensity threshold: "
            << filter->GetSlabBackgroundMinimumThreshold() << std::endl;
  std::cout << "Slab number of samples per slice: "
            << filter->GetSlabNumberOfSamples() << std::endl;
  std::cout << "Slab identification tolerance: "
            << filter->GetSlabTolerance() << std::endl;
  std::cout << "Using bias field correction: "
            << filter->GetUsingBiasFieldCorrection() << std::endl;
  std::cout << "Generating output: "
            << filter->GetGeneratingOutput() << std::endl;
  std::cout << "Bias field degree: "
            << filter->GetBiasFieldDegree() << std::endl;
  std::cout << "Volume bias field correction iterations: "
            << filter->GetVolumeCorrectionMaximumIteration() << std::endl;
  std::cout << "Interslice bias field correction iterations: "
            << filter->GetInterSliceCorrectionMaximumIteration() << std::endl;
  std::cout << "Optimizer initial radius: "
            << filter->GetOptimizerInitialRadius() << std::endl;
  std::cout << "Optimizer growth factor: "
            << filter->GetOptimizerGrowthFactor() << std::endl;
  std::cout << "Optimizer shrink factor: "
            << filter->GetOptimizerShrinkFactor() << std::endl;

  return EXIT_SUCCESS;
}
