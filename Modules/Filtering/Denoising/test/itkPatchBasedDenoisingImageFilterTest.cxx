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

#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkMath.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkDiffusionTensor3D.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMeasurementVectorTraits.h"
#include "itkStdStreamLogOutput.h"
#include "itkGaussianRandomSpatialNeighborSubsampler.h"
#include "itkPatchBasedDenoisingImageFilter.h"
#include "itkTestingMacros.h"


template< typename TFilter >
typename TFilter::RealArrayType ParseKernelBandwithSigma( char* kernelBandwithSigmaIn,
  unsigned int numIndependentComponents )
{
  typename TFilter::RealArrayType kernelBandwithSigmaOut;
  kernelBandwithSigmaOut.SetSize(numIndependentComponents);

  // Get the individual components
  char *endPtr;
  unsigned int i = 0;
  typename TFilter::RealArrayType::ValueType value;
  while( *kernelBandwithSigmaIn && i < numIndependentComponents )
    {
    value = strtod( kernelBandwithSigmaIn, &endPtr );
    if( kernelBandwithSigmaIn == endPtr )
      {
      (*kernelBandwithSigmaIn)++;
      }
    else if( endPtr == NULL || *endPtr == 0 )
      {
      kernelBandwithSigmaOut[i] = value;
      break;
      }
    else
      {
      kernelBandwithSigmaOut[i] = value;
      kernelBandwithSigmaIn = endPtr + 1;
      }
    ++i;
    }

  return kernelBandwithSigmaOut;
}

template< typename ImageT >
int doDenoising( const std::string & inputFileName, const std::string & outputFileName,
                const unsigned int numIterations, const int numThreads, char* kernelBandwithSigma,
                const int numToSample, bool computeConditionalDerivatives,
                const double kernelBandwidthMultiplicationFactor,
                const std::string & noiseModelStr, const double noiseModelFidelityWeight )
{
  typedef itk::ImageFileReader< ImageT > ReaderType;

  typedef itk::PatchBasedDenoisingImageFilter< ImageT, ImageT > FilterType;

  typedef itk::Statistics::GaussianRandomSpatialNeighborSubsampler<
    typename FilterType::PatchSampleType, typename ImageT::RegionType > SamplerType;

  typedef typename FilterType::OutputImageType OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  // Read the noisy image to be denoised
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFileName );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  // Create filter and initialize
  typename FilterType::Pointer filter = FilterType::New();

  typename FilterType::InputImageType::Pointer inputImage = reader->GetOutput();
  filter->SetInput( inputImage );

  // Set whether conditional derivatives should be used estimating sigma
  TEST_SET_GET_BOOLEAN( filter, ComputeConditionalDerivatives,
    computeConditionalDerivatives );

  // Patch radius is same for all dimensions of the image
  const unsigned int patchRadius = 4;
  filter->SetPatchRadius( patchRadius );
  TEST_SET_GET_VALUE( patchRadius, filter->GetPatchRadius() );

  // Instead of directly setting the weights, could also specify type
  bool useSmoothDiscPatchWeights = true;
  TEST_SET_GET_BOOLEAN( filter, UseSmoothDiscPatchWeights,
    useSmoothDiscPatchWeights );
  bool useFastTensorComputations = true;
  TEST_SET_GET_BOOLEAN( filter, UseFastTensorComputations,
    useFastTensorComputations );

  // Noise model to use
  typename FilterType::NoiseModelType noiseModel;
  if( noiseModelStr == "GAUSSIAN" )
    {
    noiseModel = FilterType::GAUSSIAN;
    }
  else if( noiseModelStr == "RICIAN" )
    {
    noiseModel = FilterType::RICIAN;
    }
  else if( noiseModelStr == "POISSON" )
    {
    noiseModel = FilterType::POISSON;
    }
  else
    {
    noiseModel = FilterType::NOMODEL;
    }
  filter->SetNoiseModel( noiseModel );
  TEST_SET_GET_VALUE( noiseModel, filter->GetNoiseModel() );

  // Stepsize or weight for smoothing term
  double smoothingWeight = 1.0;
  filter->SetSmoothingWeight( smoothingWeight );
  TEST_SET_GET_VALUE( smoothingWeight, filter->GetSmoothingWeight() );

  // Stepsize or weight for fidelity term
  filter->SetNoiseModelFidelityWeight( noiseModelFidelityWeight );
  TEST_SET_GET_VALUE( noiseModelFidelityWeight,
    filter->GetNoiseModelFidelityWeight() );

  // Number of iterations over the image of denoising
  filter->SetNumberOfIterations( numIterations );
  TEST_SET_GET_VALUE( numIterations, filter->GetNumberOfIterations() );

  // Number of threads to use in parallel
  filter->SetNumberOfThreads( numThreads );

  // Sampling the image to find similar patches
  typename SamplerType::Pointer sampler = SamplerType::New();

  // Variance (in physical units) for semi-local Gaussian sampling
  sampler->SetVariance( 400 );

  // Rectangular window restricting the Gaussian sampling
  sampler->SetRadius( 50 ); // 2.5 * standard deviation

  // Number of random sample "patches" to use for computations
  sampler->SetNumberOfResultsRequested( numToSample );

  // Sampler can be complete neighborhood sampler, random neighborhood sampler,
  // Gaussian sampler, etc.
  filter->SetSampler( sampler );
  TEST_SET_GET_VALUE( sampler, filter->GetSampler() );

  // Automatic estimation of the kernel bandwidth
  bool kernelBandwidthEstimation = true;
  TEST_SET_GET_BOOLEAN( filter, KernelBandwidthEstimation,
    kernelBandwidthEstimation );

  // Update bandwidth every 'n' iterations
  unsigned int kernelBandwidthUpdateFrequency = 3;
  filter->SetKernelBandwidthUpdateFrequency(
    kernelBandwidthUpdateFrequency );
  TEST_SET_GET_VALUE( kernelBandwidthUpdateFrequency,
    filter->GetKernelBandwidthUpdateFrequency() );

  // Use 20% of the pixels for the sigma update calculation
  double kernelBandwidthFractionPixelsForEstimation = 0.20;
  filter->SetKernelBandwidthFractionPixelsForEstimation(
    kernelBandwidthFractionPixelsForEstimation );
  TEST_SET_GET_VALUE( kernelBandwidthFractionPixelsForEstimation,
    filter->GetKernelBandwidthFractionPixelsForEstimation() );

  // Multiplication factor modifying the automatically-estimated kernel sigma
  filter->SetKernelBandwidthMultiplicationFactor( kernelBandwidthMultiplicationFactor );
  TEST_SET_GET_VALUE( kernelBandwidthMultiplicationFactor,
    filter->GetKernelBandwidthMultiplicationFactor() );

  // Test the filter exceptions
  //

  // Test the nonpositive pixel component exception for the RICIAN and POISSON
  // noise models.
  // Temporarily modify the value of an arbitrary pixel of the input image to
  // get a nonpositive value.
  if( filter->GetNoiseModel() == FilterType::RICIAN ||
    filter->GetNoiseModel() == FilterType::POISSON )
    {
    typename ImageT::IndexType::IndexValueType indexValue = 0;
    typename ImageT::IndexType pixelIndex;
    pixelIndex.Fill( indexValue );

    typename ImageT::PixelType originalPixelValue =
      inputImage->GetPixel( pixelIndex );

    typename ImageT::PixelType nonpositivePixelValue = itk::NumericTraits<
        typename ImageT::PixelType >::NonpositiveMin();
    inputImage->SetPixel( pixelIndex, nonpositivePixelValue );


    TRY_EXPECT_EXCEPTION( filter->Update() );

    // Restore the original pixel value
    inputImage->SetPixel( pixelIndex, originalPixelValue );
    }


  // Denoise the image
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Exercise the PrintSelf method to know the patch radius in voxels once
  // the filter has been updated
  std::cout << filter << std::endl;

  // Regression test
  typename FilterType::RealArrayType expectedKernelBandwidthSigma =
    ParseKernelBandwithSigma< FilterType >( kernelBandwithSigma,
    filter->GetNumIndependentComponents() );
  typename FilterType::RealArrayType resultKernelBandwidthSigma =
    filter->GetKernelBandwidthSigma();
  if( expectedKernelBandwidthSigma.Size() !=
    resultKernelBandwidthSigma.Size() )
    {
      std::cout << "Error in GetKernelBandwidthSigma() " << std::endl;
      std::cout << "Expected value: " << expectedKernelBandwidthSigma
        << ", but got: " << resultKernelBandwidthSigma << std::endl;
      std::cout << "Array size mismatch." << std::endl;
      std::cout << "Test failed!" << std::endl;
      return EXIT_FAILURE;
    }
  else
    {
    typename FilterType::RealArrayType::iterator expectedKernelBandwidthSigmaIt =
      expectedKernelBandwidthSigma.begin();
    typename FilterType::RealArrayType::iterator resultKernelBandwidthSigmaIt =
      resultKernelBandwidthSigma.begin();

    unsigned int i = 0;

    // Although some cases converge to a higher degree of accuracy, the
    // tolerance set for the test is relatively loose due to the convergence
    // value of the algorithm set by
    // itk::PatchBasedDenoisingImageFilter::m_SigmaUpdateConvergenceTolerance
    // Hence, some tests require such a low degree of accuracy.
    //
    while( expectedKernelBandwidthSigmaIt != expectedKernelBandwidthSigma.end() &&
      resultKernelBandwidthSigmaIt != resultKernelBandwidthSigma.end() )
      {
      typename FilterType::RealArrayType::ValueType expectedValue =
        *expectedKernelBandwidthSigmaIt;
      typename FilterType::RealArrayType::ValueType resultValue =
        *resultKernelBandwidthSigmaIt;
      double tolerance = 1e-2 * expectedValue;
      if( !itk::Math::FloatAlmostEqual( expectedValue, resultValue, 10, tolerance ) )
        {
        std::cout.precision( unsigned( itk::Math::abs( std::log10( tolerance ) ) ) );
        std::cout << "Error in GetKernelBandwidthSigma() "
          << "at index: [" << i << "]" << std::endl;
        std::cout << "Expected value: " << expectedValue << ", but got: "
          << resultValue << std::endl;
        std::cout << "Test failed!" << std::endl;
        return EXIT_FAILURE;
        }
      ++expectedKernelBandwidthSigmaIt;
      ++resultKernelBandwidthSigmaIt;
      ++i;
      }
    }

  // Write the denoised image to file
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFileName );
  writer->SetInput( filter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

int itkPatchBasedDenoisingImageFilterTest( int argc, char * argv [] )
{
  if( argc < 6 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage : " << argv[0]
              << " inputImageFileName outputImageFileName"
              << " numDimensions numComponents"
              << " kernelBandwithSigma"
              << " [numIterations] [numThreads]"
              << " [numPatchesToSample]"
              << " [computeConditionalDerivatives]"
              << " [kernelBandwidthMultiplicationFactor]"
              << " [noiseModel] [noiseModelFidelityWeight]"
              << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef float                                                       PixelType;
  typedef itk::Image< PixelType, 3 >                                  ImageType;
  typedef itk::PatchBasedDenoisingImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, PatchBasedDenoisingImageFilter,
    PatchBasedDenoisingBaseImageFilter );

  const std::string inFileName( argv[1] );

  const std::string outFileName( argv[2] );

  const unsigned int numDimensions = atoi( argv[3] );

  const unsigned int numComponents = atoi( argv[4] );

  char* kernelBandwithSigma = argv[5];

  unsigned int numIterations = 1;
  if( argc > 6 )
    {
    numIterations = atoi( argv[6] );
    }

  unsigned int numThreads = 1;
  if( argc > 7 )
    {
    numThreads = atoi( argv[7] );
    }

  unsigned int numToSample = 1000;
  if( argc > 8 )
    {
    numToSample = atoi( argv[8] );
    }

  bool computeConditionalDerivatives = false;
  if( argc > 9 )
    {
    computeConditionalDerivatives = static_cast< bool >( atoi( argv[9] ) );
    }

  double kernelBandwidthMultFactor = 1.0;
  if( argc > 10 )
    {
    kernelBandwidthMultFactor = atof( argv[10] );
    }

  std::vector< std::string > modelChoices;
  modelChoices.push_back( "GAUSSIAN" );
  modelChoices.push_back( "RICIAN" );
  modelChoices.push_back( "POISSON" );
  modelChoices.push_back( "NOMODEL" );
  std::string noiseModel = modelChoices[0];

  double noiseModelFidelityWeight = 0.0;
  if( argc > 11 )
  {
    noiseModel = argv[11];
    bool validChoice = false;
    for( unsigned int ii = 0; ii < modelChoices.size(); ++ii )
      {
      if( noiseModel == modelChoices[ii] )
        {
        validChoice = true;
        }
      }
    if( !validChoice )
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << noiseModel << " is not a valid noise model choice. Please choose one of: ";
      for( unsigned int ii = 0; ii < modelChoices.size(); ++ii )
        {
        std::cerr << modelChoices[ii] << " " << std::endl;
        }
      return EXIT_FAILURE;
      }
    if( argc > 12 )
      {
      noiseModelFidelityWeight = atof( argv[12] );
      }
    else
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Must also specify a noise model fidelity weight when a noise model is specified."
                << std::endl;
      return EXIT_FAILURE;
      }
    }

  typedef float PixelComponentType;

  typedef PixelComponentType                           OneComponentType;
  typedef itk::RGBPixel< PixelComponentType >          ThreeComponentType;
  typedef itk::RGBAPixel< PixelComponentType >         FourComponentType;
  typedef itk::DiffusionTensor3D< PixelComponentType > SixComponentType;

  typedef itk::Image< OneComponentType, 2 > OneComponent2DImage;
  typedef itk::Image< OneComponentType, 3 > OneComponent3DImage;

  //typedef itk::VectorImage< PixelComponentType, 2 > TwoComponent2DImage;
  //typedef itk::VectorImage< PixelComponentType, 3 > TwoComponent3DImage;

  typedef itk::Image< ThreeComponentType, 2 > ThreeComponent2DImage;
  typedef itk::Image< ThreeComponentType, 3 > ThreeComponent3DImage;

  typedef itk::Image< FourComponentType, 2 > FourComponent2DImage;
  typedef itk::Image< FourComponentType, 3 > FourComponent3DImage;

  typedef itk::Image< SixComponentType, 2 > SixComponent2DImage;
  typedef itk::Image< SixComponentType, 3 > SixComponent3DImage;

  if( numComponents == 1 && numDimensions == 2 )
    {
    return doDenoising< OneComponent2DImage >( inFileName, outFileName,
                                              numIterations, numThreads,
                                              kernelBandwithSigma,
                                              numToSample,
                                              computeConditionalDerivatives,
                                              kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight );
    }
  /*else if( numComponents == 2 && numDimensions == 2 )
    {
     return doDenoising< TwoComponent2DImage >( inFileName, outFileName,
                                              numIterations, numThreads,
                                              kernelBandwithSigma,
                                              numToSample,
                                              computeConditionalDerivatives,
                                              kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight );
    }*/
  else if( numComponents == 3 && numDimensions == 2 )
    {
    return doDenoising< ThreeComponent2DImage >( inFileName, outFileName,
                                                numIterations, numThreads,
                                                kernelBandwithSigma,
                                                numToSample,
                                                computeConditionalDerivatives,
                                                kernelBandwidthMultFactor,
                                                noiseModel, noiseModelFidelityWeight);
    }
  else if( numComponents == 4 && numDimensions == 2 )
    {
    return doDenoising< FourComponent2DImage >( inFileName, outFileName,
                                              numIterations, numThreads,
                                              kernelBandwithSigma,
                                              numToSample,
                                              computeConditionalDerivatives,
                                              kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight);
    }
  else if( numComponents == 6 && numDimensions == 2 )
    {
    return doDenoising< SixComponent2DImage >(inFileName, outFileName,
                                            numIterations, numThreads,
                                            kernelBandwithSigma,
                                            numToSample,
                                            computeConditionalDerivatives,
                                            kernelBandwidthMultFactor,
                                            noiseModel, noiseModelFidelityWeight);
    }
  else if( numComponents == 1 && numDimensions == 3 )
    {
    return doDenoising< OneComponent3DImage >( inFileName, outFileName,
                                              numIterations, numThreads,
                                              kernelBandwithSigma,
                                              numToSample,
                                              computeConditionalDerivatives,
                                              kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight );
    }
  /*else if( numComponents == 2 && numDimensions == 3 )
    {
    return doDenoising< TwoComponent3DImage >( inFileName, outFileName,
                                             numIterations, numThreads,
                                             kernelBandwithSigma,
                                             numToSample,
                                             computeConditionalDerivatives,
                                             kernelBandwidthMultFactor,
                                             noiseModel, noiseModelFidelityWeight );
    }*/
  else if ( numComponents == 3 && numDimensions == 3 )
    {
    return doDenoising< ThreeComponent3DImage >( inFileName, outFileName,
                                                numIterations, numThreads,
                                                kernelBandwithSigma,
                                                numToSample,
                                                computeConditionalDerivatives,
                                                kernelBandwidthMultFactor,
                                                noiseModel, noiseModelFidelityWeight );
    }
  else if( numComponents == 4 && numDimensions == 3 )
    {
    return doDenoising< FourComponent3DImage >( inFileName, outFileName,
                                              numIterations, numThreads,
                                              kernelBandwithSigma,
                                              numToSample,
                                              computeConditionalDerivatives,
                                              kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight );
    }
  else if ( numComponents == 6 && numDimensions == 3 )
    {
    return doDenoising< SixComponent3DImage >( inFileName, outFileName,
                                              numIterations, numThreads,
                                              kernelBandwithSigma,
                                              numToSample,
                                              computeConditionalDerivatives,
                                              kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight );
    }
  else
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Combination of "
              << numComponents << " components and "
              << numDimensions << " dimensions "
              << "isn't supported in this test driver."
              << std::endl;
    return EXIT_FAILURE;
    }

}
