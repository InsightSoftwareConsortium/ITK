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
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkDiffusionTensor3D.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMeasurementVectorTraits.h"
#include "itkStdStreamLogOutput.h"
#include "itkGaussianRandomSpatialNeighborSubsampler.h"
#include "itkPatchBasedDenoisingImageFilter.h"

template <typename ImageT>
int doDenoising(const std::string & inputFileName, const std::string & outputFileName,
                const int numIterations, const int numThreads,
                const int numToSample, const float kernelBandwidthMultiplicationFactor,
                const std::string & noiseModel, const float noiseModelFidelityWeight)
{
  typedef itk::ImageFileReader< ImageT > ReaderType;

  typedef itk::PatchBasedDenoisingImageFilter<ImageT, ImageT> FilterType;

  typedef typename FilterType::PatchWeightsType PatchType;

  typedef itk::Statistics::GaussianRandomSpatialNeighborSubsampler<
    typename FilterType::PatchSampleType, typename ImageT::RegionType> SamplerType;

  typedef typename FilterType::OutputImageType OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  // read the noisy image to be denoised
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFileName );
  try
  {
    reader->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Problem encountered while reading image file : " << inputFileName << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // create filter and initialize
  // give image to filter and run it
  // get filter output and write to file

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  // patch radius is same for all dimensions of the image
  const unsigned int patchRadius = 4;
  filter->SetPatchRadius(patchRadius);
  // instead of directly setting the weights, could also specify type
  filter->UseSmoothDiscPatchWeightsOn();
  filter->UseFastTensorComputationsOn();

  // noise model to use
  if (noiseModel == "GAUSSIAN")
  {
    filter->SetNoiseModel(FilterType::GAUSSIAN);
  }
  else if (noiseModel == "RICIAN")
  {
    filter->SetNoiseModel(FilterType::RICIAN);
  }
  else if (noiseModel == "POISSON")
  {
    filter->SetNoiseModel(FilterType::POISSON);
  }
  else
  {
    filter->SetNoiseModel(FilterType::NOMODEL);
  }
  // stepsize or weight for smoothing term
  // Large stepsizes may cause instabilities.
  filter->SetSmoothingWeight(1);
  // stepsize or weight for fidelity term
  // use a positive weight to prevent oversmoothing
  // (penalizes deviations from noisy data based on a noise model)
  filter->SetNoiseModelFidelityWeight(noiseModelFidelityWeight);

  // number of iterations over the image of denoising
  filter->SetNumberOfIterations(numIterations);

  // number of threads to use in parallel
  filter->SetNumberOfThreads(numThreads);

  // sampling the image to find similar patches
  typename SamplerType::Pointer sampler = SamplerType::New();
  // variance (in physical units) for semi-local Gaussian sampling
  sampler->SetVariance(400);
  // rectangular window restricting the Gaussian sampling
  sampler->SetRadius(50); // 2.5 * standard deviation
  // number of random sample "patches" to use for computations
  sampler->SetNumberOfResultsRequested(numToSample);
  // Sampler can be complete neighborhood sampler, random neighborhood sampler,
  // Gaussian sampler, etc.
  filter->SetSampler(sampler);

  // automatic estimation of the kernel bandwidth
  filter->KernelBandwidthEstimationOn();
  // update bandwidth every 'n' iterations
  filter->SetKernelBandwidthUpdateFrequency(3);
  // use 33% of the pixels for the sigma update calculation
  filter->SetKernelBandwidthFractionPixelsForEstimation(0.20);
  // multiplication factor modifying the automatically-estimated kernel sigma
  filter->SetKernelBandwidthMultiplicationFactor(kernelBandwidthMultiplicationFactor);

  // manually-selected Gaussian kernel sigma
  // filter->DoKernelBandwidthEstimationOff();
  // typename FilterType::RealArrayType gaussianKernelSigma;
  // gaussianKernelSigma.SetSize(reader->GetOutput()->GetNumberOfComponentsPerPixel());
  // gaussianKernelSigma.Fill(11);
  // filter->SetGaussianKernelSigma (gaussianKernelSigma);

  // denoise the image
  std::cout << "Filter prior to update:\n";
  filter->Print( std::cout );
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::ostringstream itkmsg;                                            \
    itkmsg << "Error: In " __FILE__ ", line " << __LINE__ << "\n"
           << "Caught exception <" << excp
           << "> while running patch-based denoising image filter."
           << "\n\n";
    ::itk::OutputWindowDisplayWarningText(itkmsg.str().c_str());
    return EXIT_FAILURE;
  }

  // write the denoised image to file
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFileName );
  writer->SetInput( filter->GetOutput() );
  std::cout << "Writing NumberOfComponents: "
            << filter->GetOutput()->GetNumberOfComponentsPerPixel()
            << " to file." << std::endl;
  try
  {
    writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int itkPatchBasedDenoisingImageFilterTest( int argc, char * argv [] )
{

  if( argc < 3 )
  {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0]
              << " inputImageFileName outputImageFileName"
              << " numDimensions numComponents"
              << " [numIterations] [numThreads]"
              << " [numPatchesToSample] [kernelBandwidthMultiplicationFactor]"
              << " [noiseModel] [noiseModelFidelityWeight]"
              << std::endl;
    return EXIT_FAILURE;
  }

  const std::string inFileName(argv[1]);

  const std::string outFileName(argv[2]);

  const unsigned int numDimensions = atoi(argv[3]);

  const unsigned int numComponents = atoi(argv[4]);

  unsigned int numIterations = 1;
  if (argc > 5)
  {
    numIterations = atoi(argv[5]);
  }

  unsigned int numThreads = 1;
  if (argc > 6)
  {
    numThreads = atoi(argv[6]);
  }

  unsigned int numToSample = 1000;
  if (argc > 7)
  {
    numToSample = atoi(argv[7]);
  }

  float kernelBandwidthMultFactor = 1;
  if (argc > 8)
  {
    kernelBandwidthMultFactor = atof(argv[8]);
  }

  std::vector< std::string > modelChoices;
  modelChoices.push_back("GAUSSIAN");
  modelChoices.push_back("RICIAN");
  modelChoices.push_back("POISSON");
  modelChoices.push_back("NOMODEL");
  std::string noiseModel;
  noiseModel = modelChoices[0];

  float noiseModelFidelityWeight = 0.0;
  if (argc > 9)
  {
    noiseModel = argv[9];
    bool validChoice = false;
    for (unsigned int ii = 0; ii < modelChoices.size(); ++ii)
    {
      if( noiseModel == modelChoices[ii])
      {
        validChoice = true;
      }
    }
    if (!validChoice)
    {
      std::cerr << noiseModel << " is not a valid noise model choice.  Please choose one of: ";
      for (unsigned int ii = 0; ii < modelChoices.size(); ++ii)
      {
        std::cerr << modelChoices[ii] << " " << std::endl;
      }
      return EXIT_FAILURE;
    }
    if (argc > 10)
    {
      noiseModelFidelityWeight = atof(argv[10]);
    }
    else
    {
      std::cerr << "Must also specify a noise model fidelity weight when a noise model is specified."
                << std::endl;
      return EXIT_FAILURE;
    }
  }

  typedef float PixelComponentType;
  //
  typedef PixelComponentType                           OneComponentType;
  typedef itk::RGBPixel< PixelComponentType >          ThreeComponentType;
  typedef itk::RGBAPixel< PixelComponentType >         FourComponentType;
  typedef itk::DiffusionTensor3D< PixelComponentType > SixComponentType;
  //
  typedef itk::Image< OneComponentType, 2 > OneComponent2DImage;
  typedef itk::Image< OneComponentType, 3 > OneComponent3DImage;
  //
  // Will not compile due to issue with VectorImageToImageAdaptor
  // wait for http://review.source.kitware.com/5572 to be resolved
  // before testing these instantiations.
//   typedef itk::VectorImage< PixelComponentType, 2 > TwoComponent2DImage;
//   typedef itk::VectorImage< PixelComponentType, 3 > TwoComponent3DImage;
  //
  typedef itk::Image< ThreeComponentType, 2 > ThreeComponent2DImage;
  typedef itk::Image< ThreeComponentType, 3 > ThreeComponent3DImage;
  //
  typedef itk::Image< FourComponentType, 2 > FourComponent2DImage;
  typedef itk::Image< FourComponentType, 3 > FourComponent3DImage;
  //
  typedef itk::Image< SixComponentType, 2 > SixComponent2DImage;
  typedef itk::Image< SixComponentType, 3 > SixComponent3DImage;
  //
  if (numComponents == 1 && numDimensions == 2)
    {
    return doDenoising<OneComponent2DImage>(inFileName, outFileName,
                                            numIterations, numThreads,
                                            numToSample, kernelBandwidthMultFactor,
                                            noiseModel, noiseModelFidelityWeight);
  }
//   else if (numComponents == 2 && numDimensions == 2)
//   {
//     return doDenoising<TwoComponent2DImage>(inFileName, outFileName,
//                                             numIterations, numThreads,
//                                             numToSample, kernelBandwidthMultFactor,
//                                             noiseModel, noiseModelFidelityWeight);
//   }
  else if (numComponents == 3 && numDimensions == 2)
  {
    return doDenoising<ThreeComponent2DImage>(inFileName, outFileName,
                                              numIterations, numThreads,
                                              numToSample, kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight);
  }
  else if (numComponents == 4 && numDimensions == 2)
  {
    return doDenoising<FourComponent2DImage>(inFileName, outFileName,
                                             numIterations, numThreads,
                                             numToSample, kernelBandwidthMultFactor,
                                             noiseModel, noiseModelFidelityWeight);
  }
  else if (numComponents == 6 && numDimensions == 2)
  {
    return doDenoising<SixComponent2DImage>(inFileName, outFileName,
                                            numIterations, numThreads,
                                            numToSample, kernelBandwidthMultFactor,
                                            noiseModel, noiseModelFidelityWeight);
  }
  //
  else if (numComponents == 1 && numDimensions == 3)
  {
    return doDenoising<OneComponent3DImage>(inFileName, outFileName,
                                            numIterations, numThreads,
                                            numToSample, kernelBandwidthMultFactor,
                                            noiseModel, noiseModelFidelityWeight);
  }
//   else if (numComponents == 2 && numDimensions == 3)
//   {
//     return doDenoising<TwoComponent3DImage>(inFileName, outFileName,
//                                             numIterations, numThreads,
//                                             numToSample, kernelBandwidthMultFactor,
//                                             noiseModel, noiseModelFidelityWeight);
//   }
  else if (numComponents == 3 && numDimensions == 3)
  {
    return doDenoising<ThreeComponent3DImage>(inFileName, outFileName,
                                              numIterations, numThreads,
                                              numToSample, kernelBandwidthMultFactor,
                                              noiseModel, noiseModelFidelityWeight);
  }
  else if (numComponents == 4 && numDimensions == 3)
  {
    return doDenoising<FourComponent3DImage>(inFileName, outFileName,
                                             numIterations, numThreads,
                                             numToSample, kernelBandwidthMultFactor,
                                             noiseModel, noiseModelFidelityWeight);
  }
  else if (numComponents == 6 && numDimensions == 3)
  {
    return doDenoising<SixComponent3DImage>(inFileName, outFileName,
                                            numIterations, numThreads,
                                            numToSample, kernelBandwidthMultFactor,
                                            noiseModel, noiseModelFidelityWeight);
  }
  else
  {
    std::cout << "Combination of "
              << numComponents << " components and "
              << numDimensions << " dimensions "
              << "isn't supported in this test driver."
              << std::endl;
    return EXIT_FAILURE;
  }

  // shouldn't reach this point, return failure here to keep the compiler happy
  return EXIT_FAILURE;
}
