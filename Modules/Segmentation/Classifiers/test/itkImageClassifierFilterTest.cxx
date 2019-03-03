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

#include "itkImageClassifierFilter.h"
#include "itkGaussianMixtureModelComponent.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkMaximumDecisionRule.h"
#include "itkImageToListSampleAdaptor.h"
#include "itkNormalVariateGenerator.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

//This program tests the ImageClassifierFilter. The test uses the
//ExpectationMaximizationMixtureModelEstimator to estimaete membership
//function parameters.
int itkImageClassifierFilterTest(int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing command line arguments: "
              << itkNameOfTestExecutableMacro(argv) << "\t" << "ClassifiedOutputImage name" << std::endl;
    return EXIT_FAILURE;
    }
  constexpr unsigned int MeasurementVectorSize = 1;
  using MeasurementComponentType = double;
  constexpr unsigned int numberOfClasses = 2;
  using InputPixelType = itk::FixedArray< MeasurementComponentType, MeasurementVectorSize >;

  constexpr unsigned int ImageDimension = 2;
  using InputImageType = itk::Image< InputPixelType, ImageDimension >;

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image< OutputPixelType, ImageDimension >;

  //Generate an image with pixel intensities generated from two normal
  //distributions
  using NormalGeneratorType = itk::Statistics::NormalVariateGenerator;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New();
  normalGenerator->Initialize( 101 );

  InputImageType::Pointer image = InputImageType::New();

  InputImageType::IndexType start;
  InputImageType::SizeType  size;

  start.Fill( 0 );
  size.Fill( 512 );

  InputImageType::RegionType region( start, size );
  image->SetRegions( region );
  image->Allocate();

  //Fill the first half of the input image with pixel intensities
  //gnerated from a normal distribution defined by the following parameters
  double mean = 10.5;
  double standardDeviation = 5.0;

  InputImageType::IndexType index;
  unsigned int halfSize = size[1]/2;

  for(unsigned int y = 0; y < halfSize; y++ )
    {
    index[1] = y;
    for(unsigned int x = 0; x < size[0]; x++ )
      {
      index[0] = x;
      InputPixelType value;
      value[0] = (normalGenerator->GetVariate() * standardDeviation ) + mean;
      //std::cout << "Index = \t" << index << "\t" << value << std::endl;
      image->SetPixel(index, value);
      }
    }

  //Pixel intensities generated from the second normal distribution
  double mean2 = 200.5;
  double standardDeviation2 = 20.0;

  for(unsigned int y = halfSize; y < size[1]; y++ )
    {
    index[1] = y;
    for(unsigned int x = 0; x < size[0]; x++ )
      {
      index[0] = x;
      InputPixelType value;
      value[0] = (normalGenerator->GetVariate() * standardDeviation2 ) + mean2;
      //std::cout << "Index = \t" << index << "\t" << value << std::endl;
      image->SetPixel(index, value);
      }
    }

  //Instantiate an image to list sample adaptor to pass the sample list
  //to EM estimator
  using ImageToListSampleAdaptorType = itk::Statistics::ImageToListSampleAdaptor<InputImageType>;

  ImageToListSampleAdaptorType::Pointer sample = ImageToListSampleAdaptorType::New();
  sample->SetImage( image );

  //Use EM estimator to estimate gaussian membership functions
  using EstimatorType = itk::Statistics::ExpectationMaximizationMixtureModelEstimator<ImageToListSampleAdaptorType>;
  using ComponentType = itk::Statistics::GaussianMixtureModelComponent<ImageToListSampleAdaptorType>;

  /* Preparing the gaussian mixture components */
  using ParametersType = itk::Array < double >;
  std::vector< ParametersType > initialParameters(numberOfClasses);
  ParametersType params(2);
  params[0] = 8.0;
  params[1] = 0.1;
  initialParameters[0] = params;

  params[0] = 170.0;
  params[1] = 2.0;
  initialParameters[1] = params;

  using ComponentPointer = ComponentType::Pointer;
  std::vector< ComponentPointer > components;
  for (unsigned int i = 0; i < numberOfClasses; i++ )
    {
      components.push_back(ComponentType::New());
      (components[i])->SetSample(sample);
      (components[i])->SetParameters(initialParameters[i]);
    }

  /* Estimating */
  EstimatorType::Pointer estimator = EstimatorType::New();
  estimator->SetSample(sample);

  int maximumIteration = 200;
  estimator->SetMaximumIteration(maximumIteration);

  itk::Array< double > initialProportions(numberOfClasses);
  initialProportions[0] = 0.5;
  initialProportions[1] = 0.5;

  estimator->SetInitialProportions(initialProportions);

  for (unsigned int i = 0; i < numberOfClasses; i++)
    {
      estimator->AddComponent((ComponentType::Superclass*)
                              (components[i]).GetPointer());
    }

  estimator->Update();

  for (unsigned int i = 0; i < numberOfClasses; i++)
    {
      std::cout << "Cluster[" << i << "]" << std::endl;
      std::cout << "    Parameters:" << std::endl;
      std::cout << "         " << (components[i])->GetFullParameters() << std::endl;
      std::cout << "    Proportion: ";
      std::cout << "         " << (estimator->GetProportions())[i] << std::endl;
    }


  using ImageClassifierFilterType = itk::Statistics::ImageClassifierFilter< ImageToListSampleAdaptorType,
  InputImageType,OutputImageType >;
  ImageClassifierFilterType::Pointer filter
                              = ImageClassifierFilterType::New();

  using ClassLabelVectorObjectType = ImageClassifierFilterType::ClassLabelVectorObjectType;
  using ClassLabelVectorType = ImageClassifierFilterType::ClassLabelVectorType;

  ClassLabelVectorObjectType::Pointer  classLabelsObject = ClassLabelVectorObjectType::New();

  // Add class labels
  ClassLabelVectorType & classLabelVector  = classLabelsObject->Get();

  using ClassLabelType = ImageClassifierFilterType::ClassLabelType;

  ClassLabelType  class1 = 0;
  classLabelVector.push_back( class1 );

  ClassLabelType  class2 = 255;
  classLabelVector.push_back( class2 );

  //Set a decision rule type
  using DecisionRuleType = itk::Statistics::MaximumDecisionRule;

  DecisionRuleType::Pointer    decisionRule = DecisionRuleType::New();

  const ImageClassifierFilterType::MembershipFunctionVectorObjectType *
                membershipFunctionsObject = estimator->GetOutput();

  /* Print out estimated parameters of the membership function */

  const ImageClassifierFilterType::MembershipFunctionVectorType
            membershipFunctions = membershipFunctionsObject->Get();

  auto
                    begin = membershipFunctions.begin();

  auto
                    end = membershipFunctions.end();

  ImageClassifierFilterType::MembershipFunctionVectorType::const_iterator functionIter;

  functionIter=begin;

  unsigned int counter=1;
  std::cout << "Estimator membership function output " << std::endl;
  while( functionIter != end )
    {
    ImageClassifierFilterType::MembershipFunctionPointer membershipFunction = *functionIter;
    const auto *
          gaussianMemberShpFunction =
        dynamic_cast<const EstimatorType::GaussianMembershipFunctionType*>(membershipFunction.GetPointer());
    std::cout << "\tMembership function:\t " << counter << std::endl;
    std::cout << "\t\tMean="<< gaussianMemberShpFunction->GetMean() << std::endl;
    std::cout << "\t\tCovariance matrix=" << gaussianMemberShpFunction->GetCovariance() << std::endl;
    functionIter++;
    counter++;
    }

  //Set membership functions weight array
  const ImageClassifierFilterType::MembershipFunctionsWeightsArrayObjectType *
            weightArrayObjects = estimator->GetMembershipFunctionsWeightsArray();
  const ImageClassifierFilterType::MembershipFunctionsWeightsArrayType  weightsArray = weightArrayObjects->Get();

  std::cout << "Estimator membership function Weight/proporation output: " << std::endl;
  for(unsigned int i=0; i < weightsArray.Size(); i++ )
    {
    std::cout << "Membership function: \t" << i << "\t" << weightsArray[i] << std::endl;
    }

  filter->SetImage( image );

  filter->SetNumberOfClasses( numberOfClasses );

  if( filter->GetNumberOfClasses() != numberOfClasses )
    {
    std::cerr << "Get/SetNumberOfClasses error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetClassLabels( classLabelsObject );
  filter->SetMembershipFunctions( membershipFunctionsObject );
  filter->SetMembershipFunctionsWeightsArray( weightArrayObjects );

  //Run the filter without setting a decision rule. An exception should be
  //thrown
  try
    {
    filter->Update();
    std::cerr << "Attempting to run a classification without setting"
              << "decision rule, should throw an exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    }


  filter->SetDecisionRule( decisionRule );

  //Test Set/GetDecisionRule method
  if( filter->GetDecisionRule() != decisionRule )
    {
    std::cerr << "Set/GetDecisionRule method error \n" << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //Write out the classified image
  using OutputImageWriterType = itk::ImageFileWriter< OutputImageType >;
  OutputImageWriterType::Pointer outputImageWriter = OutputImageWriterType::New();
  outputImageWriter->SetFileName( argv[1] );
  outputImageWriter->SetInput( filter->GetOutput() );
  outputImageWriter->Update();

  //Check if the measurement vectors are correctly labelled.
  //TODO

  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
