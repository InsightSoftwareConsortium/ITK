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

#include <fstream>

#include "itkPointSetToListSampleAdaptor.h"
#include "itkSampleClassifierFilter.h"
#include "itkMaximumDecisionRule.h"

#include "itkGaussianMixtureModelComponent.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"

//Sample classifier test using Gaussian Mixture model and EM estimator
int itkSampleClassifierFilterTest7(int argc, char* argv[] )
{
  typedef itk::PointSet< double, 2 > PointSetType;
  typedef itk::Statistics::PointSetToListSampleAdaptor< PointSetType > DataSampleType;
  typedef itk::Statistics::ExpectationMaximizationMixtureModelEstimator< DataSampleType > EstimatorType;
  typedef itk::Statistics::GaussianMixtureModelComponent< DataSampleType > ComponentType;

  if (argc < 3)
    {
    std::cout << "ERROR: Missing arguments.\t"  << argv[0]
              << "Input_data_sample" << "\t"
              << "Target_data_sample"
              << std::endl;
    return EXIT_FAILURE;
    }

  const int maximumIteration = 200;
  const double minStandardDeviation =28.54746;
  const unsigned int numberOfClasses = 2;
  typedef itk::Array< double > ParametersType;
  std::vector< ParametersType > trueParameters(numberOfClasses);
  ParametersType params(6);
  params[0] = 99.261;
  params[1] = 100.078;
  params[2] = 814.95741;
  params[3] = 38.40308;
  params[4] = 38.40308;
  params[5] = 817.64446;
  trueParameters[0] = params;

  params[0] = 200.1;
  params[1] = 201.3;
  params[2] = 859.785295;
  params[3] = -3.617316;
  params[4] = -3.617316;
  params[5] = 848.991508;
  trueParameters[1] = params;

  // only the means are altered
  std::vector< ParametersType > initialParameters(numberOfClasses);
  params[0] = 80.0;
  params[1] = 80.0;
  params[2] = 814.95741;
  params[3] = 38.40308;
  params[4] = 38.40308;
  params[5] = 817.64446;
  initialParameters[0] = params;

  params[0] = 180.0;
  params[1] = 180.0;
  params[2] = 859.785295;
  params[3] = -3.617316;
  params[4] = -3.617316;
  params[5] = 848.991508;
  initialParameters[1] = params;

  itk::Array< double > trueProportions(numberOfClasses);
  trueProportions[0] = 0.5;
  trueProportions[1] = 0.5;

  itk::Array< double > initialProportions(numberOfClasses);
  initialProportions[0] = 0.5;
  initialProportions[1] = 0.5;

  /* Loading point data */
  PointSetType::Pointer pointSet = PointSetType::New();
  PointSetType::PointsContainerPointer pointsContainer = PointSetType::PointsContainer::New();
  const int dataSizeBig = 2000;
  pointsContainer->Reserve(dataSizeBig);
  pointSet->SetPoints(pointsContainer.GetPointer());

  PointSetType::PointsContainerIterator p_iter = pointsContainer->Begin();
  PointSetType::PointType point;

  char * const dataFileName = argv[1];
  std::ifstream dataStream(dataFileName);
  if ( !dataStream )
    {
    std::cout << "ERROR: fail to open the data file." << std::endl;
    return EXIT_FAILURE;
    }

  while (p_iter != pointsContainer->End())
    {
    for ( unsigned int i = 0; i < PointSetType::PointDimension; i++)
      {
      double temp;
      dataStream >> temp;
      point[i] = temp;
      }
    p_iter.Value() = point;
    ++p_iter;
    }

  dataStream.close();

  /* Importing the point set to the sample */
  DataSampleType::Pointer sample = DataSampleType::New();

  sample->SetPointSet(pointSet.GetPointer());

  /* Preparing the gaussian mixture components */
  typedef ComponentType::Pointer ComponentPointer;
  std::vector< ComponentPointer > components;
  for ( unsigned int i = 0; i < numberOfClasses; i++ )
    {
    components.push_back(ComponentType::New());
    (components[i])->SetSample(sample.GetPointer());
    (components[i])->SetParameters(initialParameters[i]);
    }

  /* Estimating */
  EstimatorType::Pointer estimator = EstimatorType::New();
  estimator->SetSample(sample.GetPointer());
  estimator->SetMaximumIteration(maximumIteration);
  estimator->SetInitialProportions(initialProportions);

  for ( unsigned int i = 0; i < numberOfClasses; i++)
    {
      estimator->AddComponent((ComponentType::Superclass*)
                              (components[i]).GetPointer());
    }

  estimator->Update();

  std::cout << "DEBUG: current iteration = "
            << estimator->GetCurrentIteration() << std::endl;

  bool passed = true;
  for ( unsigned int i = 0; i < numberOfClasses; i++)
    {
    std::cout << "Cluster[" << i << "]" << std::endl;
    std::cout << "    Parameters:" << std::endl;
    std::cout << "         " << (components[i])->GetFullParameters() << std::endl;
    std::cout << "    Proportion: ";
    std::cout << "         " << (estimator->GetProportions())[i] << std::endl;
    double displacement = 0.0;
    const unsigned int measurementVectorSize = sample->GetMeasurementVectorSize();
    for ( unsigned int j = 0; j < measurementVectorSize; ++j )
      {
      double temp;
      temp = (components[i])->GetFullParameters()[j] - trueParameters[i][j];
      displacement += (temp * temp);
      }
    displacement = std::sqrt(displacement);
    std::cout << "    Mean displacement: " << std::endl;
    std::cout << "        " << displacement
              << std::endl << std::endl;
    if ( displacement > (minStandardDeviation / 100.0) * 3 )
      {
      passed = false;
      }
    }

  //Set up a classifier
  typedef itk::Statistics::SampleClassifierFilter< DataSampleType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  typedef FilterType::ClassLabelVectorObjectType               ClassLabelVectorObjectType;
  typedef FilterType::ClassLabelVectorType                     ClassLabelVectorType;

  ClassLabelVectorObjectType::Pointer  classLabelsObject = ClassLabelVectorObjectType::New();

  // Add class labels
  ClassLabelVectorType & classLabelVector  = classLabelsObject->Get();

  typedef FilterType::ClassLabelType        ClassLabelType;

  ClassLabelType  class1 = 0;
  classLabelVector.push_back( class1 );

  ClassLabelType  class2 = 1;
  classLabelVector.push_back( class2 );

  //Set a decision rule type
  typedef itk::Statistics::MaximumDecisionRule  DecisionRuleType;

  DecisionRuleType::Pointer    decisionRule = DecisionRuleType::New();

  const FilterType::MembershipFunctionVectorObjectType *
                membershipFunctionsObject = estimator->GetOutput();

  /* Print out estimated parameters of the membership function */

  const FilterType::MembershipFunctionVectorType
            membershipFunctions = membershipFunctionsObject->Get();

  FilterType::MembershipFunctionVectorType::const_iterator
                    begin = membershipFunctions.begin();

  FilterType::MembershipFunctionVectorType::const_iterator
                    end = membershipFunctions.end();

  FilterType::MembershipFunctionVectorType::const_iterator functionIter;

  functionIter=begin;

  unsigned int counter=1;
  std::cout << "Estimator membership function output " << std::endl;
  while( functionIter != end )
    {
    FilterType::MembershipFunctionPointer membershipFunction = *functionIter;
    const EstimatorType::GaussianMembershipFunctionType *
          gaussianMemberShpFunction =
        dynamic_cast<const EstimatorType::GaussianMembershipFunctionType*>(membershipFunction.GetPointer());
    std::cout << "\tMembership function:\t " << counter << std::endl;
    std::cout << "\t\tMean="<< gaussianMemberShpFunction->GetMean() << std::endl;
    std::cout << "\t\tCovariance matrix=" << gaussianMemberShpFunction->GetCovariance() << std::endl;
    functionIter++;
    counter++;
    }

  //Set membership functions weight array
  const FilterType::MembershipFunctionsWeightsArrayObjectType *
            weightArrayObjects = estimator->GetMembershipFunctionsWeightsArray();
  const FilterType::MembershipFunctionsWeightsArrayType  weightsArray = weightArrayObjects->Get();

  std::cout << "Estimator membership function Weight/proporation output: " << std::endl;
  for( unsigned int i=0; i < weightsArray.Size(); i++ )
    {
    std::cout << "Membership function: \t" << i << "\t" << weightsArray[i] << std::endl;
    }

  char * targetFileName = argv[2];
  std::ifstream dataTargetStream(targetFileName);
  if ( !dataTargetStream )
    {
    std::cout << "ERROR: fail to open the target data file." << std::endl;
    return EXIT_FAILURE;
    }

  PointSetType::Pointer pointSet2 = PointSetType::New();
  PointSetType::PointsContainerPointer pointsContainer2 =
    PointSetType::PointsContainer::New();
  const int dataSizeSmall = 200;
  pointsContainer2->Reserve(dataSizeSmall);
  pointSet2->SetPoints(pointsContainer2.GetPointer());

  p_iter = pointsContainer2->Begin();
  while (p_iter != pointsContainer2->End())
    {
    for ( unsigned int i = 0; i < PointSetType::PointDimension; i++)
      {
      double temp;
      dataTargetStream >> temp;
      point[i] = temp;
      }
    p_iter.Value() = point;
    ++p_iter;
    }

  dataTargetStream.close();

  /* Importing the point set to the sample */
  DataSampleType::Pointer sampleTarget = DataSampleType::New();

  sampleTarget->SetPointSet(pointSet2.GetPointer());

  filter->SetInput( sample );
  filter->SetNumberOfClasses( numberOfClasses );
  filter->SetClassLabels( classLabelsObject );
  filter->SetDecisionRule( decisionRule );
  filter->SetMembershipFunctions( membershipFunctionsObject );
  filter->SetMembershipFunctionsWeightsArray( weightArrayObjects );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  //Check if the measurement vectors are correctly labelled.
  const FilterType::MembershipSampleType* membershipSample = filter->GetOutput();
  FilterType::MembershipSampleType::ConstIterator iter = membershipSample->Begin();

  unsigned int sampleCounter = 0;

  unsigned int numberOfSamplesPerClass = 100;
  if( sampleCounter > numberOfSamplesPerClass )
    {
    if( iter.GetClassLabel() != class1 )
      {
      std::cerr << "Classification error: " << sampleCounter
                << "\t" << iter.GetMeasurementVector()
                << "\t" << "Class label= " << iter.GetClassLabel()
                << "\tTrue label=" << class1 << std::endl;
      return EXIT_FAILURE;
      }
    ++iter;
    ++sampleCounter;
    }

  if( sampleCounter > numberOfSamplesPerClass )
    {
    if( iter.GetClassLabel() != class1 )
      {
      std::cerr << "Classification error: " << sampleCounter
                << "\t" << iter.GetMeasurementVector()
                << "\t" << "Class label= " << iter.GetClassLabel()
                << "\tTrue label=" << class1 << std::endl;
      return EXIT_FAILURE;
      }
    ++iter;
    ++sampleCounter;
    }

  if( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
