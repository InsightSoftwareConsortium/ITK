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

#include "itkListSample.h"
#include "itkSampleClassifierFilter.h"
#include "itkMinimumDecisionRule.h"
#include "itkNormalVariateGenerator.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkWeightedCentroidKdTreeGenerator.h"


//run sample classifer using itk::VariableLengthVector type measurment vector
int itkSampleClassifierFilterTest5( int, char * [] )
{

  const unsigned int numberOfComponents = 1;
  typedef float      MeasurementType;

  const unsigned int numberOfClasses = 2;

  typedef itk::VariableLengthVector< MeasurementType > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  typedef itk::Statistics::SampleClassifierFilter< SampleType > FilterType;

  typedef itk::Statistics::WeightedCentroidKdTreeGenerator< SampleType > GeneratorType;
  typedef itk::Statistics::KdTreeBasedKmeansEstimator< GeneratorType::KdTreeType > EstimatorType;

  //Generate a sample list
  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( numberOfComponents );

  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New();
  normalGenerator->Initialize( 101 );

  //Populate the list with samples from two normal distributions

  EstimatorType::DistanceToCentroidMembershipFunctionType::CentroidType  mean1;

  itk::NumericTraits<
    EstimatorType::DistanceToCentroidMembershipFunctionType::CentroidType>::SetLength(
    mean1, numberOfComponents );
  mean1[0] = 10.5;

  EstimatorType::DistanceToCentroidMembershipFunctionType::CentroidType  mean2;
  itk::NumericTraits<
    EstimatorType::DistanceToCentroidMembershipFunctionType::CentroidType>::SetLength(
    mean2, numberOfComponents );
  mean2[0] = 200.5;

  MeasurementVectorType mv;
  itk::NumericTraits<MeasurementVectorType>::SetLength( mv, numberOfComponents );
  double mean = mean1[0];
  double standardDeviation = 0.1;
  unsigned int numberOfSampleEachClass = 10;

  //Add sample from the first gaussian
  for ( unsigned int i = 0; i < numberOfSampleEachClass; ++i )
    {
    mv[0] = (normalGenerator->GetVariate() * standardDeviation ) + mean;
    sample->PushBack( mv );
    }

  //Add samples from the second gaussian
  mean = mean2[0];
  standardDeviation = 0.1;
  for ( unsigned int i = 0; i < numberOfSampleEachClass; ++i )
    {
    mv[0] = (normalGenerator->GetVariate() * standardDeviation ) + mean;
    sample->PushBack( mv );
    }


  typedef FilterType::ClassLabelVectorObjectType               ClassLabelVectorObjectType;
  typedef FilterType::ClassLabelVectorType                     ClassLabelVectorType;

  ClassLabelVectorObjectType::Pointer  classLabelsObject = ClassLabelVectorObjectType::New();

  /* Creating k-d tree */
  GeneratorType::Pointer generator = GeneratorType::New();
  generator->SetSample(sample.GetPointer());
  unsigned int bucketSize = 1;
  generator->SetBucketSize(bucketSize);
  generator->GenerateData();

  /* Searching kmeans */
  EstimatorType::Pointer estimator = EstimatorType::New();

  itk::Array< double > initialMeans(2);
  initialMeans[0] = 5;
  initialMeans[1] = 70;
  estimator->SetParameters(initialMeans);
  unsigned int maximumIteration = 100;
  estimator->SetMaximumIteration(maximumIteration);
  estimator->SetKdTree(generator->GetOutput());
  estimator->SetCentroidPositionChangesThreshold(0.0);
  estimator->StartOptimization();
  //EstimatorType::ParametersType estimatedMeans = estimator->GetParameters();

  // Add class labels
  ClassLabelVectorType & classLabelVector  = classLabelsObject->Get();

  typedef FilterType::ClassLabelType        ClassLabelType;

  ClassLabelType  class1 = 0;
  classLabelVector.push_back( class1 );

  ClassLabelType  class2 = 1;
  classLabelVector.push_back( class2 );

  //Set a decision rule type
  typedef itk::Statistics::MinimumDecisionRule  DecisionRuleType;

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
  while( functionIter != end )
    {
    FilterType::MembershipFunctionPointer membershipFunction = *functionIter;
    const EstimatorType::DistanceToCentroidMembershipFunctionType *
          distanceMemberShpFunction =
        dynamic_cast<const EstimatorType::DistanceToCentroidMembershipFunctionType*>(membershipFunction.GetPointer());
    std::cout << "Centroid of the " << counter << " membership function "
              << distanceMemberShpFunction->GetCentroid() << std::endl;
    functionIter++;
    counter++;
    }

  //Instantiate and pass all the required inputs to the filter
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( sample );
  filter->SetNumberOfClasses( numberOfClasses );
  filter->SetClassLabels( classLabelsObject );
  filter->SetDecisionRule( decisionRule );
  filter->SetMembershipFunctions( membershipFunctionsObject );

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
  while ( iter != membershipSample->End() )
    {
    if( sampleCounter < numberOfSampleEachClass )
      {
      if( iter.GetClassLabel() != class1 )
        {
        std::cerr << "Classification error: " << sampleCounter
                  << "\t" << iter.GetMeasurementVector() << iter.GetClassLabel()
                  << "\tclass1=" << class1 << std::endl;
        return EXIT_FAILURE;
        }
      }
    else
      {
      if( iter.GetClassLabel() != class2 )
        {
        std::cerr << "Classification error: " << sampleCounter
                  << "\t" << iter.GetMeasurementVector() << iter.GetClassLabel()
                  << "\tclass2=" << class2 << std::endl;
        return EXIT_FAILURE;
        }
      }
    ++iter;
    ++sampleCounter;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
