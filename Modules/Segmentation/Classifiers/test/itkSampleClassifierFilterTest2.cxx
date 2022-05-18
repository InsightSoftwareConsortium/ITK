/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkMaximumDecisionRule.h"
#include "itkGaussianMembershipFunction.h"
#include "itkNormalVariateGenerator.h"


// Test if the SampleClassifier filter labels observations correctly
int
itkSampleClassifierFilterTest2(int, char *[])
{

  constexpr unsigned int numberOfComponents = 1;
  using MeasurementType = float;

  constexpr unsigned int numberOfClasses = 2;

  using MeasurementVectorType = itk::Array<MeasurementType>;
  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;
  using FilterType = itk::Statistics::SampleClassifierFilter<SampleType>;

  auto filter = FilterType::New();

  auto sample = SampleType::New();
  sample->SetMeasurementVectorSize(numberOfComponents);

  filter->SetNumberOfClasses(numberOfClasses);

  if (filter->GetNumberOfClasses() != numberOfClasses)
  {
    std::cerr << "GetNumberOfClasses() didn't matched SetNumberOfClasses()" << std::endl;
    return EXIT_FAILURE;
  }

  using ClassLabelVectorObjectType = FilterType::ClassLabelVectorObjectType;
  using ClassLabelVectorType = FilterType::ClassLabelVectorType;
  using MembershipFunctionVectorObjectType = FilterType::MembershipFunctionVectorObjectType;
  using MembershipFunctionVectorType = FilterType::MembershipFunctionVectorType;

  using MembershipFunctionType = itk::Statistics::GaussianMembershipFunction<MeasurementVectorType>;
  using MeanVectorType = MembershipFunctionType::MeanVectorType;
  using CovarianceMatrixType = MembershipFunctionType::CovarianceMatrixType;

  using MembershipFunctionPointer = MembershipFunctionType::Pointer;

  auto classLabelsObject = ClassLabelVectorObjectType::New();
  filter->SetClassLabels(classLabelsObject);

  auto membershipFunctionsObject = MembershipFunctionVectorObjectType::New();
  filter->SetMembershipFunctions(membershipFunctionsObject);
  // Add three membership functions and rerun the filter
  MembershipFunctionVectorType & membershipFunctionsVector = membershipFunctionsObject->Get();

  MembershipFunctionPointer membershipFunction1 = MembershipFunctionType::New();
  membershipFunction1->SetMeasurementVectorSize(numberOfComponents);
  MeanVectorType mean1;
  itk::NumericTraits<MeanVectorType>::SetLength(mean1, numberOfComponents);
  mean1[0] = 10.5;

  membershipFunction1->SetMean(mean1);
  CovarianceMatrixType covariance1;
  covariance1.SetSize(numberOfComponents, numberOfComponents);
  covariance1.SetIdentity();
  covariance1[0][0] = 0.5;
  membershipFunction1->SetCovariance(covariance1);
  membershipFunctionsVector.push_back(membershipFunction1);

  MembershipFunctionPointer membershipFunction2 = MembershipFunctionType::New();
  membershipFunction1->SetMeasurementVectorSize(numberOfComponents);

  MeanVectorType mean2;
  itk::NumericTraits<MeanVectorType>::SetLength(mean2, numberOfComponents);
  mean2[0] = 200.5;
  membershipFunction2->SetMean(mean2);

  CovarianceMatrixType covariance2;
  covariance2.SetSize(numberOfComponents, numberOfComponents);
  covariance2.SetIdentity();
  covariance2[0][0] = 0.5;
  membershipFunction2->SetCovariance(covariance2);
  membershipFunctionsVector.push_back(membershipFunction2);

  // Add class labels
  ClassLabelVectorType & classLabelVector = classLabelsObject->Get();

  using ClassLabelType = FilterType::ClassLabelType;

  ClassLabelType class1 = 0;
  classLabelVector.push_back(class1);

  ClassLabelType class2 = 1;
  classLabelVector.push_back(class2);

  // Set a decision rule type
  using DecisionRuleType = itk::Statistics::MaximumDecisionRule;

  auto decisionRule = DecisionRuleType::New();
  filter->SetDecisionRule(decisionRule);

  // Generate samples from a Gaussian distribution with mean and
  // covariance parameter as the first membership function.
  // All the samples should be labeled by the classifier as
  // the first class

  using NormalGeneratorType = itk::Statistics::NormalVariateGenerator;
  auto normalGenerator = NormalGeneratorType::New();

  normalGenerator->Initialize(101);

  MeasurementVectorType mv;
  itk::NumericTraits<MeasurementVectorType>::SetLength(mv, numberOfComponents);
  double       mean = mean1[0];
  double       standardDeviation = std::sqrt(covariance1[0][0]);
  unsigned int numberOfSampleEachClass = 10;
  for (unsigned int i = 0; i < numberOfSampleEachClass; ++i)
  {
    mv[0] = (normalGenerator->GetVariate() * standardDeviation) + mean;
    sample->PushBack(mv);
  }

  // Add samples for the second gaussian
  mean = mean2[0];
  standardDeviation = std::sqrt(covariance1[0][0]);
  for (unsigned int i = 0; i < numberOfSampleEachClass; ++i)
  {
    mv[0] = (normalGenerator->GetVariate() * standardDeviation) + mean;
    sample->PushBack(mv);
  }

  filter->SetInput(sample);

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Check if the measurement vectors are correctly labelled.
  const FilterType::MembershipSampleType *        membershipSample = filter->GetOutput();
  FilterType::MembershipSampleType::ConstIterator iter = membershipSample->Begin();

  unsigned int sampleCounter = 0;
  while (iter != membershipSample->End())
  {
    if (sampleCounter < numberOfSampleEachClass)
    {
      if (iter.GetClassLabel() != class1)
      {
        std::cerr << "Classification error: " << sampleCounter << "\t" << iter.GetClassLabel() << "\tclass1=" << class1
                  << std::endl;
        return EXIT_FAILURE;
      }
    }
    else
    {
      if (iter.GetClassLabel() != class2)
      {
        std::cerr << "Classification error: " << sampleCounter << "\t" << iter.GetClassLabel() << "\tclass2=" << class2
                  << std::endl;
        return EXIT_FAILURE;
      }
    }
    ++iter;
    ++sampleCounter;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
