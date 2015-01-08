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
#include "itkMaximumDecisionRule.h"
#include "itkDistanceToCentroidMembershipFunction.h"


// ADD DistanceToCentroidMembershipFunction (with the added SetDistanceMetric() method
// ADD EuclideanDistanceMetri
// Create two classes with their respective DistanceToCentroidMembershipFunction and two separate centroids
// ADD MinimumDecisionRule
// Run that classification.

int itkSampleClassifierFilterTest1( int, char * [] )
{

  const unsigned int numberOfComponents = 3;
  typedef float      MeasurementType;

  const unsigned int numberOfClasses = 3;

  typedef itk::Array< MeasurementType > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  typedef itk::Statistics::SampleClassifierFilter< SampleType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( numberOfComponents );

  // Test GetInput() before setting the input
  if( filter->GetInput() != ITK_NULLPTR )
    {
    std::cerr << "GetInput() should have returned ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetOutput() before creating the output
  if( filter->GetOutput() == ITK_NULLPTR )
    {
    std::cerr << "GetOutput() should have returned NON-ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  //Add measurement vectors
  MeasurementVectorType v1(numberOfComponents);
  v1[0] = 0;
  v1[1] = 0;
  v1[2] = 0;
  sample->PushBack( v1 );

  MeasurementVectorType v2(numberOfComponents);
  v2[0] = 1;
  v2[1] = 1;
  v2[2] = 1;
  sample->PushBack( v2 );

  MeasurementVectorType v3(numberOfComponents);
  v3[0] = 2;
  v3[1] = 2;
  v3[2] = 2;
  sample->PushBack( v3 );


  filter->SetInput( sample );

  if( filter->GetInput() != sample.GetPointer() )
    {
    std::cerr << "GetInput() didn't matched SetInput()" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetNumberOfClasses( numberOfClasses );

  if( filter->GetNumberOfClasses() != numberOfClasses )
    {
    std::cerr << "GetNumberOfClasses() didn't matched SetNumberOfClasses()" << std::endl;
    return EXIT_FAILURE;
    }

  typedef FilterType::ClassLabelVectorObjectType               ClassLabelVectorObjectType;
  typedef FilterType::ClassLabelVectorType                     ClassLabelVectorType;
  typedef FilterType::MembershipFunctionVectorObjectType       MembershipFunctionVectorObjectType;
  typedef FilterType::MembershipFunctionVectorType             MembershipFunctionVectorType;

  typedef itk::Statistics::DistanceToCentroidMembershipFunction< MeasurementVectorType >
                                                               MembershipFunctionType;

  typedef MembershipFunctionType::Pointer                      MembershipFunctionPointer;

  ClassLabelVectorObjectType::Pointer  classLabelsObject = ClassLabelVectorObjectType::New();
  filter->SetClassLabels( classLabelsObject );

  MembershipFunctionVectorObjectType::Pointer membershipFunctionsObject =
                                        MembershipFunctionVectorObjectType::New();
  filter->SetMembershipFunctions( membershipFunctionsObject );

  //Run the filter without specifying any membership functions. An exception
  //should be thrown since there will be a mismatch between the number of classes
  //and membership functions
  try
    {
    filter->Update();
    std::cerr << "Attempting to run a classification with unequal"
              << " number of membership functions and number of classes,"
              << " should throw an exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    }


  // Add three membership functions and rerun the filter
  MembershipFunctionVectorType &  membershipFunctionsVector = membershipFunctionsObject->Get();

  MembershipFunctionPointer membershipFunction1 = MembershipFunctionType::New();
  membershipFunction1->SetMeasurementVectorSize( numberOfComponents );
  MembershipFunctionType::CentroidType    centroid1;
  itk::NumericTraits<MembershipFunctionType::CentroidType>::SetLength( centroid1,
    numberOfComponents );
  membershipFunction1->SetCentroid( centroid1 );
  membershipFunctionsVector.push_back( membershipFunction1.GetPointer() );

  MembershipFunctionPointer membershipFunction2 = MembershipFunctionType::New();
  membershipFunction1->SetMeasurementVectorSize( numberOfComponents );
  MembershipFunctionType::CentroidType    centroid2;
  itk::NumericTraits<MembershipFunctionType::CentroidType>::SetLength( centroid2,
    numberOfComponents );
  membershipFunction2->SetCentroid( centroid2 );
  membershipFunctionsVector.push_back( membershipFunction2.GetPointer() );

  MembershipFunctionPointer membershipFunction3 = MembershipFunctionType::New();
  membershipFunction3->SetMeasurementVectorSize( numberOfComponents );
  MembershipFunctionType::CentroidType    centroid3;
  itk::NumericTraits<MembershipFunctionType::CentroidType>::SetLength( centroid3,
    numberOfComponents );
  membershipFunction3->SetCentroid( centroid3 );
  membershipFunctionsVector.push_back( membershipFunction3.GetPointer() );

  try
    {
    filter->Update();
    std::cerr << "Attempting to run a classification with unequal"
              << " number of class labels and number of classes,"
              << " should throw an exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    }


  // Add three class labels and rerun the filter
  ClassLabelVectorType & classLabelVector  = classLabelsObject->Get();

  typedef FilterType::ClassLabelType        ClassLabelType;

  ClassLabelType  class1 = 0;
  classLabelVector.push_back( class1 );

  ClassLabelType  class2 = 1;
  classLabelVector.push_back( class2 );

  ClassLabelType  class3 = 2;
  classLabelVector.push_back( class3 );


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

  //Set a decision rule type
  typedef itk::Statistics::MaximumDecisionRule  DecisionRuleType;

  DecisionRuleType::Pointer    decisionRule = DecisionRuleType::New();
  filter->SetDecisionRule( decisionRule );

  if( filter->GetDecisionRule() != decisionRule )
    {
    std::cerr << "Get/Set Decision rule error! " << std::endl;
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

  // Test GetOutput() after creating the output
  if( filter->GetOutput() == ITK_NULLPTR )
    {
    std::cerr << "GetOutput() should have returned NON-ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
