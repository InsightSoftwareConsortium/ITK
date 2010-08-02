/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSampleClassifierFilterTest2.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkArray.h"
#include "itkVariableLengthVector.h"
#include "itkListSample.h"
#include "itkSampleClassifierFilter.h"
#include "itkMaximumDecisionRule2.h"
#include "itkGaussianMembershipFunction.h"
#include "itkNormalVariateGenerator.h"


//Test if the SampleClassifier filter labels observations correctly
int itkSampleClassifierFilterTest2( int, char * [] )
{

  const unsigned int numberOfComponents = 1;
  typedef float      MeasurementType;

  const unsigned int numberOfClasses = 2;

  typedef itk::Array< MeasurementType >                         MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType >  SampleType;
  typedef itk::Statistics::SampleClassifierFilter< SampleType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( numberOfComponents );

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

  typedef itk::Statistics::GaussianMembershipFunction< MeasurementVectorType >
                                                               MembershipFunctionType;
  typedef MembershipFunctionType::MeanType                     MeanType;
  typedef MembershipFunctionType::CovarianceType               CovarianceType;

  typedef MembershipFunctionType::Pointer                      MembershipFunctionPointer;

  ClassLabelVectorObjectType::Pointer  classLabelsObject = ClassLabelVectorObjectType::New();
  filter->SetClassLabels( classLabelsObject );

  MembershipFunctionVectorObjectType::Pointer membershipFunctionsObject =
                                        MembershipFunctionVectorObjectType::New();
  filter->SetMembershipFunctions( membershipFunctionsObject );
  // Add three membership functions and rerun the filter
  MembershipFunctionVectorType &  membershipFunctionsVector = membershipFunctionsObject->Get();

  MembershipFunctionPointer membershipFunction1 = MembershipFunctionType::New();
  membershipFunction1->SetMeasurementVectorSize( numberOfComponents );
  MeanType  mean1;
  itk::Statistics::MeasurementVectorTraits::SetLength( mean1, numberOfComponents );
  mean1[0] = 10.5;

  membershipFunction1->SetMean( mean1 );
  CovarianceType covariance1;
  covariance1.SetSize( numberOfComponents, numberOfComponents );
  covariance1.SetIdentity();
  covariance1[0][0] = 0.5;
  membershipFunction1->SetCovariance( covariance1 );
  membershipFunctionsVector.push_back( membershipFunction1.GetPointer() );

  MembershipFunctionPointer membershipFunction2 = MembershipFunctionType::New();
  membershipFunction1->SetMeasurementVectorSize( numberOfComponents );

  MeanType  mean2;
  itk::Statistics::MeasurementVectorTraits::SetLength( mean2, numberOfComponents );
  mean2[0] = 200.5;
  membershipFunction2->SetMean( mean2 );

  CovarianceType covariance2;
  covariance2.SetSize( numberOfComponents, numberOfComponents );
  covariance2.SetIdentity();
  covariance2[0][0] = 0.5;
  membershipFunction2->SetCovariance( covariance2 );
  membershipFunctionsVector.push_back( membershipFunction2.GetPointer() );

  // Add class labels
  ClassLabelVectorType & classLabelVector  = classLabelsObject->Get();

  typedef FilterType::ClassLabelType        ClassLabelType;

  ClassLabelType  class1 = 0;
  classLabelVector.push_back( class1 );

  ClassLabelType  class2 = 1;
  classLabelVector.push_back( class2 );

  //Set a decision rule type
  typedef itk::Statistics::MaximumDecisionRule2  DecisionRuleType;

  DecisionRuleType::Pointer    decisionRule = DecisionRuleType::New();
  filter->SetDecisionRule( decisionRule );

  //Generate samples from a Gaussian distribution with mean and
  //covariance parameter as the first membership function.
  //All the samples should be labeled by the classifier as
  //the first class

  typedef itk::Statistics::NormalVariateGenerator NormalGeneratorType;
  NormalGeneratorType::Pointer normalGenerator = NormalGeneratorType::New();

  normalGenerator->Initialize( 101 );

  MeasurementVectorType mv;
  itk::Statistics::MeasurementVectorTraits::SetLength( mv, numberOfComponents );
  double mean = mean1[0];
  double standardDeviation = vcl_sqrt(covariance1[0][0]);
  unsigned int numberOfSampleEachClass = 10;
  for ( unsigned int i = 0; i < numberOfSampleEachClass; ++i )
    {
    mv[0] = (normalGenerator->GetVariate() * standardDeviation ) + mean;
    sample->PushBack( mv );
    }

  //Add samples for the second gaussian
  mean = mean2[0];
  standardDeviation = vcl_sqrt(covariance1[0][0]);
  for ( unsigned int i = 0; i < numberOfSampleEachClass; ++i )
    {
    mv[0] = (normalGenerator->GetVariate() * standardDeviation ) + mean;
    sample->PushBack( mv );
    }

  filter->SetInput( sample );

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
                  << "\t" << iter.GetClassLabel()
                  << "\tclass1=" << class1 << std::endl;
        return EXIT_FAILURE;
        }
      }
    else
      {
      if( iter.GetClassLabel() != class2 )
        {
        std::cerr << "Classification error: " << sampleCounter
                  << "\t" << iter.GetClassLabel()
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
