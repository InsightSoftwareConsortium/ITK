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
#ifndef itkScalarImageKmeansImageFilter_hxx
#define itkScalarImageKmeansImageFilter_hxx

#include "itkScalarImageKmeansImageFilter.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"

#include "itkDistanceToCentroidMembershipFunction.h"

#include "itkProgressReporter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
ScalarImageKmeansImageFilter< TInputImage, TOutputImage >
::ScalarImageKmeansImageFilter() :
  m_UseNonContiguousLabels( false ),
  m_ImageRegionDefined( false )
{
}

template< typename TInputImage, typename TOutputImage >
void ScalarImageKmeansImageFilter< TInputImage, TOutputImage >
::SetImageRegion(const ImageRegionType & region)
{
  m_ImageRegion = region;
  m_ImageRegionDefined = true;
}

template< typename TInputImage, typename TOutputImage >
void
ScalarImageKmeansImageFilter< TInputImage, TOutputImage >
::VerifyPreconditions()
{
  this->Superclass::VerifyPreconditions();

  if ( this->m_InitialMeans.size() == 0 )
    {
    itkExceptionMacro("Atleast One InialMean is required.");
    }
}

template< typename TInputImage, typename TOutputImage >
void
ScalarImageKmeansImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typename AdaptorType::Pointer adaptor = AdaptorType::New();

  // Setup the regions here if a sub-region has been specified to restrict
  // classification on. Since this is not ThreadedGenenerateData, we are
  // safe...
  if ( m_ImageRegionDefined )
    {
    typename RegionOfInterestFilterType::Pointer regionOfInterestFilter =
      RegionOfInterestFilterType::New();
    regionOfInterestFilter->SetRegionOfInterest(m_ImageRegion);
    regionOfInterestFilter->SetInput( this->GetInput() );
    regionOfInterestFilter->Update();
    adaptor->SetImage( regionOfInterestFilter->GetOutput() );
    }
  else
    {
    adaptor->SetImage( this->GetInput() );
    }

  typename TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  treeGenerator->SetSample(adaptor);
  treeGenerator->SetBucketSize(16);
  treeGenerator->Update();

  typename EstimatorType::Pointer estimator = EstimatorType::New();

  const size_t numberOfClasses = this->m_InitialMeans.size();

  ParametersType initialMeans(numberOfClasses);
  for ( unsigned int cl = 0; cl < numberOfClasses; cl++ )
    {
    initialMeans[cl] = this->m_InitialMeans[cl];
    }

  estimator->SetParameters(initialMeans);

  estimator->SetKdTree( treeGenerator->GetOutput() );
  estimator->SetMaximumIteration( 200 );
  estimator->SetCentroidPositionChangesThreshold (0.0 );
  estimator->StartOptimization();

  this->m_FinalMeans = estimator->GetParameters();

  typedef typename InputImageType::RegionType RegionType;

  // Now classify the samples
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();
  typename ClassifierType::Pointer classifier = ClassifierType::New();

  classifier->SetDecisionRule( decisionRule.GetPointer() );
  classifier->SetInput( adaptor );

  classifier->SetNumberOfClasses (numberOfClasses );

  ClassLabelVectorType classLabels;
  classLabels.resize( numberOfClasses );

  // Spread the labels over the intensity range
  unsigned int labelInterval = 1;
  if ( m_UseNonContiguousLabels )
    {
    labelInterval = ( NumericTraits< OutputPixelType >::max() / numberOfClasses ) - 1;
    }

  unsigned int label = 0;
  MembershipFunctionVectorType membershipFunctions;

  for ( unsigned int k = 0; k < numberOfClasses; k++ )
    {
    classLabels[k] = label;
    label += labelInterval;
    MembershipFunctionPointer    membershipFunction = MembershipFunctionType::New();
    MembershipFunctionOriginType origin( adaptor->GetMeasurementVectorSize() );
    origin[0] = this->m_FinalMeans[k]; // A scalar image has a MeasurementVector
                                       // of dimension 1
    membershipFunction->SetCentroid(origin);
    const MembershipFunctionType *constMembershipFunction = membershipFunction;
    membershipFunctions.push_back(constMembershipFunction);
    }

  typename ClassifierType::MembershipFunctionVectorObjectPointer membershipFunctionsObject =
    ClassifierType::MembershipFunctionVectorObjectType::New();
  membershipFunctionsObject->Set(membershipFunctions);
  classifier->SetMembershipFunctions(membershipFunctionsObject);

  typedef typename ClassifierType::ClassLabelVectorObjectType ClassLabelVectorObjectType;
  typename ClassLabelVectorObjectType::Pointer classLabelsObject = ClassLabelVectorObjectType::New();
  classLabelsObject->Set( classLabels );
  classifier->SetClassLabels( classLabelsObject );

  // Execute the actual classification
  classifier->Update();

  // Now classify the pixels
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  typedef ImageRegionIterator< OutputImageType > ImageIterator;

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  RegionType region = outputPtr->GetBufferedRegion();

  // If we constrained the classification to a region, label only pixels within
  // the region. Label outside pixels as numberOfClasses + 1
  if ( m_ImageRegionDefined )
    {
    region = m_ImageRegion;
    }

  ImageIterator pixel( outputPtr, region );
  pixel.GoToBegin();

  typedef typename ClassifierType::MembershipSampleType ClassifierOutputType;
  const ClassifierOutputType *membershipSample = classifier->GetOutput();

  typedef typename ClassifierOutputType::ConstIterator LabelIterator;

  LabelIterator iter = membershipSample->Begin();
  LabelIterator end  = membershipSample->End();

  while ( iter != end )
    {
    pixel.Set( iter.GetClassLabel() );
    ++iter;
    ++pixel;
    }

  if ( m_ImageRegionDefined )
    {
    // If a region is defined to constrain classification to, we need to label
    // pixels outside with numberOfClasses + 1.
    typedef ImageRegionExclusionIteratorWithIndex< OutputImageType >
    ExclusionImageIteratorType;
    ExclusionImageIteratorType exIt( outputPtr, outputPtr->GetBufferedRegion() );
    exIt.SetExclusionRegion( region );
    exIt.GoToBegin();
    if ( m_UseNonContiguousLabels )
      {
      OutputPixelType outsideLabel = labelInterval * numberOfClasses;
      while ( !exIt.IsAtEnd() )
        {
        exIt.Set(outsideLabel);
        ++exIt;
        }
      }
    else
      {
      while ( !exIt.IsAtEnd() )
        {
        exIt.Set(numberOfClasses);
        ++exIt;
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
ScalarImageKmeansImageFilter< TInputImage, TOutputImage >
::AddClassWithInitialMean(RealPixelType mean)
{
  this->m_InitialMeans.push_back(mean);
}

template< typename TInputImage, typename TOutputImage >
void
ScalarImageKmeansImageFilter< TInputImage, TOutputImage >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Final Means " << m_FinalMeans << std::endl;
  os << indent << "Use Contiguous Labels " << m_UseNonContiguousLabels << std::endl;
  os << indent << "Image Region Defined: " << m_ImageRegionDefined << std::endl;
  os << indent << "Image Region: " << m_ImageRegion << std::endl;
}
} // end namespace itk

#endif
