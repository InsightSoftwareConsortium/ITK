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
#ifndef itkImageClassifierFilter_hxx
#define itkImageClassifierFilter_hxx

#include "itkImageClassifierFilter.h"
#include "itkImageRegionIterator.h"

namespace itk
{
namespace Statistics
{
template< typename TSample, typename TInputImage, typename TOutputImage >
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::ImageClassifierFilter()
{
  this->m_NumberOfClasses = 0;
  this->SetNumberOfRequiredInputs(3);
  this->SetNumberOfRequiredOutputs(1);

  /** Initialize decision rule */
  m_DecisionRule = ITK_NULLPTR;

  m_NumberOfClasses = 0;
}

template< typename TSample, typename TInputImage, typename TOutputImage >
void
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of classes: "
     << this->GetNumberOfClasses()
     << std::endl;
  os << indent << "Decision Rule: "
     << this->GetDecisionRule()
     << std::endl;
  os << indent << "Image: "
     << this->GetImage()
     << std::endl;
}

template< typename TSample, typename TInputImage, typename TOutputImage >
void
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::SetImage(const InputImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( image ) );
}

template< typename TSample, typename TInputImage, typename TOutputImage >
const TInputImage *
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::GetImage() const
{
  return itkDynamicCastInDebugMode< const TInputImage * >( this->GetPrimaryInput() );
}

template< typename TSample, typename TInputImage, typename TOutputImage >
void
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::SetClassLabels(const ClassLabelVectorObjectType *classLabels)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< ClassLabelVectorObjectType * >( classLabels ) );
}

template< typename TSample, typename TInputImage, typename TOutputImage >
void
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::SetMembershipFunctions(const MembershipFunctionVectorObjectType *membershipFunctions)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 2,
                                    const_cast< MembershipFunctionVectorObjectType * >( membershipFunctions ) );
}

template< typename TSample, typename TInputImage, typename TOutputImage >
void
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::SetMembershipFunctionsWeightsArray(const
                                     MembershipFunctionsWeightsArrayObjectType *weightsArray)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 3,
                                    const_cast<
                                      MembershipFunctionsWeightsArrayObjectType * >( weightsArray ) );
}

template< typename TSample, typename TInputImage, typename TOutputImage >
void
ImageClassifierFilter< TSample, TInputImage, TOutputImage >
::GenerateData()
{
  const ClassLabelVectorObjectType *classLabelsDecorated =
    static_cast< const ClassLabelVectorObjectType * >( this->ProcessObject::GetInput(1) );

  const MembershipFunctionVectorObjectType *membershipFunctionsDecorated =
    static_cast< const MembershipFunctionVectorObjectType * >( this->ProcessObject::GetInput(2) );

  const MembershipFunctionsWeightsArrayObjectType *
  membershipFunctionsWeightsArrayDecorated =
    static_cast< const MembershipFunctionsWeightsArrayObjectType * >( this->ProcessObject::GetInput(3) );

  const ClassLabelVectorType & classLabels = classLabelsDecorated->Get();

  const MembershipFunctionVectorType & membershipFunctions = membershipFunctionsDecorated->Get();

  // Check number of Labels and MembershipSamples against the number of classes
  // */
  if ( membershipFunctions.size() != this->m_NumberOfClasses )
    {
    itkExceptionMacro("Number of Membership functions does not match the number of classes");
    }

  if ( classLabels.size() != this->m_NumberOfClasses )
    {
    itkExceptionMacro("Number of class labels does not match the number of classes");
    }

  if ( m_DecisionRule.IsNull() )
    {
    itkExceptionMacro("Decision rule is not set");
    }

  MembershipFunctionsWeightsArrayType membershipFunctionsWeightsArray;
  if ( membershipFunctionsWeightsArrayDecorated == ITK_NULLPTR )
    {
    // no weights array is set and hence all membership functions will have
    // equal
    // weight
    membershipFunctionsWeightsArray.SetSize(this->m_NumberOfClasses);
    membershipFunctionsWeightsArray.Fill(1.0);
    }
  else
    {
    membershipFunctionsWeightsArray = membershipFunctionsWeightsArrayDecorated->Get();
    }

  if ( membershipFunctionsWeightsArray.Size() != this->m_NumberOfClasses
       )
    {
    itkExceptionMacro(
      "Membership functions weight array size does not match the\
                      number of classes "                                                                  );
    }

  const InputImageType *inputImage =
    static_cast< const InputImageType * >( this->ProcessObject::GetInput(0) );

  std::vector< double > discriminantScores;
  discriminantScores.resize(this->m_NumberOfClasses);

  OutputImageType *outputImage = dynamic_cast< OutputImageType * >(
    this->ProcessObject::GetOutput(0) );

  outputImage->CopyInformation(inputImage);
  outputImage->SetRegions( inputImage->GetBufferedRegion() );
  outputImage->Allocate();

  ImageRegionConstIterator< InputImageType > inpItr( inputImage, inputImage->GetBufferedRegion() );
  ImageRegionIterator< OutputImageType >     outItr( outputImage, outputImage->GetBufferedRegion() );

  inpItr.GoToBegin();
  outItr.GoToBegin();

  while ( !inpItr.IsAtEnd() )
    {
    MeasurementVectorType measurements;

    MeasurementVectorTraits::Assign( measurements, inpItr.Get() );

    for ( unsigned int i = 0; i < this->m_NumberOfClasses; i++ )
      {
      discriminantScores[i] = membershipFunctionsWeightsArray[i]
                              * membershipFunctions[i]->Evaluate(measurements);
      }

    unsigned int classIndex;
    classIndex = static_cast<unsigned int>( m_DecisionRule->Evaluate(discriminantScores) );

    OutputPixelType value = static_cast< OutputPixelType >( classLabels[classIndex] );
    outItr.Set(value);

    ++inpItr;
    ++outItr;
    }
}
} // end of namespace Statistics
} // end of namespace itk

#endif
