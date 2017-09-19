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
#ifndef itkSegmentationLevelSetImageFilter_hxx
#define itkSegmentationLevelSetImageFilter_hxx

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::SegmentationLevelSetImageFilter()
{
  // #0 "InitialImage" required
  Self::SetPrimaryInputName("InitialImage");

  // #1 "FeatureImage" required
  Self::AddRequiredInputName("FeatureImage", 1);

  this->SetNumberOfRequiredInputs(2);
  this->SetNumberOfLayers(TInputImage::ImageDimension);
  m_SegmentationFunction = ITK_NULLPTR;
  m_AutoGenerateSpeedAdvection = true;
  this->SetIsoSurfaceValue(NumericTraits< ValueType >::ZeroValue());

  // Provide some reasonable defaults which will at least prevent infinite
  // looping.
  this->SetMaximumRMSError(0.02);
  this->SetNumberOfIterations(1000);
  m_ReverseExpansionDirection = false;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateSpeedImage()
{
  m_SegmentationFunction->AllocateSpeedImage();
  m_SegmentationFunction->CalculateSpeedImage();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateAdvectionImage()
{
  m_SegmentationFunction->AllocateAdvectionImage();
  m_SegmentationFunction->CalculateAdvectionImage();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateData()
{
  if ( m_SegmentationFunction == ITK_NULLPTR )
    {
    itkExceptionMacro("No finite difference function was specified.");
    }

  // A positive speed value causes surface expansion, the opposite of the
  // default. Flip the sign of the propagation and advection weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }

  // Allocate the images from which speeds will be sampled.
  // if it is uninitialized and AutoGenerateSpeedAvection is true
  if ( !this->m_IsInitialized && m_AutoGenerateSpeedAdvection == true )
    {
    if ( Math::NotExactlyEquals(this->GetSegmentationFunction()->GetPropagationWeight(), 0) )
      {
      this->GenerateSpeedImage();
      }

    if ( Math::NotExactlyEquals(this->GetSegmentationFunction()->GetAdvectionWeight(), 0) )
      {
      this->GenerateAdvectionImage();
      }
    }

  // Start the solver
  Superclass::GenerateData();

  // Reset all the signs of the weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "m_ReverseExpansionDirection = " << m_ReverseExpansionDirection << std::endl;
  os << indent << "m_AutoGenerateSpeedAdvection = " << m_AutoGenerateSpeedAdvection << std::endl;
  os << indent << "m_SegmentationFunction = " << m_SegmentationFunction << std::endl;
}

} // end namespace itk

#endif
