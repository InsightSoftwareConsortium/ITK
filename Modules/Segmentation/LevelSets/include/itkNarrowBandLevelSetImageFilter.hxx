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
#ifndef itkNarrowBandLevelSetImageFilter_hxx
#define itkNarrowBandLevelSetImageFilter_hxx

#include "itkNarrowBandLevelSetImageFilter.h"
#include <cstdio>
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType, typename TOutputImage >
void
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_ReverseExpansionDirection = " << m_ReverseExpansionDirection << std::endl;
  os << indent << "m_SegmentationFunction = " << m_SegmentationFunction << std::endl;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType, typename TOutputImage >
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::NarrowBandLevelSetImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  //this->SetNarrowBandInnerRadius();
  //this->SetNarrowBandTotalRadius();
  m_SegmentationFunction = ITK_NULLPTR;

  m_IsoFilter = IsoFilterType::New();
  m_ChamferFilter = ChamferFilterType::New();

  // Provide some reasonable defaults which will at least prevent infinite
  // looping.
  this->SetMaximumRMSError(0.02);
  this->SetNumberOfIterations(1000);
  m_ReverseExpansionDirection = false;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType, typename TOutputImage >
void
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::SetSegmentationFunction(SegmentationFunctionType *s)
{
  unsigned int i;

  m_SegmentationFunction = s;

  typename SegmentationFunctionType::RadiusType r;
  for ( i = 0; i < Self::ImageDimension; ++i )
    {
    r[i] = 1;
    }

  m_SegmentationFunction->Initialize(r);
  this->SetDifferenceFunction(m_SegmentationFunction);
  this->Modified();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType, typename TOutputImage >
void
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::GenerateData()
{
  if ( m_SegmentationFunction == ITK_NULLPTR )
        { itkExceptionMacro("No finite difference function was specified."); }

  // A positive speed value causes surface expansion, the opposite of the
  // default.  Flip the sign of the propagation and advection weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }

  // Allocate the images from which speeds will be sampled.
  if ( Math::NotExactlyEquals(this->GetSegmentationFunction()->GetPropagationWeight(), 0) )
    {
    m_SegmentationFunction->AllocateSpeedImage();
    m_SegmentationFunction->CalculateSpeedImage();
    }
  if ( Math::NotExactlyEquals(this->GetSegmentationFunction()->GetAdvectionWeight(), 0) )
    {
    m_SegmentationFunction->AllocateAdvectionImage();
    m_SegmentationFunction->CalculateAdvectionImage();
    }

  // Start the solver
  Superclass::GenerateData();

  // Reset all the signs of the weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType, typename TOutputImage >
void
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::CreateNarrowBand()
{
  typename OutputImageType::Pointer levelset = this->GetOutput();

  if ( !this->m_NarrowBand->Empty() )
    {
    m_IsoFilter->SetNarrowBand( this->m_NarrowBand.GetPointer() );
    m_IsoFilter->NarrowBandingOn(); //Maybe we should check that the NarrowBand
                                    // exits.
    }
  else
    {
    m_IsoFilter->NarrowBandingOff();
    }

  m_IsoFilter->SetFarValue(this->m_NarrowBand->GetTotalRadius() + 1);
  m_IsoFilter->SetInput(levelset);
  m_IsoFilter->Update();

  m_ChamferFilter->SetInput( m_IsoFilter->GetOutput() );
  m_ChamferFilter->SetMaximumDistance(this->m_NarrowBand->GetTotalRadius() + 1);
  m_ChamferFilter->SetNarrowBand( this->m_NarrowBand.GetPointer() );
  m_ChamferFilter->Update();

  this->GraftOutput( m_ChamferFilter->GetOutput() );
  m_IsoFilter->SetInput(ITK_NULLPTR);
  m_ChamferFilter->SetInput(ITK_NULLPTR);
}
} // end namespace itk

#endif
