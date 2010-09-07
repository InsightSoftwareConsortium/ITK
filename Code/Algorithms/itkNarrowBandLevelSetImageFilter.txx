/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBandLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNarrowBandLevelSetImageFilter_txx
#define __itkNarrowBandLevelSetImageFilter_txx

#include "itkNarrowBandLevelSetImageFilter.h"
#include "stdio.h"

namespace itk
{
template< class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage >
void
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_ReverseExpansionDirection = " << m_ReverseExpansionDirection << std::endl;
  os << indent << "m_SegmentationFunction = " << m_SegmentationFunction << std::endl;
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage >
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::NarrowBandLevelSetImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  //this->SetNarrowBandInnerRadius();
  //this->SetNarrowBandTotalRadius();
  m_SegmentationFunction = 0;

  m_IsoFilter = IsoFilterType::New();
  m_ChamferFilter = ChamferFilterType::New();

  // Provide some reasonable defaults which will at least prevent infinite
  // looping.
  this->SetMaximumRMSError(0.02);
  this->SetNumberOfIterations(1000);
  m_ReverseExpansionDirection = false;
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage >
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

template< class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage >
void
NarrowBandLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType, TOutputImage >
::GenerateData()
{
  if ( m_SegmentationFunction == 0 )
        { itkExceptionMacro("No finite difference function was specified."); }

  // A positive speed value causes surface expansion, the opposite of the
  // default.  Flip the sign of the propagation and advection weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }

  // Allocate the images from which speeds will be sampled.
  if ( this->GetSegmentationFunction()->GetPropagationWeight() != 0 )
    {
    m_SegmentationFunction->AllocateSpeedImage();
    m_SegmentationFunction->CalculateSpeedImage();
    }
  if ( this->GetSegmentationFunction()->GetAdvectionWeight() != 0 )
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

template< class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage >
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
  m_IsoFilter->SetInput(NULL);
  m_ChamferFilter->SetInput(NULL);
}
} // end namespace itk

#endif
