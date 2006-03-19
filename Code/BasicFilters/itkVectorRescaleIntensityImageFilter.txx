/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorRescaleIntensityImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVectorRescaleIntensityImageFilter_txx
#define _itkVectorRescaleIntensityImageFilter_txx

#include "itkVectorRescaleIntensityImageFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
VectorRescaleIntensityImageFilter<TInputImage, TOutputImage>
::VectorRescaleIntensityImageFilter()
{
  m_OutputMaximumMagnitude   = NumericTraits< OutputRealType >::Zero;
  m_InputMaximumMagnitude    = NumericTraits< InputRealType  >::Zero;
  
  m_Scale = 1.0;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorRescaleIntensityImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Output Maximum Magnitude: "
     << static_cast<typename NumericTraits<OutputRealType>::PrintType>(m_OutputMaximumMagnitude)
     << std::endl;
  os << indent << "Input Maximum Magnitude: "
     << static_cast<typename NumericTraits<InputRealType>::PrintType>(m_InputMaximumMagnitude)
     << std::endl;
  os << indent << "Internal Scale : "
     << static_cast<typename NumericTraits<InputRealType>::PrintType>(m_Scale)
     << std::endl;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
VectorRescaleIntensityImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{

  if ( m_OutputMaximumMagnitude < NumericTraits<OutputRealType>::Zero )
    {
    itkExceptionMacro(<<"Maximum output value cannot be negative. You are passing " << m_OutputMaximumMagnitude);
    return;
    }

  typedef  typename Superclass::InputImageType      InputImageType;
  typedef  typename Superclass::InputImagePointer   InputImagePointer;

  InputImagePointer inputImage =   this->GetInput();

  typedef ImageRegionConstIterator< InputImageType >  InputIterator;

  InputIterator it( inputImage, inputImage->GetBufferedRegion() );

  it.GoToBegin();

  InputRealType maximumSquaredMagnitude = NumericTraits<InputRealType>::Zero;

  while( !it.IsAtEnd() )
    {
    InputRealType magnitude = it.Get().GetSquaredNorm();
    if( magnitude > maximumSquaredMagnitude )
      {
      maximumSquaredMagnitude = magnitude;
      }
    ++it;
    }

  m_InputMaximumMagnitude = vcl_sqrt(maximumSquaredMagnitude );

  m_Scale = static_cast<InputRealType>( m_OutputMaximumMagnitude ) /
            static_cast<InputRealType>( m_InputMaximumMagnitude  ) ;
  
  // set up the functor values
  this->GetFunctor().SetFactor( m_Scale );
  
}


} // end namespace itk

#endif
