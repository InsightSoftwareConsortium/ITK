/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIntensityWindowingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkIntensityWindowingImageFilter_txx
#define _itkIntensityWindowingImageFilter_txx

#include "itkIntensityWindowingImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
IntensityWindowingImageFilter<TInputImage, TOutputImage>
::IntensityWindowingImageFilter()
{
  m_OutputMaximum   = NumericTraits<OutputPixelType>::max();
  m_OutputMinimum   = NumericTraits<OutputPixelType>::NonpositiveMin();

  m_WindowMaximum   = NumericTraits<InputPixelType>::max();
  m_WindowMinimum   = NumericTraits<InputPixelType>::NonpositiveMin();

  m_Scale = 1.0;
  m_Shift = 0.0;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
IntensityWindowingImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Output Minimum: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutputMinimum)
     << std::endl;
  os << indent << "Output Maximum: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutputMaximum)
     << std::endl;
  os << indent << "Window Minimum: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_WindowMinimum)
     << std::endl;
  os << indent << "Window Maximum: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_WindowMaximum)
     << std::endl;
  os << indent << "Scale Factor: "
     << static_cast<typename NumericTraits<RealType>::PrintType>(m_Scale)
     << std::endl;
  os << indent << "Shift offset: "
     << static_cast<typename NumericTraits<RealType>::PrintType>(m_Shift)
     << std::endl;

}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
IntensityWindowingImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  m_Scale = 
    (static_cast<RealType>( m_OutputMaximum )
   - static_cast<RealType>( m_OutputMinimum )) /
    (static_cast<RealType>( m_WindowMaximum )
   - static_cast<RealType>( m_WindowMinimum ));

  m_Shift =
          static_cast<RealType>( m_OutputMinimum ) - 
          static_cast<RealType>( m_WindowMinimum ) * m_Scale;
  
  // set up the functor values
  this->GetFunctor().SetOutputMinimum( m_OutputMinimum );
  this->GetFunctor().SetOutputMaximum( m_OutputMaximum );
  
  this->GetFunctor().SetWindowMinimum( m_WindowMinimum );
  this->GetFunctor().SetWindowMaximum( m_WindowMaximum );
  
  this->GetFunctor().SetFactor( m_Scale );
  this->GetFunctor().SetOffset( m_Shift );
}


} // end namespace itk

#endif
