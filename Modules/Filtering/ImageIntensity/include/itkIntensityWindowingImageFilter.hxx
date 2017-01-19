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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkIntensityWindowingImageFilter_hxx
#define itkIntensityWindowingImageFilter_hxx

#include "itkIntensityWindowingImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
IntensityWindowingImageFilter< TInputImage, TOutputImage >
::IntensityWindowingImageFilter() :
  m_Scale( 1.0 ),
  m_Shift( 0.0 ),
  m_WindowMinimum( NumericTraits< InputPixelType >::NonpositiveMin() ),
  m_WindowMaximum( NumericTraits< InputPixelType >::max() ),
  m_OutputMinimum( NumericTraits< OutputPixelType >::NonpositiveMin() ),
  m_OutputMaximum( NumericTraits< OutputPixelType >::max() )
{
}

template< typename TInputImage, typename TOutputImage >
void
IntensityWindowingImageFilter< TInputImage, TOutputImage >
::SetWindowLevel(const InputPixelType & window, const InputPixelType & level)
{
  typedef typename NumericTraits< InputPixelType >::RealType InputRealType;
  InputRealType tmp1, tmp2;

  tmp1 = static_cast< InputRealType >( level )
         - ( static_cast< InputRealType >( window ) / 2.0 );
  if ( tmp1 < NumericTraits< InputPixelType >::NonpositiveMin() )
    {
    tmp1 = 0.0;
    }

  tmp2 = static_cast< InputRealType >( level )
         + ( static_cast< InputRealType >( window ) / 2.0 );
  if ( tmp2 > NumericTraits< InputPixelType >::max() )
    {
    tmp2 = NumericTraits< InputPixelType >::max();
    }

  this->m_WindowMinimum = static_cast< InputPixelType >( tmp1 );
  this->m_WindowMaximum = static_cast< InputPixelType >( tmp2 );
}

template< typename TInputImage, typename TOutputImage >
typename IntensityWindowingImageFilter< TInputImage, TOutputImage >::InputPixelType
IntensityWindowingImageFilter< TInputImage, TOutputImage >
::GetWindow() const
{
  return this->m_WindowMaximum - this->m_WindowMinimum;
}

template< typename TInputImage, typename TOutputImage >
typename IntensityWindowingImageFilter< TInputImage, TOutputImage >::InputPixelType
IntensityWindowingImageFilter< TInputImage, TOutputImage >
::GetLevel() const
{
  return ( this->m_WindowMaximum + this->m_WindowMinimum ) / 2;
}

template< typename TInputImage, typename TOutputImage >
void
IntensityWindowingImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  this->m_Scale =
    ( static_cast< RealType >( this->m_OutputMaximum )
      - static_cast< RealType >( this->m_OutputMinimum ) )
    / ( static_cast< RealType >( this->m_WindowMaximum )
        - static_cast< RealType >( this->m_WindowMinimum ) );

  this->m_Shift =
    static_cast< RealType >( this->m_OutputMinimum )
    - static_cast< RealType >( this->m_WindowMinimum ) * this->m_Scale;

  // set up the functor values
  this->GetFunctor().SetOutputMinimum( this->m_OutputMinimum );
  this->GetFunctor().SetOutputMaximum( this->m_OutputMaximum );

  this->GetFunctor().SetWindowMinimum( this->m_WindowMinimum );
  this->GetFunctor().SetWindowMaximum( this->m_WindowMaximum );

  this->GetFunctor().SetFactor( this->m_Scale );
  this->GetFunctor().SetOffset( this->m_Shift );
}


template< typename TInputImage, typename TOutputImage >
void
IntensityWindowingImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output Minimum: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( this->m_OutputMinimum )
     << std::endl;
  os << indent << "Output Maximum: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( this->m_OutputMaximum )
     << std::endl;
  os << indent << "Window Minimum: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( this->m_WindowMinimum )
     << std::endl;
  os << indent << "Window Maximum: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( this->m_WindowMaximum )
     << std::endl;
  os << indent << "Scale Factor: "
     << static_cast< typename NumericTraits< RealType >::PrintType >( this->m_Scale )
     << std::endl;
  os << indent << "Shift offset: "
     << static_cast< typename NumericTraits< RealType >::PrintType >( this->m_Shift )
     << std::endl;
}

} // end namespace itk

#endif
