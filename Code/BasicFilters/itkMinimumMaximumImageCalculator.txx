/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMinimumMaximumImageCalculator_txx
#define _itkMinimumMaximumImageCalculator_txx

#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk
{ 
    
/*
 * Constructor
 */
template<class TInputImage>
MinimumMaximumImageCalculator<TInputImage>
::MinimumMaximumImageCalculator()
{
  m_Image = TInputImage::New();
  m_Maximum = NumericTraits<PixelType>::NonpositiveMin() ;
  m_Minimum = NumericTraits<PixelType>::max() ;
  m_IndexOfMinimum.Fill(0);
  m_IndexOfMaximum.Fill(0);
  m_RegionSetByUser = false;
}


/*
 * Compute Min and Max of m_Image
 */
template<class TInputImage>
void
MinimumMaximumImageCalculator<TInputImage>
::Compute(void)
{
  if( m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }

  ImageRegionConstIteratorWithIndex< TInputImage >  it( m_Image, m_Region );
  m_Maximum = NumericTraits<PixelType>::NonpositiveMin() ;
  m_Minimum = NumericTraits<PixelType>::max() ;


  while( !it.IsAtEnd() )
    {
    const PixelType value = it.Get();  
    if (value > m_Maximum) 
      {
      m_Maximum = value;
      m_IndexOfMaximum = it.GetIndex();
      }
    if (value < m_Minimum) 
      {
      m_Minimum = value;
      m_IndexOfMinimum = it.GetIndex();
      }
    ++it;
    }

}

/*
 * Compute the minimum intensity value of the image
 */
template<class TInputImage>
void
MinimumMaximumImageCalculator<TInputImage>
::ComputeMinimum(void)
{
  if( m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }
  ImageRegionConstIteratorWithIndex< TInputImage >  it( m_Image,  m_Region );
  m_Minimum = NumericTraits<PixelType>::max() ;

  while( !it.IsAtEnd() )
    {
    const PixelType value = it.Get();  
    if (value < m_Minimum) 
      {
      m_Minimum = value;
      m_IndexOfMinimum = it.GetIndex();
      }
    ++it;
    }

}

/*
 * Compute the maximum intensity value of the image
 */
template<class TInputImage>
void
MinimumMaximumImageCalculator<TInputImage>
::ComputeMaximum(void)
{
  if( m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }
  ImageRegionConstIteratorWithIndex< TInputImage >  it( m_Image,  m_Region );
  m_Maximum = NumericTraits<PixelType>::NonpositiveMin() ;

  while( !it.IsAtEnd() )
    {
    const PixelType value = it.Get();  
    if (value > m_Maximum) 
      {
      m_Maximum = value;
      m_IndexOfMaximum = it.GetIndex();
      }
    ++it;
    }

}



template<class TInputImage>
void
MinimumMaximumImageCalculator<TInputImage>
::SetRegion( const RegionType & region )
{
  m_Region = region;
  m_RegionSetByUser = true;
}


 
template<class TInputImage>
void
MinimumMaximumImageCalculator<TInputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Minimum: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Minimum)
     << std::endl;
  os << indent << "Maximum: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Maximum)
     << std::endl;
  os << indent << "Index of Minimum: " << m_IndexOfMinimum << std::endl;
  os << indent << "Index of Maximum: " << m_IndexOfMaximum << std::endl;
  os << indent << "Image: " << m_Image << std::endl;
  os << indent << "Region: " << m_Region << std::endl;
  os << indent << "Region set by User: " << m_RegionSetByUser << std::endl;
}

} // end namespace itk

#endif
