/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumConditionalImageCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMinimumMaximumConditionalImageCalculator_txx
#define _itkMinimumMaximumConditionalImageCalculator_txx

#include "itkMinimumMaximumConditionalImageCalculator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk
{ 
    
/*
 * Constructor
 */
template<class TInputImage,class TMaskImage>
MinimumMaximumConditionalImageCalculator<TInputImage,TMaskImage>
::MinimumMaximumConditionalImageCalculator()
{
  m_Image = TInputImage::New();
  m_MaskImage = TMaskImage::New();
  m_Maximum = NumericTraits<PixelType>::NonpositiveMin() ;
  m_Minimum = NumericTraits<PixelType>::max() ;
  m_IndexOfMinimum.Fill(0);
  m_IndexOfMaximum.Fill(0);
  m_RegionSetByUser = false;
}


/*
 * Compute Min and Max of m_Image
 */
template<class TInputImage,class TMaskImage>
void
MinimumMaximumConditionalImageCalculator<TInputImage,TMaskImage>
::Compute(void)
{
  if( !m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }

  ImageRegionConstIteratorWithIndex< TInputImage >  it( m_Image, m_Region );
  ImageRegionConstIteratorWithIndex< TMaskImage  >  mt( m_MaskImage, m_Region );

  m_Maximum = NumericTraits<PixelType>::NonpositiveMin() ;
  m_Minimum = NumericTraits<PixelType>::max() ;


  it.GoToBegin();
  mt.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if( mt.Get() == m_MaskValue )
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
      }
    ++it;
    ++mt;
    }

}

/*
 * Compute the minimum intensity value of the image
 */
template<class TInputImage,class TMaskImage>
void
MinimumMaximumConditionalImageCalculator<TInputImage,TMaskImage>
::ComputeMinimum(void)
{
  if( !m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }
  ImageRegionConstIteratorWithIndex< TInputImage >  it( m_Image,  m_Region );
  ImageRegionConstIteratorWithIndex< TMaskImage  >  mt( m_MaskImage, m_Region );

  m_Minimum = NumericTraits<PixelType>::max() ;

  it.GoToBegin();
  mt.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if( mt.Get() == m_MaskValue )
      {
      const PixelType value = it.Get();  
      if (value < m_Minimum) 
        {
        m_Minimum = value;
        m_IndexOfMinimum = it.GetIndex();
        }
      }
    ++it;
    ++mt;
    }

}

/*
 * Compute the maximum intensity value of the image
 */
template<class TInputImage,class TMaskImage>
void
MinimumMaximumConditionalImageCalculator<TInputImage,TMaskImage>
::ComputeMaximum(void)
{
  if( !m_RegionSetByUser )
    {
    m_Region = m_Image->GetRequestedRegion();
    }
  ImageRegionConstIteratorWithIndex< TInputImage >  it( m_Image,  m_Region );
  ImageRegionConstIteratorWithIndex< TMaskImage  >  mt( m_MaskImage, m_Region );

  m_Maximum = NumericTraits<PixelType>::NonpositiveMin() ;

  it.GoToBegin();
  mt.GoToBegin();
  while( !it.IsAtEnd() )
    {
    if( mt.Get() == m_MaskValue )
      {
      const PixelType value = it.Get();  
      if (value > m_Maximum) 
        {
        m_Maximum = value;
        m_IndexOfMaximum = it.GetIndex();
        }
      }
    ++it;
    ++mt;
    }

}



template<class TInputImage,class TMaskImage>
void
MinimumMaximumConditionalImageCalculator<TInputImage,TMaskImage>
::SetRegion( const RegionType & region )
{
  m_Region = region;
  m_RegionSetByUser = true;
}


 
template<class TInputImage,class TMaskImage>
void
MinimumMaximumConditionalImageCalculator<TInputImage,TMaskImage>
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
  os << indent << "Image: " << std::endl;
    m_Image->Print(os, indent.GetNextIndent());
  os << indent << "Region: " << std::endl;
    m_Region.Print(os,indent.GetNextIndent());
  os << indent << "Region set by User: " << m_RegionSetByUser << std::endl;

  os << indent << "Mask Value: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_MaskValue)
     << std::endl;
  os << indent << "Mask Image: " << std::endl;
    m_MaskImage->Print(os, indent.GetNextIndent());
}

} // end namespace itk

#endif
