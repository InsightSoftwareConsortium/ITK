/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToListAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToListAdaptor_txx
#define _itkImageToListAdaptor_txx

namespace itk{ 
namespace Statistics{

template < class TImage, class TMeasurementVector >
ImageToListAdaptor< TImage, TMeasurementVector >
::ImageToListAdaptor()
{
  m_UseBuffer = true ;
  m_Image = 0 ;
  m_PixelContainer = 0 ;
  Superclass::SetMeasurementVectorSize( MeasurementVectorSize );
}

template < class TImage, class TMeasurementVector >
void
ImageToListAdaptor< TImage, TMeasurementVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: " ;
  if ( m_Image.IsNotNull() )
    {
    os << m_Image << std::endl;
    }
  else
    {
    os << "not set." << std::endl ;
    }
   
  os << indent << "PixelContainer: " ;
  if ( m_PixelContainer.IsNotNull() )
    {
    os << m_PixelContainer << std::endl;
    }
  else
    {
    os << "not set." << std::endl ;
    }
  
  os << indent << "Use buffer: " << m_UseBuffer << std::endl;
}

template < class TImage, class TMeasurementVector >
void
ImageToListAdaptor< TImage, TMeasurementVector >
::SetImage(const TImage* image) 
{ 
  m_Image = image ; 
  m_PixelContainer = image->GetPixelContainer() ;
  m_ImageBeginIndex = image->GetLargestPossibleRegion().GetIndex() ;

  for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
    {
    m_ImageEndIndex[i] = 
      m_ImageBeginIndex[i] + image->GetLargestPossibleRegion().GetSize()[i] - 1 ;
    }

  if ( strcmp( m_Image->GetNameOfClass(), "Image" ) != 0 )
    {
    m_UseBuffer = false ;
    }
  else
    {
    m_UseBuffer = true ;
    }
}

template < class TImage, class TMeasurementVector >
const TImage*
ImageToListAdaptor< TImage, TMeasurementVector >
::GetImage() const
{
  return m_Image.GetPointer() ; 
}  

template < class TImage, class TMeasurementVector >
typename ImageToListAdaptor< TImage, TMeasurementVector >::TotalFrequencyType
ImageToListAdaptor< TImage, TMeasurementVector >
::GetTotalFrequency() const
{ 
  return this->Size() ; 
}

} // end of namespace Statistics 
} // end of namespace itk

#endif



