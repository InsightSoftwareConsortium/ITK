/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToListAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
void
ImageToListAdaptor< TImage, TMeasurementVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: " << m_Image << std::endl;
  os << indent << "PixelContainer: " << m_PixelContainer << std::endl;
  os << indent << "Use buffer: " << m_UseBuffer << std::endl;
}

template < class TImage, class TMeasurementVector >
void
ImageToListAdaptor< TImage, TMeasurementVector >
::SetImage(ImagePointer image) 
{ 
  m_Image = image ; 
  m_PixelContainer = image->GetPixelContainer() ;
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
typename ImageToListAdaptor< 
  TImage, 
  TMeasurementVector >::ImagePointer
ImageToListAdaptor< TImage, TMeasurementVector >
::GetImage() 
{
  return m_Image ; 
}  

/** returns the number of measurement vectors in this container*/
template < class TImage, class TMeasurementVector >
inline unsigned int
ImageToListAdaptor< TImage, TMeasurementVector >
::Size() const
{
  return m_PixelContainer->Size() ;
}

template < class TImage, class TMeasurementVector >
TMeasurementVector
ImageToListAdaptor< TImage, TMeasurementVector >
::GetMeasurementVector(const InstanceIdentifier &id)
{
  if ( m_UseBuffer )
    {
    return *( reinterpret_cast< MeasurementVectorType* >(&(*m_PixelContainer)[id]) ) ;
    }
  else
    {
    return *(reinterpret_cast< MeasurementVectorType* >
             ( &(m_Image->GetPixel( m_Image->ComputeIndex( id ) ) ) ) ) ;
    }
}

template < class TImage, class TMeasurementVector >
inline typename ImageToListAdaptor< TImage, TMeasurementVector >::FrequencyType
ImageToListAdaptor< TImage, TMeasurementVector >
::GetFrequency(const InstanceIdentifier &) const 
{
  return NumericTraits< FrequencyType >::One ;
}

template < class TImage, class TMeasurementVector >
typename ImageToListAdaptor< TImage, TMeasurementVector >::FrequencyType
ImageToListAdaptor< TImage, TMeasurementVector >
::GetTotalFrequency() const
{ 
  return this->Size() ; 
}

} // end of namespace Statistics 
} // end of namespace itk

#endif



