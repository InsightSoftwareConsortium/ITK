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

template < class TImage >
void
ImageToListAdaptor< TImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: " << m_Image << std::endl;
  //  os << indent << "Accesor: " << m_Accessor << std::endl;
}

template < class TImage >
void
ImageToListAdaptor< TImage >
::SetImage(ImagePointer image) 
{ 
  m_Image = image ; 
  m_PixelContainer = image->GetPixelContainer() ;
}

template < class TImage >
typename ImageToListAdaptor< TImage >::ImagePointer
ImageToListAdaptor< TImage >
::GetImage() 
{
  return m_Image ; 
}  

/** returns the number of measurement vectors in this container*/
template < class TImage >
inline unsigned int
ImageToListAdaptor< TImage >
::Size() const
{
  return m_PixelContainer->Size() ;
}

template < class TImage >
inline unsigned int
ImageToListAdaptor< TImage >
::Size(const unsigned int &) const
{
  return m_PixelContainer->Size() ;
}

template < class TImage >
inline unsigned int
ImageToListAdaptor< TImage >
::GetNumberOfInstances() const
{
  return this->Size() ;
}


template < class TImage >
inline void
ImageToListAdaptor< TImage >
::SetMeasurementVector(const InstanceIdentifier id,
                       const MeasurementVectorType &measurementVector) 
{ 
  (*m_PixelContainer)[id] = measurementVector ;
}


template < class TImage >
inline typename ImageToListAdaptor< TImage >::MeasurementVectorType&
ImageToListAdaptor< TImage >
::GetMeasurementVector(const InstanceIdentifier &id)
{
  return (*m_PixelContainer)[id] ;
}

template < class TImage >
inline typename ImageToListAdaptor< TImage >::FrequencyType
ImageToListAdaptor< TImage >
::GetFrequency(const InstanceIdentifier &) const 
{
  return NumericTraits< FrequencyType >::One ;
}

template < class TImage >
typename ImageToListAdaptor< TImage >::FrequencyType
ImageToListAdaptor< TImage >
::GetTotalFrequency(const unsigned int &) const
{ 
  return this->Size() ; 
}

} // end of namespace Statistics 
} // end of namespace itk

#endif



