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

template < class TImage, class TDataAccessor >
void
ImageToListAdaptor< TImage, TDataAccessor >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: " << m_Image << std::endl;
  //  os << indent << "Accesor: " << m_Accessor << std::endl;
}

template < class TImage, class TDataAccessor >
void
ImageToListAdaptor< TImage, TDataAccessor >
::SetImage(ImagePointer image) 
{ 
  m_Image = image ; 
  m_PixelContainer = image->GetPixelContainer() ;
  m_DataAccessor.SetPixelContainer(m_PixelContainer) ;
}

template < class TImage, class TDataAccessor >
ImageToListAdaptor< TImage, TDataAccessor >::ImagePointer
ImageToListAdaptor< TImage, TDataAccessor >
::GetImage() 
{
  return m_Image ; 
}  

  /** returns the number of measurement vectors in this container*/
template < class TImage, class TDataAccessor >
inline int
ImageToListAdaptor< TImage, TDataAccessor >
::Size() const
{
  return m_PixelContainer->Size() ;
}

template < class TImage, class TDataAccessor >
inline int
ImageToListAdaptor< TImage, TDataAccessor >
::Size(const unsigned int &dimension) const
{
  return m_PixelContainer->Size() ;
}

template < class TImage, class TDataAccessor >
inline int
ImageToListAdaptor< TImage, TDataAccessor >
::GetNumberOfInstances() const
{
  return this->Size() ;
}


template < class TImage, class TDataAccessor >
inline void
ImageToListAdaptor< TImage, TDataAccessor >
::SetMeasurementVector(const InstanceIdentifier id,
                       const MeasurementVectorType &measurementVector) 
{ 
  m_DataAccessor.SetMeasurementVector(id, measurementVector) ;
}


template < class TImage, class TDataAccessor >
inline ImageToListAdaptor< TImage, TDataAccessor >::MeasurementVectorType&
ImageToListAdaptor< TImage, TDataAccessor >
::GetMeasurementVector(const InstanceIdentifier &id)
{
  return m_DataAccessor.GetMeasurementVector(id) ;
}

template < class TImage, class TDataAccessor >
inline void
ImageToListAdaptor< TImage, TDataAccessor >
::SetMeasurement(const InstanceIdentifier &id, 
                 const unsigned int &dim,
                 const MeasurementType &value) 
{
  m_TempMeasurementVector = this->GetMeasurementVector(id) ;
  m_TempMeasurementVector[dim] = value ;
  SetMeasurementVector(id, m_TempMeasurementVector) ;
}

template < class TImage, class TDataAccessor >
inline ImageToListAdaptor< TImage, TDataAccessor >::MeasurementType& 
ImageToListAdaptor< TImage, TDataAccessor >
::GetMeasurement(const InstanceIdentifier &id, 
                 const unsigned int &dim)
{
  return m_DataAccessor.GetMeasurement(id, dim) ; 
}

template < class TImage, class TDataAccessor >
inline ImageToListAdaptor< TImage, TDataAccessor >::FrequencyType
ImageToListAdaptor< TImage, TDataAccessor >
::GetFrequency(const InstanceIdentifier &id) const 
{
  return 1 ;
}

template < class TImage, class TDataAccessor >
ImageToListAdaptor< TImage, TDataAccessor >::FrequencyType
ImageToListAdaptor< TImage, TDataAccessor >
::GetTotalFrequency(const unsigned int &dim) const
{ 
  return this->Size() ; 
}

  

template < class TImage >
void
ScalarImageAccessor< TImage >
::SetPixelContainer(PixelContainerPointer pixelContainer)
{
  m_PixelContainer = pixelContainer ;
}

template < class TImage >
inline void
ScalarImageAccessor< TImage >
::SetMeasurementVector(const InstanceIdentifier &id, 
                       const MeasurementVectorType &measurementVector)
{ 
  (*m_PixelContainer)[id] = measurementVector[0] ; 
}
  
template < class TImage >
inline ScalarImageAccessor< TImage >::MeasurementVectorType& 
ScalarImageAccessor< TImage >
::GetMeasurementVector(const InstanceIdentifier &id)
{ 
  m_TempMeasurementVector[0] = (*m_PixelContainer)[id] ;
  return m_TempMeasurementVector ;
}

template < class TImage >
inline ScalarImageAccessor< TImage >::MeasurementType& 
ScalarImageAccessor< TImage >
::GetMeasurement(const InstanceIdentifier &id, 
                 const unsigned int &dimension)
{
  return (*m_PixelContainer)[id] ;
}

template < class TImage >
void
VectorImageAccessor< TImage >
::SetPixelContainer(PixelContainerPointer pixelContainer)
{
  m_PixelContainer = pixelContainer ;
}

template < class TImage >
inline void
VectorImageAccessor< TImage >
::SetMeasurementVector(const InstanceIdentifier &id,
                       const MeasurementVectorType &measurementVector) 
{ 
  (*m_PixelContainer)[id] = measurementVector ; 
}
  
template < class TImage >
inline VectorImageAccessor< TImage >::MeasurementVectorType&
VectorImageAccessor< TImage >
::GetMeasurementVector(const InstanceIdentifier &id)
{ 
  return (*m_PixelContainer)[id] ; 
}
  
template < class TImage >
inline VectorImageAccessor< TImage >::MeasurementType&
VectorImageAccessor< TImage >
::GetMeasurement(const InstanceIdentifier &id, 
                 const unsigned int &dimension)
{ 
  return (*m_PixelContainer)[id][dimension] ;
}


  } // end of namespace Statistics 
} // end of namespace itk

#endif



