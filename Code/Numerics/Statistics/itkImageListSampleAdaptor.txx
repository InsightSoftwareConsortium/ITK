/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageListSampleAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageListSampleAdaptor_txx
#define _itkImageListSampleAdaptor_txx

#include "itkImageListSampleAdaptor.h"

namespace itk{ 
  namespace Statistics{

template < class TImage, class TMeasurement, 
           unsigned int VMeasurementVectorSize , 
           class TAccessor >
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize, 
                               TAccessor >::IndexType
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >
::GetIndex(const InstanceIdentifier id) 
{
  IndexType index = m_Image->ComputeIndex(id) ;
  return index ;
}

template < class TImage, class TMeasurement, 
           unsigned int VMeasurementVectorSize , 
           class TAccessor >
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >::InstanceIdentifier
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >
::GetInstanceIdentifier(const IndexType index) 
{
  return m_Image->ComputeOffset(index) ;
}


template < class TImage, class TMeasurement, 
           unsigned int VMeasurementVectorSize , 
           class TAccessor >
void
ImageListSampleAdaptor< TImage, TMeasurement, VMeasurementVectorSize,
                               TAccessor >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image: " << m_Image << std::endl;
  //  os << indent << "Accesor: " << m_Accessor << std::endl;
}

  } // end of namespace Statistics 
} // end of namespace itk

#endif
