/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToListAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScalarImageToListAdaptor_txx
#define _itkScalarImageToListAdaptor_txx

namespace itk{ 
namespace Statistics{

template < class TImage >
void
ScalarImageToListAdaptor< TImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template < class TImage >
typename ScalarImageToListAdaptor< TImage >::MeasurementVectorType
ScalarImageToListAdaptor< TImage >
::GetMeasurementVector(const InstanceIdentifier &id)
{
  if( m_UseBuffer )
    {
    m_TempVector[0] = (*m_PixelContainer)[id] ;
    }
  else
    {
    m_TempVector[0] = this->GetImage()->GetPixel( this->GetImage()->ComputeIndex( id ) )  ;
    }
  return m_TempVector  ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif



