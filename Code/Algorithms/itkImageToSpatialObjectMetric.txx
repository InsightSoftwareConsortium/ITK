/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToSpatialObjectMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _ImageToSpatialObjectMetric_txx
#define _ImageToSpatialObjectMetric_txx

#include "itkImageToSpatialObjectMetric.h"

namespace itk
{

/** Constructor */
template < class TFixedImage, class TMovingSpatialObject> 
ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>
::ImageToSpatialObjectMetric()
{

  m_FixedImage          = 0; // has to be provided by the user.
  m_MovingSpatialObject = 0; // has to be provided by the user.
  m_Transform           = 0; // has to be provided by the user.
  m_Interpolator        = 0; // has to be provided by the user.

}

/** PrintSelf */
template < class TFixedImage, class TMovingSpatialObject> 
void
ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Moving Spatial Object: " << m_MovingSpatialObject.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedImage.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Last Transform parameters = " << m_LastTransformParameters << std::endl;
}


} // end namespace itk


#endif
