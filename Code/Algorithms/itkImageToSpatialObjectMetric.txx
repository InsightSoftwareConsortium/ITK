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


} // end namespace itk


#endif
