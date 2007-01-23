/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhasedArray3DSpecialCoordinatesImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPhasedArray3DSpecialCoordinatesImage_txx
#define __itkPhasedArray3DSpecialCoordinatesImage_txx
#include "itkPhasedArray3DSpecialCoordinatesImage.h"

namespace itk
{

/**
 *
 */
template<class TPixel>
void 
PhasedArray3DSpecialCoordinatesImage<TPixel>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent
     << "m_RadiusSampleSize = " << m_RadiusSampleSize
     << std::endl;
  os << indent
     << "m_AzimuthAngularSeparation = "
     << m_AzimuthAngularSeparation
     << std::endl;
  os << indent
     << "m_ElevationAngularSeparation = "
     << m_ElevationAngularSeparation
     << std::endl;
  os << indent 
     << "m_FirstSampleDistance = "
     << m_FirstSampleDistance
     << std::endl;
}


} // end namespace itk

#endif
