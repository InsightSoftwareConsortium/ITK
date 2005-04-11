/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrientedImage.txx
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
#ifndef _itkOrientedImage_txx
#define _itkOrientedImage_txx
#include "itkOrientedImage.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
OrientedImage<TPixel, VImageDimension>
::OrientedImage()
{
  m_IndexToPhysicalPoint.SetIdentity();
  m_PhysicalPointToIndex.SetIdentity();
}
} // end namespace itk

#endif
