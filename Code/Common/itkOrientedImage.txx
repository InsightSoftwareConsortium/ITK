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
#ifndef __itkOrientedImage_txx
#define __itkOrientedImage_txx
#include "itkOrientedImage.h"

namespace itk
{

/**
 * Constructor
 */
template<class TPixel, unsigned int VImageDimension>
OrientedImage<TPixel, VImageDimension>
::OrientedImage()
{
}


/** Version of index to point matrix computation that bypasses the
 * implementation in the itk::Image, and invokes the default method in the
 * itk::ImageBase class.
 */
template<class TPixel, unsigned int VImageDimension>
void
OrientedImage<TPixel, VImageDimension>
::ComputeIndexToPhysicalPointMatrices()
{
  // Use the default implementation of ImageBase, that always take 
  // direction into account. In this way we bypass the option of 
  // ignoring direction that is available in the itk::Image.
  this->ImageBase<VImageDimension>::ComputeIndexToPhysicalPointMatrices();
}

} // end namespace itk

#endif
