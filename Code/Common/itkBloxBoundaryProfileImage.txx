/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryProfileImage_txx
#define __itkBloxBoundaryProfileImage_txx

#include "itkBloxBoundaryProfileImage.h"

#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_math.h>
#include <itkArray.h>
#include <itkArray2D.h>

typedef vnl_matrix<double> MatrixType;
typedef vnl_vector<double> VectorType;

namespace itk
{

template <unsigned int VImageDimension>
BloxBoundaryProfileImage<VImageDimension>
::BloxBoundaryProfileImage()
{

}

template <unsigned int VImageDimension>
BloxBoundaryProfileImage<VImageDimension>
::~BloxBoundaryProfileImage()
{

}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileImage<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
