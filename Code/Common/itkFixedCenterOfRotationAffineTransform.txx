/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedCenterOfRotationAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFixedCenterOfRotationAffineTransform_txx
#define _itkFixedCenterOfRotationAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

/** Constructor with default arguments */
template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
FixedCenterOfRotationAffineTransform() :
  Superclass(OutputSpaceDimension, ParametersDimension)
{
}

template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
FixedCenterOfRotationAffineTransform( unsigned int outputSpaceDims,
                                      unsigned int paramsDims ) :
  Superclass(outputSpaceDims, paramsDims)
{
}

template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
FixedCenterOfRotationAffineTransform( const MatrixType & matrix,
                                      const OutputVectorType & offset ) :
  Superclass(matrix, offset)
{
}


/** Destructor */
template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
~FixedCenterOfRotationAffineTransform()
{
  return;
}

} // namespace

#endif
