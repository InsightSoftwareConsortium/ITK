/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkCenteredAffineRegistrationTransform_txx
#define _itkCenteredAffineRegistrationTransform_txx

#include <itkExceptionObject.h>
#include "itkCenteredAffineRegistrationTransform.h"


namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::CenteredAffineRegistrationTransform()
{
  m_DomainTransformationCenter.Fill( 0 );
  m_RangeTransformationCenter.Fill( 0 );
}


/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::CenteredAffineRegistrationTransform( const Self & other )
{
  m_AffineTransform = other.m_AffineTransform;
  m_DomainTransformationCenter = other.m_DomainTransformationCenter;
  m_RangeTransformationCenter = other.m_RangeTransformationCenter;
}


/**
 * Assignment Operator
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
const
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters> &
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::operator=( const Self & other )
{
  m_AffineTransform = other.m_AffineTransform;
  m_DomainTransformationCenter = other.m_DomainTransformationCenter;
  m_RangeTransformationCenter = other.m_RangeTransformationCenter;
  return *this;
}


/**
 * Transform a Point
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::PointType
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::Transform( const PointType & point ) const
{
  return m_AffineTransform.Transform( point );
}



/**
 * Set the transformation parameters
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
void
CenteredAffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::SetParameters(const ParametersType & parameters )
{

  m_Parameters = parameters;

  typename AffineTransformType::MatrixType  linear;
  typename AffineTransformType::VectorType  constant;

  // Transfer the linear part
  unsigned int par = 0;

  for(unsigned int row=0; row<NDimensions; row++)
  {
    for(unsigned int col=0; col<NDimensions; col++)
    {
      linear[row][col] = m_Parameters[par];
      ++par;
    }
  }

  PointType temp;
  temp = linear * m_DomainTransformationCenter;

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++)
  {
    constant[i] = m_Parameters[par] +
      m_RangeTransformationCenter[i] - temp[i];
    ++par;
  }

  m_AffineTransform.SetMatrix( linear );
  m_AffineTransform.SetOffset( constant );

}

/**
 *  Compute the Jacobian of the transformation
 */
template<class TScalarType, unsigned int NDimensions, class TParameters>
const
CenteredAffineRegistrationTransform<TScalarType, NDimensions,TParameters>
::JacobianType &
CenteredAffineRegistrationTransform<TScalarType, NDimensions,TParameters>
::GetJacobian( const PointType & p ) const
{

  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.

  m_Jacobian.Fill( 0.0 );

  unsigned int blockOffset = 0;

  for(unsigned int block=0; block < SpaceDimension; block++)
  {
    for(unsigned int dim=0; dim < SpaceDimension; dim++ )
    {
       m_Jacobian[ block ][ blockOffset + dim ] =
        p[dim] - m_DomainTransformationCenter[dim];
    }

    blockOffset += SpaceDimension;

  }

  for(unsigned int dim=0; dim < SpaceDimension; dim++ )
  {
     m_Jacobian[ dim ][ blockOffset + dim ] = 1;
  }

  return m_Jacobian;

}



} // end namespace itk

#endif









