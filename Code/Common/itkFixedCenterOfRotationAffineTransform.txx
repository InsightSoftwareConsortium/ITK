/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedCenterOfRotationAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFixedCenterOfRotationAffineTransform_txx
#define _itkFixedCenterOfRotationAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
FixedCenterOfRotationAffineTransform()
:Superclass(SpaceDimension,ParametersDimension)
  {
  m_CenterOfRotation.Fill( 0 );
  }




// Destructor
template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
~FixedCenterOfRotationAffineTransform()
  {
  return;
  }



// Print self
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
  {
  Superclass::PrintSelf(os,indent);

  unsigned int i;
  
  os << indent << "CenterOfRotation: ";
  for (i = 0; i < NDimensions; i++) 
    {
    os << m_CenterOfRotation[i] << " ";
    }
  os << std::endl;
  }


// Create and return an inverse transformation
template<class TScalarType, unsigned int NDimensions>
typename FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::Pointer
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
Inverse( void ) const
  {
  Pointer result = New();
  
  result->SetMatrix( this->GetMatrix() );
  result->SetOffset( this->GetOffset() );
  result->Superclass::Inverse();
  
  result->m_CenterOfRotation      =   m_CenterOfRotation;
  
  return result;
  }



// Set the parameters in order to fit an Identity transform
template<class TScalarType, unsigned int NDimensions>
void
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
SetIdentity( void ) 
  { 
  this->Superclass::SetIdentity();
  m_CenterOfRotation.Fill( 0.0 );
  this->Modified();  
  }

// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions>
const typename FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::JacobianType & 
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
GetJacobian( const InputPointType & p ) const
  {
  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.
  // The block corresponding to the center parameters is
  // composed by ( Identity matrix - Rotation Matrix).

  m_Jacobian.Fill( 0.0 );


  unsigned int blockOffset = 0;
  
  for(unsigned int block=0; block < SpaceDimension; block++) 
    {
    for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
      {
       m_Jacobian( block , blockOffset + dim ) = p[dim];
      }
    blockOffset += SpaceDimension;
    }

  // Block associated with the center parameters
  /*
  const MatrixType & matrix = this->GetMatrix();
  for(unsigned int k=0; k < SpaceDimension; k++) 
    {
    m_Jacobian( k, blockOffset + k ) = 1.0;
    for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
      {
      m_Jacobian( k, blockOffset + dim ) -= matrix[k][dim];
      }
    }
  blockOffset += SpaceDimension;
  */

  // Block associated with the translations
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    m_Jacobian( dim , blockOffset + dim ) = 1.0;
    }

  return m_Jacobian;
  }


template<class TScalarType, unsigned int NDimensions>
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
OutputPointType
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
TransformPoint(const InputPointType & point) const
  {
  const OffsetType & offset = this->GetOffset();
  const MatrixType & matrix = this->GetMatrix();
  int i;
  OutputVectorType vect1;
  for(i=0; i<NDimensions; i++)
    {
    vect1[i] = point[i] - m_CenterOfRotation[i];
    }
  OutputVectorType vect2 = matrix * vect1;
  OutputPointType point2;
  for(i=0; i<NDimensions; i++)
    {
    point2[i] = vect2[i] + m_CenterOfRotation[i] + offset[i];
    }
  return point2;
  }

template<class TScalarType, unsigned int NDimensions>
typename AffineTransform<TScalarType, NDimensions> &
FixedCenterOfRotationAffineTransform<TScalarType, NDimensions>::
GetAffineTransform(void) const
  {
  }

} // namespace

#endif
