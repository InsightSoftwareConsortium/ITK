/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCenteredAffineTransform_txx
#define __itkCenteredAffineTransform_txx

#include "itkNumericTraits.h"
#include "itkCenteredAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions>
CenteredAffineTransform<TScalarType, NDimensions>::
CenteredAffineTransform():Superclass(SpaceDimension,ParametersDimension)
{
}

// Destructor
template<class TScalarType, unsigned int NDimensions>
CenteredAffineTransform<TScalarType, NDimensions>::
~CenteredAffineTransform()
{
  return;
}

// Get parameters
template<class TScalarType, unsigned int NDimensions>
const typename CenteredAffineTransform<TScalarType, 
                                       NDimensions>::ParametersType &
CenteredAffineTransform<TScalarType, NDimensions>::
GetParameters( void ) const
{

  // Transfer the linear part
  unsigned int par = 0;

  const MatrixType & matrix = this->GetMatrix();

  for(unsigned int row=0; row<NDimensions; row++) 
    {
    for(unsigned int col=0; col<NDimensions; col++) 
      {
      this->m_Parameters[par] = matrix[row][col];
      ++par;
      }
    }

 
  // Transfer the rotation center 
  InputPointType center = this->GetCenter();
  for(unsigned int j=0; j<NDimensions; j++) 
    {
    this->m_Parameters[par] = center[j];
    ++par;
    }
  
  // Transfer the translation
  OutputVectorType translation = this->GetTranslation();
  for(unsigned int k=0; k<NDimensions; k++) 
    {
    this->m_Parameters[par] = translation[k];
    ++par;
    }


  return this->m_Parameters;

}

/** Set the parameters */
template<class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>::
SetParameters( const ParametersType & parameters )
{

  // Transfer the linear part
  unsigned int par = 0;

  this->m_Parameters = parameters;

  MatrixType matrix;

  for(unsigned int row=0; row<NDimensions; row++) 
    {
    for(unsigned int col=0; col<NDimensions; col++) 
      {
      matrix[row][col] = this->m_Parameters[par];
      ++par;
      }
    }

  this->SetMatrix( matrix );

  // Transfer the rotation center 
  InputPointType center;
  for(unsigned int i=0; i<NDimensions; i++) 
    {
    center[i] = this->m_Parameters[par];
    ++par;
    }
  this->SetCenter(center);
  
  // Transfer the translation
  OutputVectorType translation;
  for(unsigned int k=0; k<NDimensions; k++) 
    {
    translation[k] = this->m_Parameters[par];
    ++par;
    }
  this->SetTranslation(translation);

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

}


// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions>
const typename CenteredAffineTransform<TScalarType, NDimensions>
::JacobianType & 
CenteredAffineTransform<TScalarType, NDimensions>::
GetJacobian( const InputPointType & p ) const
{
  
  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.
  // The block corresponding to the center parameters is
  // composed by ( Identity matrix - Rotation Matrix).

  this->m_Jacobian.Fill( 0.0 );


  unsigned int blockOffset = 0;
  
  for(unsigned int block=0; block < SpaceDimension; block++) 
    {
    for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
      {
      this->m_Jacobian( block , blockOffset + dim ) = p[dim];
      }
    blockOffset += SpaceDimension;
    }

  // Block associated with the center parameters
  const MatrixType & matrix = this->GetMatrix();
  for(unsigned int k=0; k < SpaceDimension; k++) 
    {
    this->m_Jacobian( k, blockOffset + k ) = 1.0;
    for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
      {
      this->m_Jacobian( k, blockOffset + dim ) -= matrix[k][dim];
      }
    }
  blockOffset += SpaceDimension;

  // Block associated with the translations
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    this->m_Jacobian( dim , blockOffset + dim ) = 1.0;
    }

  return this->m_Jacobian;

}

} // namespace

#endif
