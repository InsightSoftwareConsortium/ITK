/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCenteredAffineTransform_txx
#define _itkCenteredAffineTransform_txx

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
  m_Center.Fill( 0 );
  m_Translation.Fill( 0 );
}




// Destructor
template<class TScalarType, unsigned int NDimensions>
CenteredAffineTransform<TScalarType, NDimensions>::
~CenteredAffineTransform()
{
  return;
}



// Print self
template<class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i, j;
  
  os << indent << "Center: " << std::endl;
  for (i = 0; i < NDimensions; i++) 
    {
    os << m_Center[i] << std::endl;
    }

  os << indent << "Translation: " << std::endl;
  for (j = 0; j < NDimensions; j++) 
    {
    os << m_Translation[j] << std::endl;
    }
}




// Create and return an inverse transformation
template<class TScalarType, unsigned int NDimensions>
typename CenteredAffineTransform<TScalarType, NDimensions>::Pointer
CenteredAffineTransform<TScalarType, NDimensions>::
Inverse( void ) const
{
  Pointer result = New();
  
  result->SetMatrix( this->GetMatrix() );
  result->SetOffset( this->GetOffset() );
  result->Superclass::Inverse();
  
  result->m_Center      =   m_Center;
  result->m_Translation =  -m_Translation;
  
  result->ComputeOffset();
  
  return result;
}



// Set the parameters in order to fit an Identity transform
template<class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>::
SetIdentity( void ) 
{ 
  this->Superclass::SetIdentity();
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );
  this->ComputeOffset();
  this->Modified();  
}






// Get parameters
template<class TScalarType, unsigned int NDimensions>
const typename CenteredAffineTransform<TScalarType, NDimensions>::ParametersType &
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
      m_Parameters[par] = matrix[row][col];
      ++par;
    }
  }

 
  // Transfer the rotation center 
  for(unsigned int j=0; j<NDimensions; j++) 
  {
    m_Parameters[par] = m_Center[j];
    ++par;
  }
  
  // Transfer the translation
  for(unsigned int k=0; k<NDimensions; k++) 
  {
    m_Parameters[par] = m_Translation[k];
    ++par;
  }


  return m_Parameters;

}




// Set parameters
template<class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>::
SetParameters( const ParametersType & parameters )
{

  // Transfer the linear part
  unsigned int par = 0;

  m_Parameters = parameters;

  MatrixType matrix;

  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      matrix[row][col] = m_Parameters[par];
      ++par;
    }
  }

  this->SetMatrix( matrix );

  // Transfer the rotation center 
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    m_Center[i] = m_Parameters[par];
    ++par;
  }
  
  // Transfer the translation
  for(unsigned int k=0; k<NDimensions; k++) 
  {
    m_Translation[k] = m_Parameters[par];
    ++par;
  }

  this->ComputeOffset();

}


// Compute the Jacobian in one position 
template<class TScalarType, unsigned int NDimensions>
const typename CenteredAffineTransform<TScalarType, NDimensions>::JacobianType & 
CenteredAffineTransform<TScalarType, NDimensions>::
GetJacobian( const InputPointType & p ) const
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
       m_Jacobian( block , blockOffset + dim ) = p[dim];
    }

    blockOffset += SpaceDimension;

  }

  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
  {
     m_Jacobian( dim , blockOffset + dim ) = 1.0;
  }

  return m_Jacobian;

}


 

// Get parameters
template<class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>::
ComputeOffset( void ) 
{

  const MatrixType & matrix = this->GetMatrix();
  
  OffsetType offset;
  for(unsigned int i=0; i<SpaceDimension; i++)
    {
    offset[i] = m_Translation[i] + m_Center[i];
    for(unsigned int j=0; j<SpaceDimension; j++)
      {
      offset[i] -= matrix[i][j] * m_Center[j];
      }
    }

  this->SetOffset( offset );

}




} // namespace

#endif
