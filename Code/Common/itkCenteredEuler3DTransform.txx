/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredEuler3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCenteredEuler3DTransform_txx
#define _itkCenteredEuler3DTransform_txx

#include "itkCenteredEuler3DTransform.h"


namespace itk
{

// Constructor with default arguments
template<class TScalarType>
CenteredEuler3DTransform<TScalarType>::
CenteredEuler3DTransform()
{
  this->m_Center.Fill( 0 );
  this->m_Translation.Fill( 0 );
}
 
// Destructor
template<class TScalarType>
CenteredEuler3DTransform<TScalarType>::
~CenteredEuler3DTransform()
{
}


// Set the center of rotation and recompute the matrix (and the offset
// or translation relative to [0,0,0]).
template <class TScalarType>
void
CenteredEuler3DTransform<TScalarType>
::SetCenter( const InputPointType & center )
{
  this->m_Center = center;
  this->ComputeMatrix();
}


// Set the translation of the centre of rotation and recompute the
// matrix and the corresponding offset or translation relative to [0,0,0].
template <class TScalarType>
void
CenteredEuler3DTransform<TScalarType>
::SetTranslation( const OutputVectorType & translation )
{
  this->m_Translation = translation;
  this->ComputeMatrix();
}



//
// Set Parameters
// 
// Parameters are ordered as:
//
// p[0:2] = rotations about x, y and z axes
// p[3:5} = translation components
//
//
template <class TScalarType>
void
CenteredEuler3DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  itkDebugMacro( << "Setting paramaters " << parameters );

  // Set the translations
  this->m_Translation[0] = parameters[3];
  this->m_Translation[1] = parameters[4];
  this->m_Translation[2] = parameters[5];

  // Set the rotations (NB. This function will call the over-ridden
  // ComputeMatrix() function)
  this->SetRotation(parameters[0], parameters[1], parameters[2]);

  itkDebugMacro(<<"After setting parameters ");
}

//
// Get Parameters
// 
// Parameters are ordered as:
//
// p[0:2] = rotations about x, y and z axes
// p[3:5} = translation components
//

template <class TScalarType>
const typename CenteredEuler3DTransform<TScalarType>::ParametersType &
CenteredEuler3DTransform<TScalarType>
::GetParameters( void ) const
{
  ParametersType parameters = Superclass::GetParameters();

  this->m_Parameters[0] = parameters[0];
  this->m_Parameters[1] = parameters[1];
  this->m_Parameters[2] = parameters[2];

  for( unsigned int i=0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i+3] = this->m_Translation[i];
    }

  return this->m_Parameters;
}


// Print self
template<class TScalarType>
void
CenteredEuler3DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Center of Rotation: " << this->m_Center        << std::endl;
  os << indent << "Translation: "        << this->m_Translation   << std::endl;
}

// return an inverse transformation
template<class TScalarType>
bool
CenteredEuler3DTransform<TScalarType>::
GetInverse( Self* inverse) const
{
  if(!Superclass::GetInverse(inverse))
    {
    return false;
    }

  inverse->m_Center      =   this->m_Center;
  inverse->m_Translation =  -this->m_Translation;
  inverse->ComputeMatrix();

  return true;  

}


  
// Compute the Jacobian in one position 
template<class TScalarType >
void
CenteredEuler3DTransform< TScalarType >::
SetIdentity( void ) 
{
  this->Superclass::SetIdentity();
  this->m_Center.Fill( 0.0 );
  this->m_Translation.Fill( 0.0 );
  this->ComputeMatrix();
  this->Modified();  
}
 

// Compute the matrix
template <class TScalarType>
void
CenteredEuler3DTransform<TScalarType>
::ComputeMatrix( void )
{
  Superclass::ComputeMatrixAndOffset();

  ParametersType parameters = this->GetParameters();

  const double cx = cos(parameters[0]);
  const double sx = sin(parameters[0]);
  const double cy = cos(parameters[1]);
  const double sy = sin(parameters[1]); 
  const double cz = cos(parameters[2]);
  const double sz = sin(parameters[2]);

  const double ox = this->m_Center[0];
  const double oy = this->m_Center[1];
  const double oz = this->m_Center[2];

  const double tx = this->m_Translation[0];
  const double ty = this->m_Translation[1];
  const double tz = this->m_Translation[2];

  OffsetType          offset;   

  /** Recompute the offset according to the centre of rotation and
    * translation */
  if(this->GetComputeZYX())
    {
    offset[0] = cz*cy*(tx - ox) + (-sz*cx + cz*sy*sx)*(ty - oy) + ( sz*sx + cz*sy*cx)*(tz - oz) + ox;
    offset[1] = sz*cy*(tx - ox) + ( cz*cx + sz*sy*sx)*(ty - oy) + (-cz*sx + sz*sy*cx)*(tz - oz) + oy;
    offset[2] =                               -sy*(tx - ox) + cy*sx*(ty - oy) + cy*cx*(tz - oz) + oz;
    }
  else
    {
    offset[0] = (cz*cy - sz*sy*sx)*(tx - ox) - sz*cx*(ty - oy) + (cz*sy + sz*sx*cy)*(tz - oz) + ox;
    offset[1] = (sz*cy + cz*sy*sx)*(tx - ox) + cz*cx*(ty - oy) + (sz*sy - cz*sx*cy)*(tz - oz) + oy;
    offset[2] =                             -cx*sy*(tx - ox) + sx*(ty - oy) + cy*cx*(tz - oz) + oz;
    }

  this->SetOffset( offset );
  this->Modified();
}

} // namespace

#endif
