/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleSkewVersor3DTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScaleSkewVersor3DTransform_txx
#define _itkScaleSkewVersor3DTransform_txx

#include "itkScaleSkewVersor3DTransform.h"


namespace itk
  {

// Constructor with default arguments
template <class TScalarType>
ScaleSkewVersor3DTransform<TScalarType>
::ScaleSkewVersor3DTransform():Superclass(OutputSpaceDimension, ParametersDimension)
  {
  m_Scale.Fill( 1.0 );
  m_Skew.Fill( 0.0 );
  this->ComputeMatrixAndOffset();
  }


// Constructor with arguments
template<class TScalarType>
ScaleSkewVersor3DTransform<TScalarType>::
ScaleSkewVersor3DTransform( unsigned int spaceDimension, 
                        unsigned int parametersDimension):
  Superclass(spaceDimension,parametersDimension)
  {
  // note: this virtual function will only
  // call the one defined in this class because 
  // we are in a constructor
  this->ComputeMatrixAndOffset(); 
  }
 


// Copy Constructor
template <class TScalarType>
ScaleSkewVersor3DTransform<TScalarType>
::ScaleSkewVersor3DTransform( const Self & other )
  {
  // call the superclass copy constructor
  this->ComputeMatrixAndOffset();
  }

// Set Parameters
template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
  {

  itkDebugMacro( << "Setting paramaters " << parameters );


  // Transfer the versor part
  
  AxisType axis;

  axis[0] = parameters[0];
  axis[1] = parameters[1];
  axis[2] = parameters[2];

  VersorType versor;
  versor.Set( axis );
  
  Superclass::SetRotation( versor );

  itkDebugMacro( <<"Versor is now " << versor );
  
  InputPointType center;
  // Transfer the center part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    center[i] = parameters[i+SpaceDimension];
    }
  Superclass::SetCenter( center );

  OutputVectorType translation;
  // Transfer the translation part
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    translation[j] = parameters[j+2*SpaceDimension];
    }
  Superclass::SetTranslation( translation );
  
  for(unsigned int k=0; k < SpaceDimension; k++) 
    {
    m_Scale[k] = parameters[k+3*SpaceDimension];
    }

  for(unsigned int l=0; l < 6; l++) 
    {
    m_Skew[l] = parameters[l+4*SpaceDimension];
    }

  this->ComputeMatrixAndOffset();

  itkDebugMacro(<<"After setting paramaters ");
  }



//
// Get Parameters
// 
// Parameters are ordered as:
//
// p[0:2] = right part of the versor (axis times sin(t/2))
// p[3:5} = center of rotation coordinates
// p[6:8} = translation components
// p[9:11} = Scale
// p[12:17} = Skew {xy, xz, yx, yz, zx, zy}
//

template <class TScalarType>
const typename ScaleSkewVersor3DTransform<TScalarType>::ParametersType &
ScaleSkewVersor3DTransform<TScalarType>
::GetParameters( void ) const
  {
  itkDebugMacro( << "Getting parameters ");

  VersorType versor = Superclass::GetVersor();
  InputPointType center = Superclass::GetCenter();
  OutputVectorType translation = Superclass::GetTranslation();

  m_Parameters[0] = versor.GetX();
  m_Parameters[1] = versor.GetY();
  m_Parameters[2] = versor.GetZ();

  // Transfer the center of rotation 
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    m_Parameters[i+SpaceDimension] = center[i];
    }

  // Transfer the translation
  for(unsigned int j=0; j < SpaceDimension; j++) 
    {
    m_Parameters[j+2*SpaceDimension] = translation[j];
    }

  for(unsigned int k=0; k < SpaceDimension; k++) 
    {
    m_Parameters[k+3*SpaceDimension] = m_Scale[k];
    }

  for(unsigned int l=0; l < 6; l++) 
    {
    m_Parameters[l+4*SpaceDimension] = m_Skew[l];  
    }

  itkDebugMacro(<<"After getting parameters " << m_Parameters );

  return m_Parameters;
  }

template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetIdentity()
  {
  m_Scale.Fill( 1.0 );
  m_Skew.Fill( 0.0 );
  Superclass::SetIdentity();
  }


template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetScale( const ScaleVectorType & scale )
{
  m_Scale = m_Scale;
  this->ComputeMatrixAndOffset();
}

template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::SetSkew( const SkewVectorType & skew )
{
  m_Skew = m_Skew;
  this->ComputeMatrixAndOffset();
}

// Compute the matrix
template <class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>
::ComputeMatrixAndOffset( void )
{

  VersorType versor = Superclass::GetVersor();
  
  const TScalarType vx = versor.GetX();
  const TScalarType vy = versor.GetY();
  const TScalarType vz = versor.GetZ();
  const TScalarType vw = versor.GetW();
      
  const TScalarType xx = vx * vx;
  const TScalarType yy = vy * vy;
  const TScalarType zz = vz * vz;
  const TScalarType xy = vx * vy;
  const TScalarType xz = vx * vz;
  const TScalarType xw = vx * vw;
  const TScalarType yz = vy * vz;
  const TScalarType yw = vy * vw;
  const TScalarType zw = vz * vw;

  m_RotationMatrix[0][0] = m_Scale[0] - 2.0 * ( yy + zz );
  m_RotationMatrix[1][1] = m_Scale[1] - 2.0 * ( xx + zz );
  m_RotationMatrix[2][2] = m_Scale[2] - 2.0 * ( xx + yy );
  m_RotationMatrix[0][1] = 2.0 * ( xy - zw )  + ( m_Skew[0] );
  m_RotationMatrix[0][2] = 2.0 * ( xz + yw )  + ( m_Skew[1] );
  m_RotationMatrix[1][0] = 2.0 * ( xy + zw )  + ( m_Skew[2] );
  m_RotationMatrix[1][2] = 2.0 * ( yz - xw )  + ( m_Skew[3] );
  m_RotationMatrix[2][0] = 2.0 * ( xz - yw )  + ( m_Skew[4] );
  m_RotationMatrix[2][1] = 2.0 * ( yz + xw )  + ( m_Skew[5] );
 
  m_InverseMatrix = m_RotationMatrix.GetInverse();

  InputPointType center = Superclass::GetCenter();
  OutputVectorType translation = Superclass::GetTranslation();
  OffsetType offset; 
  for(unsigned int i=0; i<3; i++)
    {
    offset[i] = translation[i] + center[i];  
    for(unsigned int j=0; j<3; j++)
      {
      offset[i] -= m_RotationMatrix[i][j] * center[j];
      }
    }

  this->SetOffset( offset );
}

 
// Print self
template<class TScalarType>
void
ScaleSkewVersor3DTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  
  os << indent << "Scale:       " << m_Scale        << std::endl;
  os << indent << "Skew:        " << m_Skew         << std::endl;
}

} // namespace

#endif
