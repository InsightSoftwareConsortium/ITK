/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransform.txx
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
#ifndef _itkVersorRigid3DTransform_txx
#define _itkVersorRigid3DTransform_txx

#include "itkVersorRigid3DTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
VersorRigid3DTransform<TScalarType>
::VersorRigid3DTransform()
{

}

// Copy Constructor
template <class TScalarType>
VersorRigid3DTransform<TScalarType>
::VersorRigid3DTransform( const Self & other )
{
  // call the superclass copy constructor
}

// Set Parameters
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{


  // Transfer the versor part
  
  AxisType axis;

  axis[0] = parameters[0];
  axis[1] = parameters[1];
  axis[2] = parameters[2];

  const TScalarType angle  = parameters[3];

  m_Versor.Set( axis, angle );


  
  
  // Transfer the translation part
  
  OffsetType offset;
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    offset[i] = parameters[i+3];
    }

  
  this->SetOffset( offset );

  ComputeMatrix();

}


// Set Rotational Part
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetRotation( const VersorType & versor )
{
    m_Versor = versor;
    ComputeMatrix();
}



// Set Rotational Part
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::SetRotation( const AxisType & axis, AngleType  angle )
{
    m_Versor.Set( axis, angle );
    ComputeMatrix();
}


// Compute the matrix
template <class TScalarType>
void
VersorRigid3DTransform<TScalarType>
::ComputeMatrix( void )
{

  const TScalarType vx = m_Versor.GetX();
  const TScalarType vy = m_Versor.GetY();
  const TScalarType vz = m_Versor.GetZ();
  const TScalarType vw = m_Versor.GetW();
      
  const TScalarType xx = vx * vx;
  const TScalarType yy = vy * vy;
  const TScalarType zz = vz * vz;
  const TScalarType xy = vx * vy;
  const TScalarType xz = vx * vz;
  const TScalarType xw = vx * vw;
  const TScalarType yz = vy * vz;
  const TScalarType yw = vy * vw;
  const TScalarType zw = vz * vw;

  m_DirectMatrix[0][0] = 1.0 - 2.0 * ( yy + zz );
  m_DirectMatrix[1][1] = 1.0 - 2.0 * ( xx + zz );
  m_DirectMatrix[2][2] = 1.0 - 2.0 * ( xx + yy );
  m_DirectMatrix[0][1] = 2.0 * ( xy - zw );
  m_DirectMatrix[0][2] = 2.0 * ( xz + yw );
  m_DirectMatrix[1][0] = 2.0 * ( xy + zw );
  m_DirectMatrix[2][0] = 2.0 * ( xz - yw );
  m_DirectMatrix[2][1] = 2.0 * ( yz + xw );
  m_DirectMatrix[1][2] = 2.0 * ( yz - xw );
 
  m_InverseMatrix = m_DirectMatrix.GetTranspose();

}


// Set parameters
template<class TScalarType>
const VersorRigid3DTransform<TScalarType>::JacobianType &
VersorRigid3DTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{
  
  typedef typename VersorType::ValueType  ValueType;

  // compute derivatives with respect to rotation
  const ValueType vx = m_Versor.GetX();
  const ValueType vy = m_Versor.GetY();
  const ValueType vz = m_Versor.GetZ();
  const ValueType vw = m_Versor.GetW();

  m_Jacobian.Fill(0.0);

  // compute Jacobian with respect to quaternion parameters
  m_Jacobian[0][0] = 2.0 * (  vx * p[0] + vy * p[1] + vz * p[2] );
  m_Jacobian[0][1] = 2.0 * (- vy * p[0] + vx * p[1] + vw * p[2] );
  m_Jacobian[0][2] = 2.0 * (- vz * p[0] - vw * p[1] + vx * p[2] );
  m_Jacobian[0][3] = 2.0 * (  vw * p[0] - vz * p[1] + vy * p[2] );

  m_Jacobian[1][0] = - m_Jacobian[0][1];
  m_Jacobian[1][1] =   m_Jacobian[0][0];
  m_Jacobian[1][2] = - m_Jacobian[0][3];
  m_Jacobian[1][3] =   m_Jacobian[0][2];

  m_Jacobian[2][0] = - m_Jacobian[0][2];
  m_Jacobian[2][1] =   m_Jacobian[0][3];
  m_Jacobian[2][2] =   m_Jacobian[0][0];
  m_Jacobian[2][3] = - m_Jacobian[0][1];


  // compute derivatives for the translation part
  unsigned int blockOffset = 4;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
     m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return m_Jacobian;

}
  
} // namespace

#endif
