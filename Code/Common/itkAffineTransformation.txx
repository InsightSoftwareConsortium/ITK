/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: 
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
#ifndef _itkAffineTransformation_txx
#define _itkAffineTransformation_txx

#include <itkExceptionObject.h>
#include "itkAffineTransformation.h"

namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,int NDimensions>
AffineTransformation<TScalarType,NDimensions>
::AffineTransformation()
{   
  m_SetMatrix = false;
  m_SetOffset = false;
  m_SetLinear = false;
}


/**
 * Assignment Operator
 */
template <class TScalarType,int NDimensions>
const AffineTransformation<TScalarType,NDimensions> &
AffineTransformation<TScalarType,NDimensions>
::operator=( const Self & other )
{
  m_Matrix = other.m_Matrix;
  m_Offset = other.m_Offset;
  m_Linear = other.m_Linear;
  m_SetMatrix = other.m_SetMatrix;
  m_SetOffset = other.m_SetOffset;
  m_SetLinear = other.m_SetLinear;
  return *this;
}


/**
 * Set the matrix
 */
template <class TScalarType,int NDimensions>
void
AffineTransformation<TScalarType,NDimensions>
::SetMatrix(const MatrixType &matrix)
{
  m_Matrix = matrix;
  m_SetMatrix = true;
}


/**
 * Set the Linear part
 */
template <class TScalarType,int NDimensions>
void
AffineTransformation<TScalarType,NDimensions>
::SetLinear(const LinearType &linear)
{
  m_Linear = linear;
  m_SetLinear = true;
}



/**
 * Set the constant part
 */
template <class TScalarType,int NDimensions>
void
AffineTransformation<TScalarType,NDimensions>
::SetOffset(const VectorType &offset)
{
  m_Offset = offset;
  m_SetOffset = true;

  /*Init the Linear as the Identity*/
  if( m_SetLinear == false )
    {
    for(unsigned int i=0; i<NDimensions ; i++)
      {
      for(unsigned int j=0; j<NDimensions ; j++)
        {
        if(i == j) m_Linear(i,j) = 1;
        else  m_Linear(i,j) = 0;
        }
      }
    m_SetLinear = true;
    }
}




/***
 *Perform Y = Linear*X + Offset and return Y
 */
template <class TScalarType,int NDimensions>
AffineTransformation<TScalarType,NDimensions>::PointType
AffineTransformation<TScalarType,NDimensions>
::Transform( PointType &initial_point )
{
  //Matrix case
  PointType result;
  if( m_SetMatrix == true)
    {
    for (int i = 0; i < NDimensions; i++) 
      { 
      result[i] = m_Matrix[NDimensions][i];
      for (int j = 0; j < NDimensions; j++) 
        {
        result[i] += m_Matrix[i][j] * initial_point[j];
        }
      }
    }
  
  //Linear + Offset case
  else 
    {
    if((m_SetLinear == true) && (m_SetOffset == true))
      {
      for (int i = 0; i < NDimensions; i++) 
        { 
        result[i] = m_Offset[i];
        for (int j = 0; j < NDimensions; j++) 
          { 
          result[i] += m_Linear[i][j] * initial_point[j];
          }
        }
      }
    } 

  return result;

}


} // end namespace itk

#endif
