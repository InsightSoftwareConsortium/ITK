/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhysicalImage.txx
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
#ifndef _itkPhysicalImage_txx
#define _itkPhysicalImage_txx
#include "itkPhysicalImage.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::PhysicalImage()
{
  unsigned int i;
  for (i=0; i < VImageDimension; i++)
    {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::~PhysicalImage()
{
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::SetSpacing(const double spacing[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    } 
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::SetSpacing(const float spacing[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    } 
}



//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
const double *
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::GetSpacing() const
{
  return m_Spacing;
}




//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::SetOrigin(const double origin[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::SetOrigin(const float origin[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
const double *
PhysicalImage<TPixel, VImageDimension, TImageTraits>
::GetOrigin() const
{
  return m_Origin;
}

//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
PhysicalImage<TPixel, VImageDimension, TImageTraits>::AffineTransformType
PhysicalImage<TPixel, VImageDimension, TImageTraits>::
GetIndexToPhysicalTransform(void) const
{
  typename AffineTransformType::MatrixType matrix;
  typename AffineTransformType::VectorType offset;
  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    for (unsigned int j = 0; j < VImageDimension; j++)
      {
      matrix[i][j] = 0.0;
      }
    matrix[i][i] = m_Spacing[i];
    offset[i]    = m_Origin [i];
    }

  AffineTransformType result(matrix, offset);
  result.SetMatrix(matrix);
  result.SetOffset(offset);

  return result;
}


//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
PhysicalImage<TPixel, VImageDimension, TImageTraits>::AffineTransformType
PhysicalImage<TPixel, VImageDimension, TImageTraits>::
GetPhysicalToIndexTransform(void) const
{
  typename AffineTransformType::MatrixType matrix;
  typename AffineTransformType::VectorType offset;

  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    for (unsigned int j = 0; j < VImageDimension; j++)
      {
      matrix[i][j] = 0.0;
      }
    matrix[i][i] = 1.0 / m_Spacing[i];
    offset[i]    = -m_Origin[i] / m_Spacing[i];
    }

  AffineTransformType result(matrix, offset);

  return result;
}

} // end namespace itk

#endif
