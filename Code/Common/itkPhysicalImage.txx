/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhysicalImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
