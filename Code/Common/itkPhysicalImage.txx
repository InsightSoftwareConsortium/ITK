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
#include "itkPhysicalImage.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
::~PhysicalImage()
{
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
const double *
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
::GetSpacing() const
{
  return m_Spacing;
}




//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
void 
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
const double *
PhysicalImage<TPixel, VImageDimension, TPixelContainer>
::GetOrigin() const
{
  return m_Origin;
}

//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
PhysicalImage<TPixel, VImageDimension, TPixelContainer>::AffineTransformType
PhysicalImage<TPixel, VImageDimension, TPixelContainer>::
GetIndexToPhysicalTransform()
{
  AffineTransformType::MatrixType matrix;
  AffineTransformType::VectorType offset;
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
template<class TPixel, unsigned int VImageDimension, class TPixelContainer>
PhysicalImage<TPixel, VImageDimension, TPixelContainer>::AffineTransformType
PhysicalImage<TPixel, VImageDimension, TPixelContainer>::
GetPhysicalToIndexTransform()
{
  AffineTransformType::MatrixType matrix;
  AffineTransformType::VectorType offset;

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
