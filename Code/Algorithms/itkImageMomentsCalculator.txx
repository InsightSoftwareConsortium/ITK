/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMomentsCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000-2001 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageMomentsCalculator_txx
#define _itkImageMomentsCalculator_txx


#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{ 


template<class TImage>
const char * ImageMomentsCalculator<TImage>::notvalid = "No valid image moments are available.";


//----------------------------------------------------------------------
// Construct without computing moments
template<class TImage>
ImageMomentsCalculator<TImage>::ImageMomentsCalculator(void) 
{
  m_valid = 0;
}

//-----------------------------------------------------------------------
// Construct and compute moments
template<class TImage>
ImageMomentsCalculator<TImage>::
ImageMomentsCalculator( ImageType * image) 
{
  ComputeMoments(image);
}

//----------------------------------------------------------------------
// Destructor
template<class TImage>
ImageMomentsCalculator<TImage>::
~ImageMomentsCalculator()
{
}

//----------------------------------------------------------------------
// Compute moments for a new or modified image
template<class TImage>
void
ImageMomentsCalculator<TImage>::
ComputeMoments( ImageType * image)
{

  AffineTransformType indexToPhysical = image->GetIndexToPhysicalTransform();

  m_m0 = 0.0;
  m_m1.Fill( 0.0 );
  m_m2.Fill( 0.0 );
  m_cg.Fill( 0.0 );
  m_cm.Fill( 0.0 );

  typedef typename ImageType::IndexType IndexType;
    
  SimpleImageRegionIterator< ImageType > it( image,
                                   image->GetRequestedRegion() ); 

  it.Begin();
  while( !it.IsAtEnd() )
  {
    double value = it.Value();
    IndexType index = it.GetIndex();
    Point<double,ImageDimension> indexPosition;
    Point<double,ImageDimension> physicalPosition;

    for(unsigned int i=0; i<ImageType::ImageDimension; i++)
    {
      indexPosition[i] = index[i];
    }

    m_m0 += value;

    for(unsigned int i=0; i<ImageDimension; i++)
    {
      m_m1[i] += indexPosition[i] * value; 
      for(unsigned int j=0; j<ImageDimension; j++)
      {
        double weight = value * indexPosition[i] * indexPosition[j];
        m_m2[i][j] += weight;
      }
    }

    physicalPosition = indexToPhysical.Transform( indexPosition );
      
    for(unsigned int i=0; i<ImageDimension; i++)
    {
      m_cg[i] += physicalPosition[i] * value; 
      for(unsigned int j=0; j<ImageDimension; j++)
      {
        double weight = value * physicalPosition[i] * physicalPosition[j];
        m_cm[i][j] += weight;
      }

    }

    ++it;
  }

  // Normalize using the total mass
  for(unsigned int i=0; i<ImageDimension; i++)
  {
    m_cg[i] /= m_m0;
    m_m1[i] /= m_m0;
    for(unsigned int j=0; j<ImageDimension; j++)
    {
      m_m2[i][j] /= m_m0;
      m_cm[i][j] /= m_m0;
    }
  }


  // Center the second order moments
  for(unsigned int i=0; i<ImageDimension; i++)
  {
    for(unsigned int j=0; j<ImageDimension; j++)
    {
      m_m2[i][j] -= m_m1[i] * m_m1[j];
      m_cm[i][j] -= m_cg[i] * m_cg[j];
    }
  }

  // Compute principal moments and axes
  vnl_symmetric_eigensystem<double> eigen( m_cm.GetVnlMatrix() );
  vnl_diag_matrix<double> pm = eigen.D;
  for(unsigned int i=0; i<ImageDimension; i++)
  {
    m_pm[i] = pm(i,i);
  }
  m_pa = eigen.V.transpose();

  // Add a final reflection if needed for a proper rotation,
  // by multiplying the last row by the determinant
  vnl_real_eigensystem eigenrot( m_pa.GetVnlMatrix() );
  vnl_diag_matrix< vnl_double_complex> eigenval = eigenrot.D;
  vnl_double_complex det( 1.0, 0.0 );

  for(unsigned int i=0; i<ImageDimension; i++)
  {
    det *= eigenval( i, i );
  }

  for(unsigned int i=0; i<ImageDimension; i++)
  {
    m_pa[ ImageDimension-1 ][i] *= std::real( det );
  }
  
  /* Remember that the moments are valid */
  m_valid = 1;

}


//---------------------------------------------------------------------
// Get sum of intensities
template<class TImage>
ImageMomentsCalculator<TImage>::ScalarType
ImageMomentsCalculator<TImage>::
GetTotalMass()
{
  if (!m_valid)    Error(notvalid);
  return m_m0;
}

//--------------------------------------------------------------------
// Get first moments about origin, in index coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetFirstMoments()
{
  if (!m_valid)    Error(notvalid);
  return m_m1;
}

//--------------------------------------------------------------------
// Get second moments about origin, in index coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetSecondMoments()
{
  if (!m_valid)    Error(notvalid);
  return m_m2;
}

//--------------------------------------------------------------------
// Get center of gravity, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetCenterOfGravity()
{
  if (!m_valid)    Error(notvalid);
  return m_cg;
}

//--------------------------------------------------------------------
// Get second central moments, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetCentralMoments()
{
  if (!m_valid)    Error(notvalid);
  return m_cm;
}

//--------------------------------------------------------------------
// Get principal moments, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetPrincipalMoments()
{
  if (!m_valid)    Error(notvalid);
  return m_pm;
}



//--------------------------------------------------------------------
// Get principal axes, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetPrincipalAxes()
{
  if (!m_valid)    Error(notvalid);
  return m_pa;
}



//--------------------------------------------------------------------
// Get principal axes to physical axes transform
template<class TImage>
ImageMomentsCalculator<TImage>::AffineTransformType
ImageMomentsCalculator<TImage>::
GetPrincipalAxesToPhysicalAxesTransform(void) const
{
    AffineTransformType::MatrixType matrix;
    AffineTransformType::VectorType offset;
    for (unsigned int i = 0; i < ImageDimension; i++) 
    {
      offset[i]  = m_cg [i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_pa[i][j];    // Note the transposition
      }
    }

    AffineTransformType result(matrix, offset);
    result.SetMatrix(matrix);
    result.SetOffset(offset);

    return result;
}


//--------------------------------------------------------------------
// Get physical axes to principal axes transform

template<class TImage>
ImageMomentsCalculator<TImage>::AffineTransformType
ImageMomentsCalculator<TImage>::
GetPhysicalAxesToPrincipalAxesTransform(void) const
{
    AffineTransformType::MatrixType matrix;
    AffineTransformType::VectorType offset;
    for (unsigned int i = 0; i < ImageDimension; i++) 
    {
      offset[i]    = m_cg [i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_pa[i][j];    // Note the transposition
      }
    }

    AffineTransformType result(matrix, offset);
    result.SetMatrix(matrix);
    result.SetOffset(offset);

    return result.Inverse();
}

//--------------------------------------------------------------------
/**
 * This private and interim method reports a error by printing
 * a given char string to standard error and aborting.  This
 * is a purely temporary method used as a placeholder until
 * the right implementation of error handling in itk is designed.
 */
template<class TImage>
void
ImageMomentsCalculator<TImage>::
Error (const char *string) {
   std::cerr << string << "\n";
   ExceptionObject problem;
   problem.SetLocation("ImageMomentsCalculator");
   problem.SetDescription( string );
   throw problem;
}


} // end namespace itk



#endif
