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
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{ 


class ITK_EXPORT InvalidImageMomentsError : public ExceptionObject
{
 public:
  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidImageMomentsError(const char *file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) { this->SetDescription("No valid image moments are availble.");}

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidImageMomentsError(const std::string& file, unsigned int lineNumber) : ExceptionObject(file, lineNumber) { this->SetDescription("No valid image moments are availble.");}  
  
  itkTypeMacro(InvalidImageMomentsError, ExceptionObject);
};

  
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
ComputeMoments( ImageType * image )
{

  AffineTransformPointer indexToPhysical 
          = image->GetIndexToPhysicalTransform();

  m_m0 = 0.0;
  m_m1.Fill( 0.0 );
  m_m2.Fill( 0.0 );
  m_cg.Fill( 0.0 );
  m_cm.Fill( 0.0 );

  typedef typename ImageType::IndexType IndexType;
    
  ImageRegionIteratorWithIndex< ImageType > it( image,
                                                image->GetRequestedRegion() ); 

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

    physicalPosition = indexToPhysical->TransformPoint( indexPosition );
      
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

  // Throw an error if the total mass is zero
  if ( m_m0 == 0.0 )
    throw InvalidImageMomentsError(__FILE__, __LINE__);

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
    m_pm[i] = pm(i,i) * m_m0;
  }
  m_pa = eigen.V.transpose();

  // Add a final reflection if needed for a proper rotation,
  // by multiplying the last row by the determinant
  vnl_real_eigensystem eigenrot( m_pa.GetVnlMatrix() );
  vnl_diag_matrix< vcl_complex<double> > eigenval = eigenrot.D;
  vcl_complex<double> det( 1.0, 0.0 );

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
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_m0;
}

//--------------------------------------------------------------------
// Get first moments about origin, in index coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetFirstMoments()
{
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_m1;
}

//--------------------------------------------------------------------
// Get second moments about origin, in index coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetSecondMoments()
{
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_m2;
}

//--------------------------------------------------------------------
// Get center of gravity, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetCenterOfGravity()
{
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_cg;
}

//--------------------------------------------------------------------
// Get second central moments, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetCentralMoments()
{
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_cm;
}

//--------------------------------------------------------------------
// Get principal moments, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetPrincipalMoments()
{
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_pm;
}



//--------------------------------------------------------------------
// Get principal axes, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetPrincipalAxes()
{
  if (!m_valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_pa;
}



//--------------------------------------------------------------------
// Get principal axes to physical axes transform
template<class TImage>
ImageMomentsCalculator<TImage>::AffineTransformPointer
ImageMomentsCalculator<TImage>::
GetPrincipalAxesToPhysicalAxesTransform(void) const
{
    AffineTransformType::MatrixType matrix;
    AffineTransformType::OffsetType offset;
    for (unsigned int i = 0; i < ImageDimension; i++) 
    {
      offset[i]  = m_cg [i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_pa[i][j];    // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();
    
    result->SetMatrix(matrix);
    result->SetOffset(offset);

    return result;
}


//--------------------------------------------------------------------
// Get physical axes to principal axes transform

template<class TImage>
ImageMomentsCalculator<TImage>::AffineTransformPointer
ImageMomentsCalculator<TImage>::
GetPhysicalAxesToPrincipalAxesTransform(void) const
{
    AffineTransformType::MatrixType matrix;
    AffineTransformType::OffsetType offset;
    for (unsigned int i = 0; i < ImageDimension; i++) 
    {
      offset[i]    = m_cg [i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_pa[i][j];    // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();
    result->SetMatrix(matrix);
    result->SetOffset(offset);

    return result->Inverse();
}

} // end namespace itk



#endif
