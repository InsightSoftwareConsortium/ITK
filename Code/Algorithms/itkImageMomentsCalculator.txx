/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMomentsCalculator.txx
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
  m_Valid = 0;
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

  m_M0 = 0.0;
  m_M1.Fill( 0.0 );
  m_M2.Fill( 0.0 );
  m_Cg.Fill( 0.0 );
  m_Cm.Fill( 0.0 );

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

    m_M0 += value;

    for(unsigned int i=0; i<ImageDimension; i++)
    {
      m_M1[i] += indexPosition[i] * value; 
      for(unsigned int j=0; j<ImageDimension; j++)
      {
        double weight = value * indexPosition[i] * indexPosition[j];
        m_M2[i][j] += weight;
      }
    }

    physicalPosition = indexToPhysical->TransformPoint( indexPosition );
      
    for(unsigned int i=0; i<ImageDimension; i++)
    {
      m_Cg[i] += physicalPosition[i] * value; 
      for(unsigned int j=0; j<ImageDimension; j++)
      {
        double weight = value * physicalPosition[i] * physicalPosition[j];
        m_Cm[i][j] += weight;
      }

    }

    ++it;
  }

  // Throw an error if the total mass is zero
  if ( m_M0 == 0.0 )
    throw InvalidImageMomentsError(__FILE__, __LINE__);

  // Normalize using the total mass
  for(unsigned int i=0; i<ImageDimension; i++)
  {
    m_Cg[i] /= m_M0;
    m_M1[i] /= m_M0;
    for(unsigned int j=0; j<ImageDimension; j++)
    {
      m_M2[i][j] /= m_M0;
      m_Cm[i][j] /= m_M0;
    }
  }

  // Center the second order moments
  for(unsigned int i=0; i<ImageDimension; i++)
  {
    for(unsigned int j=0; j<ImageDimension; j++)
    {
      m_M2[i][j] -= m_M1[i] * m_M1[j];
      m_Cm[i][j] -= m_Cg[i] * m_Cg[j];
    }
  }

  // Compute principal moments and axes
  vnl_symmetric_eigensystem<double> eigen( m_Cm.GetVnlMatrix() );
  vnl_diag_matrix<double> pm = eigen.D;
  for(unsigned int i=0; i<ImageDimension; i++)
  {
    m_Pm[i] = pm(i,i) * m_M0;
  }
  m_Pa = eigen.V.transpose();

  // Add a final reflection if needed for a proper rotation,
  // by multiplying the last row by the determinant
  vnl_real_eigensystem eigenrot( m_Pa.GetVnlMatrix() );
  vnl_diag_matrix< vcl_complex<double> > eigenval = eigenrot.D;
  vcl_complex<double> det( 1.0, 0.0 );

  for(unsigned int i=0; i<ImageDimension; i++)
  {
    det *= eigenval( i, i );
  }

  for(unsigned int i=0; i<ImageDimension; i++)
  {
    m_Pa[ ImageDimension-1 ][i] *= std::real( det );
  }
  
  /* Remember that the moments are valid */
  m_Valid = 1;

}


//---------------------------------------------------------------------
// Get sum of intensities
template<class TImage>
ImageMomentsCalculator<TImage>::ScalarType
ImageMomentsCalculator<TImage>::
GetTotalMass()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_M0;
}

//--------------------------------------------------------------------
// Get first moments about origin, in index coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetFirstMoments()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_M1;
}

//--------------------------------------------------------------------
// Get second moments about origin, in index coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetSecondMoments()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_M2;
}

//--------------------------------------------------------------------
// Get center of gravity, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetCenterOfGravity()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_Cg;
}

//--------------------------------------------------------------------
// Get second central moments, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetCentralMoments()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_Cm;
}

//--------------------------------------------------------------------
// Get principal moments, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::VectorType
ImageMomentsCalculator<TImage>::
GetPrincipalMoments()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_Pm;
}



//--------------------------------------------------------------------
// Get principal axes, in physical coordinates
template<class TImage>
ImageMomentsCalculator<TImage>::MatrixType
ImageMomentsCalculator<TImage>::
GetPrincipalAxes()
{
  if (!m_Valid)        throw InvalidImageMomentsError(__FILE__, __LINE__);
  return m_Pa;
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
      offset[i]  = m_Cg [i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_Pa[i][j];    // Note the transposition
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
      offset[i]    = m_Cg [i];
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        matrix[j][i] = m_Pa[i][j];    // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();
    result->SetMatrix(matrix);
    result->SetOffset(offset);

    return result->Inverse();
}

} // end namespace itk



#endif
