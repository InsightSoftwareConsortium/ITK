/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContinuousImageFunction.h
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
#ifndef _itkContinuousImageFunction_h
#define _itkContinuousImageFunction_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkContinuousIndex.h"
#include "itkVector.h"

namespace itk
{

/** 
 * \class ContinuousImageFunction
 * \brief Evaluates a function of an image at specified point.
 *
 * ContinuousImageFunction is a baseclass for all objects that evaluates
 * a function of an image at point. This class is templated over 
 * the input image type and the type of the function output.
 *
 * The input image is set via method SetInputImage().
 * The Evaluate() method evaluates the function at an point.
 * Optionally, image origin and spacing can be specify via methods
 * SetImageOrigin() and SetImageSpacing. The default origin is 0 and
 * spacing is 1.0 for all dimension.
 *
 */
template <
class TInputImage, 
class TOutput 
>
class ITK_EXPORT ContinuousImageFunction : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ContinuousImageFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * OutputType typedef support.
   */
  typedef TOutput OutputType;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self); 

  /**
   * InputImagePointer typedef support
   */ 
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /**
   * InputPixel typedef support
   */
  typedef typename InputImageType::PixelType PixelType;

  /**
   * Dimension underlying input image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Point Type
   */
  typedef Point<double,ImageDimension> PointType;

  /**
   * ContinuousIndex Type
   */
  typedef ContinuousIndex<double,ImageDimension> ContinuousIndexType;

  /**
   * Vector Type
   */
  typedef Vector<double,ImageDimension> VectorType;

  
  /** 
   * Set the input image.
   */
  virtual void SetInputImage( const InputImageType * ptr );

  /**
   * Get the input image.
   */
  InputImageConstPointer GetInputImage() const
    { return m_Image.GetPointer(); }

  /**
   * Set the image spacing
   */
  void SetImageSpacing( const VectorType& spacing );

  /**
   * Get the image spacing
   */
  const VectorType& GetImageSpacing() const
    { return m_ImageSpacing; }

  /**
   * Set the image origin
   */
  void SetImageOrigin( const PointType& spacing );

  /**
   * Get the image origin
   */
  const PointType& GetImageOrigin() const
    { return m_ImageOrigin; } 

  /**
   * Evaluate the function at a geometric point position
   */
  virtual TOutput Evaluate( const PointType& point ) const = 0;

  /**
   * Evaluate the function at continuous index position
   */
  virtual TOutput Evaluate( const ContinuousIndexType& index )
    {
    PointType point;
    this->ConvertContinuousIndexToPoint( index, point );
    return( this->Evaluate( point ) );
    }

  /**
   * Check if a point inside the image buffer
   */
  bool IsInsideBuffer( const PointType& point ) const
    {
      for( unsigned int j = 0; j < ImageDimension; j++ )
        {
        if( point[j] < m_GeometricStart[j] ||
            point[j] > m_GeometricEnd[j] ) return false;
        }
      return true;
    }

  bool IsInsideBuffer( const ContinuousIndexType& index ) const
    {
      for( unsigned int j = 0; j < ImageDimension; j++ )
        {
        if( index[j] < m_BufferStart[j] ||
            index[j] > m_BufferEnd[j] ) return false;
        }
      return true;
    }


protected:

  ContinuousImageFunction();
  ~ContinuousImageFunction(){};
  ContinuousImageFunction( const Self& ){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  // made protected so subclass can access
  InputImageConstPointer  m_Image;
  PointType               m_GeometricStart;
  PointType               m_GeometricEnd;
  ContinuousIndexType     m_BufferStart;
  ContinuousIndexType     m_BufferEnd;

  PointType               m_ImageOrigin;
  VectorType              m_ImageSpacing;


  /**
   * Convert from geometric coordinate to image coordinates
   */
  void ConvertPointToContinuousIndex( 
    const PointType& point, ContinuousIndexType& index  ) const
    {
      for( unsigned int j = 0; j < ImageDimension; j++ )
        {
        index[j] = ( point[j] - m_ImageOrigin[j] ) / m_ImageSpacing[j]; 
        }
    }

  /**
   * Convert image index to geometric coordinate
   */
  void ConvertContinuousIndexToPoint(
    const ContinuousIndexType& index, PointType& point ) const
    {
      for( unsigned int j = 0; j < ImageDimension; j++ )
        {
        point[j] = index[j] * m_ImageSpacing[j] + m_ImageOrigin[j];
        }
    }

private:

  /**
   * Compute buffer limits
   */
  void ComputeBufferLimits();

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkContinuousImageFunction.txx"
#endif

#endif
