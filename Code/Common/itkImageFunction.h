/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFunction.h
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
#ifndef _itkImageFunction_h
#define _itkImageFunction_h

#include "itkFunctionBase.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkContinuousIndex.h"

namespace itk
{

/** 
 * \class ImageFunction
 * \brief Evaluates a function of an image at specified position.
 *
 * ImageFunction is a baseclass for all objects that evaluates
 * a function of an image at index, continuous index or point.
 * This class is templated over the input image type and the type 
 * of the function output.
 *
 * The input image is set via method SetInputImage().
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex
 * respectively evaluates the function at an geometric point,
 * image index and continuous image index.
 *
 * \warning Image BufferedRegion information is cached during
 * in SetInputImage( image ). If the image BufferedRegion has changed
 * one must call SetInputImage( image ) again to update the cache 
 * to the current values.
 *
 * \sa Point
 * \sa Index
 * \sa Continuous
 *
 * \ingroup ImageFunctions
 *
 */
template <
class TInputImage, 
class TOutput 
>
class ITK_EXPORT ImageFunction : 
  public FunctionBase< Point<double,TInputImage::ImageDimension>, 
                       TOutput > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FunctionBase< Point<double,TInputImage::ImageDimension>,
            TOutput > Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageFunction, FunctionBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self); 

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * InputPixel typedef support
   */
  typedef typename InputImageType::PixelType PixelType;

  /**
   * InputImagePointer typedef support
   */ 
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /**
   * Dimension underlying input image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * OutputType typedef support.
   */
  typedef TOutput OutputType;

  /**
   * Index Type.
   */
  typedef typename InputImageType::IndexType IndexType;

  /**
   * ContinuousIndex Type.
   */
  typedef ContinuousIndex<double,ImageDimension> ContinuousIndexType;

  /**
   * Point Type.
   */
  typedef Point<double,ImageDimension> PointType;

  /** 
   * Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values.
   */
  virtual void SetInputImage( const InputImageType * ptr );

  /**
   * Get the input image.
   */
  InputImageConstPointer GetInputImage() const
    { return m_Image.GetPointer(); }

  /**
   * Evaluate the function at specified Point position.
   * Subclasses should override this method.
   */
  virtual TOutput Evaluate( const PointType& point ) const = 0;

  /**
   * Evaluate the function at specified Index position.
   * Subclasses should override this method.
   */
  virtual TOutput EvaluateAtIndex( const IndexType & index ) const = 0;

  /**
   * Evaluate the function at specified ContinousIndex position.
   * Subclasses should override this method.
   */
  virtual TOutput EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const = 0;
    
  /**
   * Check if an index is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image is done.
   */
  inline bool IsInsideBuffer( const IndexType & index ) const;
            
  /**
   * Check if a continuous index is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image is done.
   */
  inline bool IsInsideBuffer( const ContinuousIndexType & index ) const;

  /**
   * Check if a point is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image pointer is done.
   */
  inline bool IsInsideBuffer( const PointType & point ) const;

  /**
   * Point/Index/ContinuousIndex conversion functions.
   * \warning For efficiency, no validity checking of the
   * input image pointer is done.
   */
  inline void ConvertPointToContinuousIndex(
    const PointType& point, ContinuousIndexType& index ) const;

  inline void ConvertContinuousIndexToPoint(
    const ContinuousIndexType& index, PointType& point ) const;

  inline void ConvertIndexToPoint(
    const IndexType& index, PointType& point ) const;

  inline void ConvertPointToNearestIndex(
    const PointType& point, IndexType& index ) const;

  inline void ConvertContinuousIndexToNearestIndex(
    const ContinuousIndexType &cindex, IndexType& index ) const;


protected:
  ImageFunction();
  ~ImageFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Const pointer to the input image.
   */
  InputImageConstPointer  m_Image;

  /**
   * Cache some image information
   */
  const double * m_Origin;
  const double * m_Spacing;
  PointType      m_GeometricStart;
  PointType      m_GeometricEnd;
  IndexType      m_BufferStart;
  IndexType      m_BufferEnd;

private:
  Self(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFunction.txx"
#endif

#endif
