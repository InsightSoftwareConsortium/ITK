/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageFunction_h
#define _itkImageFunction_h

#include "itkFunctionBase.h"
#include "itkPoint.h"
#include "itkIndex.h"
#include "itkContinuousIndex.h"
#include "itkImageBase.h"

namespace itk
{


/** \class ImageFunction
 * \brief Evaluates a function of an image at specified position.
 *
 * ImageFunction is a baseclass for all objects that evaluates
 * a function of an image at index, continuous index or point.
 * This class is templated over the input image type, the type 
 * of the function output and the coordinate representation type
 * (e.g. float or double).
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
 * \sa ContinuousIndex
 *
 * \ingroup ImageFunctions
 */
template <
class TInputImage, 
class TOutput,
class TCoordRep = float 
>
class ITK_EXPORT ImageFunction : 
    public FunctionBase< Point<TCoordRep,
                               ::itk::GetImageDimension<TInputImage>::ImageDimension>, 
                       TOutput > 
{
public:
  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef ImageFunction Self;
  typedef FunctionBase< Point<TCoordRep,
                              itkGetStaticConstMacro(ImageDimension)>,
                        TOutput > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFunction, FunctionBase);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** InputPixel typedef support */
  typedef typename InputImageType::PixelType InputPixelType;

  /** InputImagePointer typedef support */ 
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  /** OutputType typedef support. */
  typedef TOutput OutputType;

  /** CoordRepType typedef support. */
  typedef TCoordRep CoordRepType;

  /** Index Type. */
  typedef typename InputImageType::IndexType IndexType;

  /** ContinuousIndex Type. */
  typedef ContinuousIndex<TCoordRep,itkGetStaticConstMacro(ImageDimension)>
          ContinuousIndexType;

  /** Point Type. */
  typedef Point<TCoordRep,itkGetStaticConstMacro(ImageDimension)> PointType;

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  virtual void SetInputImage( const InputImageType * ptr );

  /** Get the input image. */
  const InputImageType * GetInputImage() const
    { return m_Image.GetPointer(); }

  /** Evaluate the function at specified Point position.
   * Subclasses must provide this method. */
  virtual TOutput Evaluate( const PointType& point ) const = 0;

  /** Evaluate the function at specified Index position.
   * Subclasses must provide this method. */
  virtual TOutput EvaluateAtIndex( const IndexType & index ) const = 0;

  /** Evaluate the function at specified ContinousIndex position.
   * Subclasses must provide this method. */
  virtual TOutput EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const = 0;
    
  /** Check if an index is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image is done. */
  inline bool IsInsideBuffer( const IndexType & index ) const
    { 
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        if ( index[j] < m_StartIndex[j] ) { return false; };
        if ( index[j] > m_EndIndex[j] ) { return false; };
        }
      return true;
    }
            
  /** Check if a continuous index is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image is done. */
  inline bool IsInsideBuffer( const ContinuousIndexType & index ) const
    { 
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        if ( index[j] < m_StartContinuousIndex[j] ) { return false; };
        if ( index[j] > m_EndContinuousIndex[j] ) { return false; };
        }
      return true;
    }

  /** Check if a point is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image pointer is done. */
  inline bool IsInsideBuffer( const PointType & point ) const
    { 
    ContinuousIndexType index;
    m_Image->TransformPhysicalPointToContinuousIndex( point, index );
    return this->IsInsideBuffer( index );
    }

  /** Convert point to nearest index. */
  void ConvertPointToNearestIndex( const PointType & point,
    IndexType & index ) const
    {
    ContinuousIndexType cindex;
    m_Image->TransformPhysicalPointToContinuousIndex( point, cindex );
    this->ConvertContinuousIndexToNearestIndex( cindex, index );
    }

  /** Convert continuous index to nearest index. */
  void ConvertContinuousIndexToNearestIndex( const ContinuousIndexType & cindex,
    IndexType & index ) const
    {
    typedef typename IndexType::IndexValueType ValueType;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      { index[j] = static_cast<ValueType>( vnl_math_rnd( cindex[j] ) ); }
    }
  
protected:
  ImageFunction();
  ~ImageFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Const pointer to the input image. */
  InputImageConstPointer  m_Image;

  /** Cache some values for testing if indices are inside buffered region. */
  IndexType               m_StartIndex;
  IndexType               m_EndIndex;
  ContinuousIndexType     m_StartContinuousIndex;
  ContinuousIndexType     m_EndContinuousIndex;

private:
  ImageFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFunction.txx"
#endif

#endif
