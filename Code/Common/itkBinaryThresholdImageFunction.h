/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryThresholdImageFunction_h
#define _itkBinaryThresholdImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/** \class BinaryThresholdImageFunction
 * \brief Returns true is the value of an image lies within a range of thresholds
This ImageFunction returns true (or false) if the pixel value lies
within (outside) a lower and upper threshold value. The threshold
range can be set with the ThresholdBelow, ThresholdBetween or
ThresholdAbove methods.  The input image is set via method
SetInputImage().

Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex
respectively evaluate the function at an geometric point, image index
and continuous image index.

 * \ingroup ImageFunctions
 * 
 * */
template <class TInputImage>
class ITK_EXPORT BinaryThresholdImageFunction : 
  public ImageFunction<TInputImage,bool> 
{
public:
  /** Standard class typedefs. */
  typedef BinaryThresholdImageFunction Self;
  typedef ImageFunction<TInputImage,bool> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  
  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType PixelType;

  /** Dimension underlying input image. */
  enum { ImageDimension = Superclass::ImageDimension };

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** BinaryThreshold the image at a point position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */

  virtual bool Evaluate( const PointType& point ) const
    {
    IndexType index;
    this->ConvertPointToNearestIndex( point, index );
    return ( this->EvaluateAtIndex( index ) );
    }

  /** BinaryThreshold the image at a continuous index position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const
    {
      IndexType nindex;

      this->ConvertContinuousIndexToNearestIndex (index, nindex);
      return this->EvaluateAtIndex(nindex);
    }

  /** BinaryThreshold the image at an index position.
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool EvaluateAtIndex( const IndexType & index ) const
    {
    PixelType value = m_Image->GetPixel(index);
    return ( m_Lower <= value && value <= m_Upper);
    }

  /** Get the lower threshold value. */
  itkGetMacro(Lower,PixelType);

  /** Get the upper threshold value. */
  itkGetMacro(Upper,PixelType);

  /** Values greater than or equal to the value are inside. */
  void ThresholdAbove(PixelType thresh);
  
  /** Values less than or equal to the value are inside. */
  void ThresholdBelow(PixelType thresh);

  /** Values that lie between lower and upper inclusive are inside. */
  void ThresholdBetween(PixelType lower, PixelType upper);

protected:
  BinaryThresholdImageFunction();
  ~BinaryThresholdImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BinaryThresholdImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  PixelType m_Lower;
  PixelType m_Upper;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryThresholdImageFunction.txx"
#endif

#endif
