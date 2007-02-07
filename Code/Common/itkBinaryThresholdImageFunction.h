/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdImageFunction_h
#define __itkBinaryThresholdImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/** \class BinaryThresholdImageFunction
 * \brief Returns true is the value of an image lies within a range 
 *        of thresholds
 * This ImageFunction returns true (or false) if the pixel value lies
 * within (outside) a lower and upper threshold value. The threshold
 * range can be set with the ThresholdBelow, ThresholdBetween or
 * ThresholdAbove methods.  The input image is set via method
 * SetInputImage().
 *
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex
 * respectively evaluate the function at an geometric point, image index
 * and continuous image index.
 *
 * \ingroup ImageFunctions
 * 
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT BinaryThresholdImageFunction : 
  public ImageFunction<TInputImage,bool,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef BinaryThresholdImageFunction              Self;
  typedef ImageFunction<TInputImage,bool,TCoordRep> Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  
  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType PixelType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

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
    PixelType value = this->GetInputImage()->GetPixel(index);
    return ( m_Lower <= value && value <= m_Upper);
    }

  /** Get the lower threshold value. */
  itkGetConstReferenceMacro(Lower,PixelType);

  /** Get the upper threshold value. */
  itkGetConstReferenceMacro(Upper,PixelType);

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

} // end namespace itk


// Define instantiation macro for this template.
#define ITK_TEMPLATE_BinaryThresholdImageFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT BinaryThresholdImageFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef BinaryThresholdImageFunction< ITK_TEMPLATE_2 x > \
                               BinaryThresholdImageFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkBinaryThresholdImageFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkBinaryThresholdImageFunction.txx"
#endif

#endif
