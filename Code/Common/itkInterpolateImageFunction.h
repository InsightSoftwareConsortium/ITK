/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkInterpolateImageFunction_h
#define _itkInterpolateImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/** \class InterpolateImageFunction
 * \brief Base class for all image interpolaters.
 *
 * InterpolateImageFunction is the base for all ImageFunctions that
 * interpolates image intensity at a non-integer pixel position. 
 * This class is templated over the input image type and the 
 * coordinate representation type (e.g. float or double ).
 *
 * \warning This heirarchy of functions work only for images 
 * with scalar pixel types. For images of vector pixel types
 * use VectorInterpolateImageFunctions. 
 *
 * \sa VectorInterpolateImageFunction
 * \ingroup ImageFunctions
 * 
 * */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT InterpolateImageFunction : 
  public ImageFunction< TInputImage, 
    ITK_TYPENAME NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep > 
{
public:
  /** Standard class typedefs. */
  typedef InterpolateImageFunction Self;
  typedef ImageFunction<TInputImage,double,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(InterpolateImageFunction, ImageFunction);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  
  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** RealType typedef support. */
  typedef typename NumericTraits<typename TInputImage::PixelType>::RealType RealType;

  /** Interpolate the image at a point position
   *
   * Returns the interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType Evaluate( const PointType& point ) const
    {
    ContinuousIndexType index;
    m_Image->TransformPhysicalPointToContinuousIndex( point, index );
    return ( this->EvaluateAtContinuousIndex( index ) );
    }

  /** Interpolate the image at a continuous index position
   *
   * Returns the interpolated image intensity at a 
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * Subclasses must override this method.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const = 0;

  /** Interpolate the image at an index position.
   *
   * Simply returns the image value at the
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtIndex( const IndexType & index ) const
    {
    return ( static_cast<RealType>( m_Image->GetPixel( index ) ) );
    }

protected:
  InterpolateImageFunction(){};
  ~InterpolateImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf( os, indent ); }

private:
  InterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // namespace itk

#endif
