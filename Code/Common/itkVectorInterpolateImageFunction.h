/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVectorInterpolateImageFunction_h
#define _itkVectorInterpolateImageFunction_h

#include "itkImageFunction.h"
#include "itkVector.h"

namespace itk
{

/**
 * Due to a bug in MSVC, an enum value cannot be accessed out of a template
 * parameter until the template class opens.  In order for templated classes
 * to access the dimension of a template parameter in defining their
 * own dimension, this class is needed as a work-around.
 */
template <typename T>
struct GetVectorDimension
{
  itkStaticConstMacro(VectorDimension, int, T::VectorDimension);
}; 

  
/** \class VectorInterpolateImageFunction
 * \brief Base class for all vector image interpolaters.
 *
 * VectorInterpolateImageFunction is the base for all ImageFunctions that
 * interpolates image with vector pixel types. This function outputs
 * a return value of type Vector<double,VectorDimension>.
 *
 * This class is templated input image type and the coordinate
 * representation type.
 *
 * \warning This heirarchy of functions work only for images 
 * with Vector-based pixel types. For scalar images use 
 * InterpolateImageFunction.
 * 
 * \sa InterpolateImageFunction
 * \ingroup ImageFunctions
 */
template <
class TInputImage,
class TCoordRep = float,
class TPixelType = typename TInputImage::PixelType
>
class ITK_EXPORT VectorInterpolateImageFunction : 
  public ImageFunction<
    TInputImage, 
    Vector< ITK_TYPENAME NumericTraits<typename TPixelType::ValueType>::RealType, 
      ::itk::GetVectorDimension<TPixelType>::VectorDimension>,
    TCoordRep > 
{
public:
  /** Extract the vector dimension from the pixel template parameter. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      TPixelType::VectorDimension);
  
  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef VectorInterpolateImageFunction Self;
  typedef ImageFunction<TInputImage,
    Vector<double, itkGetStaticConstMacro(VectorDimension)>, TCoordRep > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorInterpolateImageFunction, ImageFunction);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename InputImageType::PixelType  PixelType;
  typedef typename PixelType::ValueType       ValueType;
  typedef typename NumericTraits<ValueType>::RealType  RealType;
    

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Output type is Vector<double,VectorDimension>. */
  typedef typename Superclass::OutputType OutputType;

  /** CoordRep typedef support. */
  typedef TCoordRep CoordRepType;

  /** Returns the interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType Evaluate( const PointType& point ) const
    {
    ContinuousIndexType index;
    this->ConvertPointToContinuousIndex( point, index );
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
   * Simply returns the image value at the
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtIndex( const IndexType & index ) const
    {
    OutputType output;
    PixelType input = m_Image->GetPixel( index );
    for( unsigned int k = 0; k < VectorDimension; k++ )
      {
      output[k] = static_cast<double>( input[k] );
      }
    return ( output );
    }

protected:
  VectorInterpolateImageFunction() {}
  ~VectorInterpolateImageFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf( os, indent ); }

private:
  VectorInterpolateImageFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // namespace itk

#endif


