/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtrapolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkExtrapolateImageFunction_h
#define _itkExtrapolateImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/** \class ExtrapolateImageFunction
 * \brief Base class for all image extrapolaters.
 *
 * ExtrapolateImageFunction is the base for all ImageFunctions that
 * extrapolates image intensity at a non-integer pixel position
 * outside the image buffer.
 * This class is templated over the input image type and the 
 * coordinate representation type (e.g. float or double ).
 *
 * \warning This heirarchy of functions work only for images 
 * with scalar pixel types.
 *
 * \ingroup ImageFunctions
 * 
 * */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT ExtrapolateImageFunction : 
  public ImageFunction< TInputImage, 
    ITK_TYPENAME NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep > 
{
public:
  /** Standard class typedefs. */
  typedef ExtrapolateImageFunction Self;
  typedef ImageFunction<TInputImage,double,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtrapolateImageFunction, ImageFunction);

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

  /** Extrapolate the image at a point position
   *
   * Returns the extrapolated image intensity at a 
   * specified point position.
   */
  virtual OutputType Evaluate( const PointType& point ) const
    {
    ContinuousIndexType index;
    m_Image->TransformPhysicalPointToContinuousIndex( point, index );
    return ( this->EvaluateAtContinuousIndex( index ) );
    }

  /** Extrapolate the image at a continuous index position
   *
   * Returns the extrapolated image intensity at a 
   * specified point position.
   */
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const = 0;

  /** Extrapolate the image at an index position.
   *
   * Returns the extrapolated image intensity at a 
   * specified point position.
   */
  virtual OutputType EvaluateAtIndex( 
    const IndexType & index ) const = 0;

protected:
  ExtrapolateImageFunction(){};
  ~ExtrapolateImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf( os, indent ); }

private:
  ExtrapolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // namespace itk

#endif
