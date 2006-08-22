/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLinearInterpolateImageFunction_h
#define __itkLinearInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"

namespace itk
{

/** \class LinearInterpolateImageFunction
 * \brief Linearly interpolate an image at specified positions.
 *
 * LinearInterpolateImageFunction linearly interpolates image intensity at
 * a non-integer pixel position. This class is templated
 * over the input image type and the coordinate representation type 
 * (e.g. float or double).
 *
 * This function works for N-dimensional images.
 *
 * \warning This function work only for images with scalar pixel
 * types. For vector images use VectorLinearInterpolateImageFunction.
 *
 * \sa VectorLinearInterpolateImageFunction
 *
 * \ingroup ImageFunctions ImageInterpolators 
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT LinearInterpolateImageFunction : 
  public InterpolateImageFunction<TInputImage,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef LinearInterpolateImageFunction Self;
  typedef InterpolateImageFunction<TInputImage,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(LinearInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;
  typedef typename Superclass::PointType           PointType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the linearly interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const;

  /** Check if a continuous index is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image is done. */
  virtual bool IsInsideBuffer( const ContinuousIndexType & index ) const
    { 
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      if ( (index[j]+0.5) < this->m_StartContinuousIndex[j]) { return false; };
      if ( (index[j]-0.5) > this->m_EndContinuousIndex[j] ) { return false; };
      }
    return true;
    }
  virtual bool IsInsideBuffer( const PointType & point ) const
    { 
    // This function needed to be overrided cause gcc gets confused if 
    // the function defintion is not present in this subclass
    ContinuousIndexType index;
    this->m_Image->TransformPhysicalPointToContinuousIndex( point, index );
    return this->IsInsideBuffer( index );
    }
  

protected:
  LinearInterpolateImageFunction();
  ~LinearInterpolateImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  LinearInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /** Number of neighbors used in the interpolation */
  static const unsigned long  m_Neighbors;  

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearInterpolateImageFunction.txx"
#endif

#endif
