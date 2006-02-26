/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNearestNeighborInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorNearestNeighborInterpolateImageFunction_h
#define __itkVectorNearestNeighborInterpolateImageFunction_h

#include "itkVectorInterpolateImageFunction.h"

namespace itk
{

/** 
 * \class VectorNearestNeighborInterpolateImageFunction
 * \brief Nearest neighbor interpolate a vector image at specified positions.
 *
 * VectorNearestNeighborInterpolateImageFunction interpolates vector
 * image intensity non-integer pixel position using nearest neighbor interpolation. 
 * This class is templated over the input image type and the coordinate 
 * representation type.
 *
 * This function works for N-dimensional images.
 *
 * \warning This function works only for Vector images.
 *
 * \ingroup ImageFunctions ImageInterpolators
 * 
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT VectorNearestNeighborInterpolateImageFunction : 
  public VectorInterpolateImageFunction<TInputImage,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef VectorNearestNeighborInterpolateImageFunction Self;
  typedef VectorInterpolateImageFunction<TInputImage,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorNearestNeighborInterpolateImageFunction, 
    VectorInterpolateImageFunction);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::PixelType      PixelType;
  typedef typename Superclass::ValueType      ValueType;
  typedef typename Superclass::RealType       RealType;
    
  /** Grab the vector dimension from the superclass. */
  itkStaticConstMacro(Dimension, unsigned int,
                       Superclass::Dimension);

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Output type is Vector<double,Dimension> */
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const
  {
   IndexType nindex;
   this->ConvertContinuousIndexToNearestIndex(index, nindex);
   return static_cast<OutputType>( this->GetInputImage()->GetPixel( nindex ) );
  }

protected:
  VectorNearestNeighborInterpolateImageFunction(){};
  ~VectorNearestNeighborInterpolateImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const
   { Superclass::PrintSelf( os, indent ); }

private:
  VectorNearestNeighborInterpolateImageFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#endif
