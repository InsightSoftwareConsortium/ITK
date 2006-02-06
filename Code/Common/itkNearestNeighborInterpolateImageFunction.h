/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNearestNeighborInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNearestNeighborInterpolateImageFunction_h
#define __itkNearestNeighborInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"

namespace itk
{

/** \class NearestNeighborInterpolateImageFunction
 * \brief Nearest neighbor interpolation of a scalar image.
 *
 * NearestNeighborInterpolateImageFunction interpolates image intensity at
 * a non-integer pixel position by copying the intensity for the nearest
 * neighbor. This class is templated
 * over the input image type and the coordinate representation type 
 * (e.g. float or double).
 *
 * \ingroup ImageFunctions ImageInterpolators
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT NearestNeighborInterpolateImageFunction : 
  public InterpolateImageFunction<TInputImage,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef NearestNeighborInterpolateImageFunction Self;
  typedef InterpolateImageFunction<TInputImage,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NearestNeighborInterpolateImageFunction, 
    InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

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
  NearestNeighborInterpolateImageFunction(){};
  ~NearestNeighborInterpolateImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const
   { Superclass::PrintSelf( os, indent ); }

private:
  NearestNeighborInterpolateImageFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
