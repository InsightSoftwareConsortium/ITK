/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNearestNeighborInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNearestNeighborInterpolateImageFunction_h
#define _itkNearestNeighborInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"

namespace itk
{

/** \class NearestNeighborInterpolateImageFunction
 * \brief Nearest neighbor interpolation of a scalar image.
 *
 * \warning This function work only for images with scalar pixel
 * types.
 *
 * \ingroup ImageFunctions
 */
template <class TInputImage>
class ITK_EXPORT NearestNeighborInterpolateImageFunction : 
  public InterpolateImageFunction<TInputImage> 
{
public:
  /** Standard class typedefs. */
  typedef NearestNeighborInterpolateImageFunction Self;
  typedef InterpolateImageFunction<TInputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NearestNeighborInterpolateImageFunction, 
    InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  enum { ImageDimension = Superclass::ImageDimension };

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the linearly interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual double EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const
  {
   IndexType nindex;
   this->ConvertContinuousIndexToNearestIndex(index, nindex);
   return static_cast<double>( m_Image->GetPixel( nindex ) );
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

} // namespace itk

#endif
