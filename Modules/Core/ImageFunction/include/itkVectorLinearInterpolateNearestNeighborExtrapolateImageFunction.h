/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction_h
#define itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction_h

#include "itkVectorInterpolateImageFunction.h"

namespace itk
{
/**
 * \class VectorLinearInterpolateNearestNeighborExtrapolateImageFunction
 * \brief Linearly interpolate or NN extrapolate a vector image at
 * specified positions.
 *
 * VectorLinearInterpolateNearestNeighborExtrapolateImageFunction
 * linearly interpolates (or NN extrapolates) a vector
 * image intensity non-integer pixel position. This class is templated
 * over the input image type and the coordinate representation type.
 *
 * This class is designed to work as a VectorInterpolateImageFunction,
 * hence the  IsInsideBuffer(PointType p) is overridden to always
 * answer true
 *
 * This function works for N-dimensional images.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * \warning This function work only for Vector images. For
 * scalar images use LinearInterpolateImageFunction.
 *
 *  This paper was contributed in the Insight Journal paper:
 *  https://hdl.handle.net/1926/510
 *
 * \ingroup ImageFunctions ImageInterpolators
 * \ingroup ITKImageFunction
 *
 */
template< typename TInputImage, typename TCoordRep = float >
class ITK_TEMPLATE_EXPORT VectorLinearInterpolateNearestNeighborExtrapolateImageFunction:
  public VectorInterpolateImageFunction< TInputImage, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef VectorLinearInterpolateNearestNeighborExtrapolateImageFunction Self;
  typedef VectorInterpolateImageFunction< TInputImage, TCoordRep >       Superclass;
  typedef SmartPointer< Self >                                           Pointer;
  typedef SmartPointer< const Self >                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorLinearInterpolateNearestNeighborExtrapolateImageFunction,
               VectorInterpolateImageFunction);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::PixelType      PixelType;
  typedef typename Superclass::ValueType      ValueType;
  typedef typename Superclass::RealType       RealType;

  typedef typename Superclass::PointType PointType;

  /** Grab the vector dimension from the superclass. */
  //itkStaticConstMacro(Dimension, unsigned int,
  //                    Superclass::Dimension);

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::IndexValueType IndexValueType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Output type is Vector<double,Dimension> */
  typedef typename Superclass::OutputType OutputType;

  /** Should check if an index is inside the image buffer, however we
   * require that it answers true to use the extrapolation possibility. */
  virtual bool IsInsideBuffer(const IndexType &) const ITK_OVERRIDE
  {
    return true;
  }

  /** Should check if a point is inside the image buffer, however we
   * require that it answers true to use the extrapolation possibility. */
  virtual bool IsInsideBuffer(const PointType &) const ITK_OVERRIDE
  {
    return true;
  }

  /** Should check if a continuous index is inside the image buffer, however we
   * require that it answers true to use the extrapolation possibility. */
  virtual bool IsInsideBuffer(const ContinuousIndexType &) const ITK_OVERRIDE
  {
    return true;
  }

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the linearly interpolated image intensity at a
   * specified point position. If the point does not lie within the
   * image buffer a nearest neighbor interpolation is done. */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const ITK_OVERRIDE;

  /** Evaluate the function at an index position
   *
   * Simply returns the image value at the
   * specified index position. If the index does not lie within the
   * image buffer a nearest neighbor interpolation is done. */
  virtual OutputType EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE;

protected:
  VectorLinearInterpolateNearestNeighborExtrapolateImageFunction();
  virtual ~VectorLinearInterpolateNearestNeighborExtrapolateImageFunction() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  VectorLinearInterpolateNearestNeighborExtrapolateImageFunction(const Self &); //purposely
                                                                                // not
                                                                                // implemented
  void operator=(const Self &);                                                 //purposely
                                                                                // not
                                                                                // implemented

  /** Number of neighbors used in the interpolation */
  static const unsigned int m_Neighbors;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.hxx"
#endif

#endif
