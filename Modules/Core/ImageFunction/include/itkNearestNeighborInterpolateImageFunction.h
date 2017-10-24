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
#ifndef itkNearestNeighborInterpolateImageFunction_h
#define itkNearestNeighborInterpolateImageFunction_h

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
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TCoordRep = double >
class ITK_TEMPLATE_EXPORT NearestNeighborInterpolateImageFunction:
  public InterpolateImageFunction< TInputImage, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef NearestNeighborInterpolateImageFunction            Self;
  typedef InterpolateImageFunction< TInputImage, TCoordRep > Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;

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
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

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
    const ContinuousIndexType & index) const ITK_OVERRIDE
  {
    IndexType nindex;

    this->ConvertContinuousIndexToNearestIndex(index, nindex);
    return static_cast< OutputType >( this->GetInputImage()->GetPixel(nindex) );
  }

protected:
  NearestNeighborInterpolateImageFunction(){}
  ~NearestNeighborInterpolateImageFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NearestNeighborInterpolateImageFunction);
};
} // end namespace itk

#endif
