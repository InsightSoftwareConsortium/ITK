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
#ifndef itkVectorCentralDifferenceImageFunction_h
#define itkVectorCentralDifferenceImageFunction_h

#include "itkImageFunction.h"
#include "itkMatrix.h"

namespace itk
{

/**
 * \class VectorCentralDifferenceImageFunction
 * \brief Calculate the derivative by central differencing.
 *
 * This class is templated over the input image type and
 * the coordinate representation type (e.g. float or double).
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \ingroup ImageFunctions
 * \ingroup ITKReview
 */
template<
  typename TInputImage,
  typename TCoordRep = float >
class ITK_TEMPLATE_EXPORT VectorCentralDifferenceImageFunction:
  public ImageFunction< TInputImage,
                        Matrix< double,
                                TInputImage::PixelType::Dimension,
                                TInputImage::ImageDimension >,
                        TCoordRep >
{
public:
  typedef typename TInputImage::PixelType InputPixelType;

  /** Extract the vector dimension from the pixel template parameter. */
  itkStaticConstMacro(Dimension, unsigned int,
                      InputPixelType::Dimension);

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef VectorCentralDifferenceImageFunction Self;
  typedef ImageFunction< TInputImage,
                         Matrix< double, itkGetStaticConstMacro(Dimension),
                                 itkGetStaticConstMacro(ImageDimension) >,
                         TCoordRep >       Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorCentralDifferenceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** OutputType typdef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Evalulate the image derivative by central differencing at specified index.
   *
   *  No bounds checking is done.
   *  The point is assume to lie within the image buffer.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   *  calling the method. */
  virtual OutputType EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE;

  /** Evalulate the image derivative by central differencing at non-integer
   *  positions.
   *
   *  No bounds checking is done.
   *  The point is assumed to lie within the image buffer.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   *  calling the method. */
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex(index);
  }

  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex) const ITK_OVERRIDE
  {
    IndexType index;

    this->ConvertContinuousIndexToNearestIndex(cindex, index);
    return this->EvaluateAtIndex(index);
  }

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not. The flag ON will take into
   * account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

protected:
  VectorCentralDifferenceImageFunction();
  ~VectorCentralDifferenceImageFunction(){}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorCentralDifferenceImageFunction);

  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorCentralDifferenceImageFunction.hxx"
#endif

#endif
