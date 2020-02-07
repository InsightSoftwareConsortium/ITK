/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMinimumMaximumImageCalculator_h
#define itkMinimumMaximumImageCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class MinimumMaximumImageCalculator
 *  \brief Computes the minimum and the maximum intensity values of
 *         an image.
 *
 * This calculator computes the minimum and the maximum intensity values of
 * an image.  It is templated over input image type.  If only Maximum or
 * Minimum value is needed, just call ComputeMaximum() (ComputeMinimum())
 * otherwise Compute() will compute both.
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/FindMaxAndMinInImage,Find Max And Min In Image}
 * \sphinxexample{Developer/OilPaintingImageFilter,Multi-threaded oil painting image filter}
 * \endsphinx
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT MinimumMaximumImageCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinimumMaximumImageCalculator);

  /** Standard class type aliases. */
  using Self = MinimumMaximumImageCalculator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageCalculator, Object);

  /** Type definition for the input image. */
  using ImageType = TInputImage;

  /** Pointer type for the image. */
  using ImagePointer = typename TInputImage::Pointer;

  /** Const Pointer type for the image. */
  using ImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using PixelType = typename TInputImage::PixelType;

  /** Type definition for the input image index type. */
  using IndexType = typename TInputImage::IndexType;

  /** Type definition for the input image region type. */
  using RegionType = typename TInputImage::RegionType;

  /** Set the input image. */
  itkSetConstObjectMacro(Image, ImageType);

  /** Compute the minimum value of intensity of the input image. */
  void
  ComputeMinimum();

  /** Compute the maximum value of intensity of the input image. */
  void
  ComputeMaximum();

  /** Compute the minimum and maximum values of intensity of the input image. */
  void
  Compute();

  /** Return the minimum intensity value. */
  itkGetConstMacro(Minimum, PixelType);

  /** Return the maximum intensity value. */
  itkGetConstMacro(Maximum, PixelType);

  /** Return the index of the minimum intensity value. */
  itkGetConstReferenceMacro(IndexOfMinimum, IndexType);

  /** Return the index of the maximum intensity value. */
  itkGetConstReferenceMacro(IndexOfMaximum, IndexType);

  /** Set the region over which the values will be computed */
  void
  SetRegion(const RegionType & region);

protected:
  MinimumMaximumImageCalculator();
  ~MinimumMaximumImageCalculator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  PixelType         m_Minimum;
  PixelType         m_Maximum;
  ImageConstPointer m_Image;

  IndexType m_IndexOfMinimum;
  IndexType m_IndexOfMaximum;

  RegionType m_Region;
  bool       m_RegionSetByUser;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMinimumMaximumImageCalculator.hxx"
#endif

#endif /* itkMinimumMaximumImageCalculator_h */
