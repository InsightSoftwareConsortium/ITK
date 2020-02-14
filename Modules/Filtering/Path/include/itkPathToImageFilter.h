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
#ifndef itkPathToImageFilter_h
#define itkPathToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class PathToImageFilter
 * \brief Base class for filters that take a Path as input and produce an image as output.
 * Base class for filters that take a Path as input and produce an image as
 * output. By default, if the user does not specify the size of the output
 * image, the maximum size of the path's bounding box is used.  The default
 * spacing of the image is given by the spacing of the input  path (currently
 * assumed internally to be 1.0).
 * \ingroup ITKPath
 */
template <typename TInputPath, typename TOutputImage>
class ITK_TEMPLATE_EXPORT PathToImageFilter : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PathToImageFilter);

  /** Standard class type aliases. */
  using Self = PathToImageFilter;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathToImageFilter, ImageSource);

  /** Some convenient type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using InputPathType = TInputPath;
  using InputPathPointer = typename InputPathType::Pointer;
  using InputPathConstPointer = typename InputPathType::ConstPointer;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using SizeType = typename OutputImageType::SizeType;
  using ValueType = typename OutputImageType::ValueType;

  /** ImageDimension constants */
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Set/Get the path input of this process object.  */
  using Superclass::SetInput;
  virtual void
  SetInput(const InputPathType * path);

  virtual void
  SetInput(unsigned int, const TInputPath * path);

  const InputPathType *
  GetInput();

  const InputPathType *
  GetInput(unsigned int idx);

  /** Spacing (size of a pixel) of the output image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  virtual void
  SetSpacing(const double * spacing);

  virtual void
  SetSpacing(const float * spacing);

  virtual const double *
  GetSpacing() const;

  /** Set/Get the value for pixels on and off the path.
   * By default, this filter will return a "0" image with path pixels set to 1 */
  itkSetMacro(PathValue, ValueType);
  itkGetConstMacro(PathValue, ValueType);
  itkSetMacro(BackgroundValue, ValueType);
  itkGetConstMacro(BackgroundValue, ValueType);

  /** The origin of the output image. The origin is the geometric
   * coordinates of the index (0,0,...,0).  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  virtual void
  SetOrigin(const double * origin);

  virtual void
  SetOrigin(const float * origin);

  virtual const double *
  GetOrigin() const;

  /** Set/Get Size */
  itkSetMacro(Size, SizeType);
  itkGetConstMacro(Size, SizeType);

protected:
  PathToImageFilter();
  ~PathToImageFilter() override = default;

  void
  GenerateOutputInformation() override
  {} // do nothing
  void
  GenerateData() override;

  SizeType  m_Size;
  double    m_Spacing[OutputImageDimension];
  double    m_Origin[OutputImageDimension];
  ValueType m_PathValue;
  ValueType m_BackgroundValue;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPathToImageFilter.hxx"
#endif

#endif
