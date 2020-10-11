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
#ifndef itkParametricImageSource_h
#define itkParametricImageSource_h

#include "itkGenerateImageSource.h"

#include "itkArray.h"

namespace itk
{

/**
 *\class ParametricImageSource
 * \brief Base class for all parametric image sources.
 *
 * This abstract class specifies an interface than enables parameters
 * to be set through a vector of values using the
 * SetParameters() method. This interface makes it easier to include
 * parameterized image sources within ITK's
 * optimization/registration framework.
 *
 * Concrete subclasses must implement the methods SetParameters(),
 * GetParameters() and GetNumberOfParameters().
 *
 * \ingroup DataSources
 * \ingroup ITKImageSources
 */
template <typename TOutputImage>
class ITK_TEMPLATE_EXPORT ParametricImageSource : public GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ParametricImageSource);

  /** Standard class type aliases. */
  using Self = ParametricImageSource;
  using Superclass = GenerateImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = DataObject::Pointer;

  /** Some convenient type alias. */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using ParametersValueType = double;
  using ParametersType = Array<ParametersValueType>;

  /** ImageDimension constant */
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParametricImageSource, GenerateImageSource);

  /** Set the parameters for this source. Setting the parameters does
   * not mark the image source as modified; subclasses should override
   * this method to forward parameters through setters that call
   * Modified(). */
  virtual void
  SetParameters(const ParametersType & parameters) = 0;

  /** Get the parameters for this source. */
  virtual ParametersType
  GetParameters() const = 0;

  /** Get the number of parameters. */
  virtual unsigned int
  GetNumberOfParameters() const = 0;

protected:
  ParametricImageSource() = default;
  ~ParametricImageSource() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkParametricImageSource.hxx"
#endif

#endif
