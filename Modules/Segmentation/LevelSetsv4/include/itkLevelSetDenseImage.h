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

#ifndef itkLevelSetDenseImage_h
#define itkLevelSetDenseImage_h

#include "itkDiscreteLevelSetImage.h"

namespace itk
{
/**
 *  \class LevelSetDenseImage
 *  \brief Base class for the "dense" representation of a level-set function on
 *  one image.
 *
 *  This representation is a "dense" level-set function, i.e. it defines
 *  a level-set function on a grid (more precisely the underlying structure
 *  is an Image).
 *
 *  \tparam TImage Input image type of the level set function
 *  \todo Think about using image iterators instead of GetPixel()
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT LevelSetDenseImage
  : public DiscreteLevelSetImage<typename TImage::PixelType, TImage::ImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetDenseImage);

  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using IndexType = typename ImageType::IndexType;
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;

  using Self = LevelSetDenseImage;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = DiscreteLevelSetImage<PixelType, ImageType::ImageDimension>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(LevelSetDenseImage, DiscreteLevelSetImage);

  static constexpr unsigned int Dimension = Superclass::Dimension;

  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;
  using OutputRealType = typename Superclass::OutputRealType;
  using GradientType = typename Superclass::GradientType;
  using HessianType = typename Superclass::HessianType;
  using LevelSetDataType = typename Superclass::LevelSetDataType;

  virtual void
  SetImage(ImageType * inputImage);
  itkGetModifiableObjectMacro(Image, ImageType);

  /** Returns the value of the level set function at a given location inputIndex */
  OutputType
  Evaluate(const InputType & inputIndex) const override;
  void
  Evaluate(const InputType & inputIndex, LevelSetDataType & data) const override;

protected:
  LevelSetDenseImage() = default;

  ~LevelSetDenseImage() override = default;

  ImagePointer m_Image;

  bool
  IsInsideDomain(const InputType & inputIndex) const override;

  /** Initial the level set pointer */
  void
  Initialize() override;

  /** Copy level set information from data object */
  void
  CopyInformation(const DataObject * data) override;

  /** Graft data object as level set object */
  void
  Graft(const DataObject * data) override;

private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetDenseImage.hxx"
#endif

#endif // itkLevelSetDenseImage_h
