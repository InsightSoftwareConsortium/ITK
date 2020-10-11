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

#ifndef itkLevelSetImage_h
#define itkLevelSetImage_h

#include "itkLevelSetBase.h"
#include "itkObjectFactory.h"
#include "itkIndex.h"
#include "itkImageBase.h"

namespace itk
{
/**
 *  \class LevelSetImage
 *  \brief Abstract class for a level-set function on one Image.
 *
 *  \tparam TOutput OutputType of the level-set function value
 *  \tparam VDimension Dimension of the underlying Image.
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, unsigned int VDimension, typename TOutput>
class ITK_TEMPLATE_EXPORT LevelSetImage : public LevelSetBase<TInput, VDimension, TOutput, ImageBase<VDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetImage);

  using ImageBaseType = ImageBase<VDimension>;

  using Self = LevelSetImage;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetBase<TInput, VDimension, TOutput, ImageBaseType>;

  /** Run-time type information */
  itkTypeMacro(LevelSetImage, LevelSetBase);

  static constexpr unsigned int Dimension = Superclass::Dimension;

  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;
  using OutputRealType = typename Superclass::OutputRealType;
  using GradientType = typename Superclass::GradientType;
  using HessianType = typename Superclass::HessianType;
  using LevelSetDataType = typename Superclass::LevelSetDataType;
  using OffsetType = typename ImageBaseType::OffsetType;
  using OffsetValueType = typename ImageBaseType::OffsetValueType;

  /* Set/Get the domain offset from input domain */
  itkSetMacro(DomainOffset, OffsetType);
  itkGetConstMacro(DomainOffset, OffsetType);

protected:
  LevelSetImage();

  ~LevelSetImage() override = default;

  using ScalingType = GradientType;
  ScalingType m_NeighborhoodScales;
  OffsetType  m_DomainOffset;

  virtual bool
  IsInsideDomain(const InputType & iP) const = 0;

private:
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetImage.hxx"
#endif

#endif // itkLevelSetImage_h
