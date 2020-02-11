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
#ifndef itkExtensionVelocitiesImageFilter_h
#define itkExtensionVelocitiesImageFilter_h

#include "itkLevelSetVelocityNeighborhoodExtractor.h"
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkReinitializeLevelSetImageFilter.h"

namespace itk
{
/** \class ExtensionVelocitiesImageFilter
 *  \brief Extend velocities smoothly from a particular level set.
 *
 * ExtensionVelocitiesImageFilter extends velocities smoothly from a particular
 * level set.
 *
 * This class is templated over the image type which represents
 * the level set, the type of the velocity and the
 * number of velocities to be extended.
 *
 * This class supports narrowbanding. If the input narrowband is provided,
 * the algorithm will only locate the level set within the input narrowband.
 * For the output, the extended velocity is only valid for a distance
 * of OutputNarrowBandwidth / 2 of either side of the level set of interest.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation
 * \ingroup ITKLevelSets
 */
template <typename TLevelSet, typename TAuxValue = float, unsigned int VAuxDimension = 1>
class ITK_TEMPLATE_EXPORT ExtensionVelocitiesImageFilter : public ReinitializeLevelSetImageFilter<TLevelSet>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExtensionVelocitiesImageFilter);

  /** Standard class type aliases. */
  using Self = ExtensionVelocitiesImageFilter;
  using Superclass = ReinitializeLevelSetImageFilter<TLevelSet>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtensionVelocitiesImageFilter, ReinitializeLevelSetImageFilter);

  /** The type of level set and the pointer type. */
  using LevelSetType = LevelSetTypeDefault<TLevelSet>;
  using LevelSetPointer = typename LevelSetType::LevelSetPointer;
  using LevelSetConstPointer = typename LevelSetType::LevelSetConstPointer;
  using PixelType = typename LevelSetType::PixelType;
  using NodeType = typename LevelSetType::NodeType;
  using NodeContainer = typename LevelSetType::NodeContainer;
  using NodeContainerPointer = typename LevelSetType::NodeContainerPointer;

  /** The dimension of the level set. */
  static constexpr unsigned int SetDimension = LevelSetType::SetDimension;

  /** AuxVarType type alias support */
  using AuxVarType = AuxVarTypeDefault<TAuxValue, VAuxDimension, Self::SetDimension>;
  using AuxValueType = typename AuxVarType::AuxValueType;
  using AuxValueVectorType = typename AuxVarType::AuxValueVectorType;
  using AuxValueContainer = typename AuxVarType::AuxValueContainer;
  using AuxImageType = typename AuxVarType::AuxImageType;
  using AuxImagePointer = typename AuxVarType::AuxImagePointer;
  using AuxImageConstPointer = typename AuxVarType::AuxImageConstPointer;

  /** Number of velocity images to be extended. */
  static constexpr unsigned int AuxDimension = VAuxDimension;

  /** Set/Get one of the input velocity images to be extended. */
  void
  SetInputVelocityImage(const AuxImageType * ptr, unsigned int idx = 0);

  const AuxImageType *
  GetInputVelocityImage(unsigned int idx = 0);

  /** Get one of the extended velocity images. */
  AuxImageType *
  GetOutputVelocityImage(unsigned int idx = 0);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(AuxValueHasNumericTraitsCheck, (Concept::HasNumericTraits<TAuxValue>));
  itkConceptMacro(LevelSetOStreamWritableCheck, (Concept::OStreamWritable<PixelType>));
  // End concept checking
#endif

protected:
  ExtensionVelocitiesImageFilter();
  ~ExtensionVelocitiesImageFilter() override = default;

  void
  GenerateDataFull() override;

  void
  GenerateDataNarrowBand() override;

  void
  AllocateOutput() override;

  void
  EnlargeOutputRequestedRegion(DataObject *) override;

private:
  /** Internal type alias. */
  using SpeedImageType = Image<float, Self::SetDimension>;

  using LocatorType = LevelSetVelocityNeighborhoodExtractor<TLevelSet, TAuxValue, VAuxDimension>;
  using FastMarchingImageFilterType =
    FastMarchingExtensionImageFilter<TLevelSet, TAuxValue, VAuxDimension, SpeedImageType>;

  typename LocatorType::Pointer m_Locator;

  typename FastMarchingImageFilterType::Pointer m_Marcher;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkExtensionVelocitiesImageFilter.hxx"
#endif

#endif
