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
#ifndef itkFastMarchingExtensionImageFilter_h
#define itkFastMarchingExtensionImageFilter_h

#include "itkFastMarchingImageFilter.h"

namespace itk
{
/**
 *\class FastMarchingExtensionImageFilter
 * \brief Extend auxiliary variables smoothly using Fast Marching.
 *
 * Fast marching can be used to extend auxiliary variables smoothly
 * from the zero level set. Starting from an initial position on the
 * front, this class simultaneously calculate the signed distance and
 * extend a set of auxiliary values.
 *
 * This class is templated over the level set image type, the auxiliary
 * variable type and the number of auxiliary variables to extended. The initial
 * front is specified by two containers: one containing the known points
 * and one containing the trial points. The auxiliary variables on the front
 * are represented by two auxiliary variable containers: one containing
 * the value of the variables at the know points and on containing the
 * value of the variables at the trail points.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * For an alternative implementation, see itk::FastMarchingExtensionImageFilterBase.
 *
 * \sa FastMarchingExtensionImageFilterBase
 * \sa FastMarchingImageFilter
 * \sa LevelSetTypeDefault
 * \sa AuxVarTypeDefault
 * \ingroup LevelSetSegmentation
 * \ingroup ITKFastMarching
 */
template <typename TLevelSet,
          typename TAuxValue,
          unsigned int VAuxDimension = 1,
          typename TSpeedImage = Image<float, TLevelSet ::ImageDimension>>
class ITK_TEMPLATE_EXPORT FastMarchingExtensionImageFilter : public FastMarchingImageFilter<TLevelSet, TSpeedImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingExtensionImageFilter);

  /** Standard class typdedefs. */
  using Self = FastMarchingExtensionImageFilter;
  using Superclass = FastMarchingImageFilter<TLevelSet, TSpeedImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingExtensionImageFilter, FastMarchingImageFilter);

  /** Inherited type alias. */
  using LevelSetType = typename Superclass::LevelSetType;
  using SpeedImageType = typename Superclass::SpeedImageType;
  using LevelSetImageType = typename Superclass::LevelSetImageType;

  /** The dimension of the level set. */
  static constexpr unsigned int SetDimension = Superclass::SetDimension;

  /** Number of auxiliary variables to be extended. */
  static constexpr unsigned int AuxDimension = VAuxDimension;

  /** AuxVarType type alias support */
  using AuxVarType = AuxVarTypeDefault<TAuxValue, Self::AuxDimension, Self::SetDimension>;
  using AuxValueType = typename AuxVarType::AuxValueType;
  using AuxValueVectorType = typename AuxVarType::AuxValueVectorType;
  using AuxValueContainer = typename AuxVarType::AuxValueContainer;
  using AuxImageType = typename AuxVarType::AuxImageType;
  using AuxImagePointer = typename AuxVarType::AuxImagePointer;

  /** Index type alias support */
  using IndexType = Index<Self::SetDimension>;

  /** Get one of the extended auxiliary variable image. */
  AuxImageType *
  GetAuxiliaryImage(unsigned int idx);

  /** Set the container auxiliary values at the initial alive points. */
  void
  SetAuxiliaryAliveValues(AuxValueContainer * values)
  {
    m_AuxAliveValues = values;
  }

  /** Get the container of auxiliary values at the initial alive points. */
  AuxValueContainer *
  GetAuxiliaryAliveValues()
  {
    return m_AuxAliveValues.GetPointer();
  }

  /** Set the container of auxiliary values at the initial trial points. */
  void
  SetAuxiliaryTrialValues(AuxValueContainer * values)
  {
    m_AuxTrialValues = values;
  }

  /** Get the container of auxiliary values at the initial trial points. */
  typename AuxValueContainer::Pointer
  GetAuxiliaryTrialValues()
  {
    return m_AuxTrialValues;
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(AuxValueHasNumericTraitsCheck, (Concept::HasNumericTraits<TAuxValue>));
  // End concept checking
#endif

protected:
  FastMarchingExtensionImageFilter();
  ~FastMarchingExtensionImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  Initialize(LevelSetImageType *) override;

  double
  UpdateValue(const IndexType & index, const SpeedImageType * speed, LevelSetImageType * output) override;

  /** Generate the output image meta information */
  void
  GenerateOutputInformation() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

private:
  typename AuxValueContainer::Pointer m_AuxAliveValues;
  typename AuxValueContainer::Pointer m_AuxTrialValues;

  AuxImageType * m_AuxImages[AuxDimension];
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastMarchingExtensionImageFilter.hxx"
#endif

#endif
