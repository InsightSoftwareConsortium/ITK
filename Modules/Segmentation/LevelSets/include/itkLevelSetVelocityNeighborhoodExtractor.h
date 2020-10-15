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
#ifndef itkLevelSetVelocityNeighborhoodExtractor_h
#define itkLevelSetVelocityNeighborhoodExtractor_h

#include "itkLevelSetNeighborhoodExtractor.h"

namespace itk
{
/** \class LevelSetVelocityNeighborhoodExtractor
 * \brief Locate pixels of a particular level set.
 *
 * LevelSetVelocityNeighborhoodExtractor extends the functionality of
 * LevelSetNeighborhoodExtractor by also extracting the values
 * of velocity variables at the specified level set. Specifically,
 * it populates two containers: one containing the value of velocity
 * variables immediately inside the contour defined by the level set and the
 * other containing values for velocity variables immediately outside.
 *
 * The containers AuxInsideValues() and AuxOutsideValues() can
 * be used in conjunction with Superclass::InsidePoints() and
 * Superclass::OutsidePoints() in FastMarchingExtensionImageFilter
 * to produce images which extends the velocity variables smoothly
 * from the specified level set.
 *
 * This class is templated over the image type representing
 * the level set, the type of the auxiliary/velocity variables and the
 * number of auxiliary/velocity variables.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation
 *
 * \ingroup ITKLevelSets
 */
template <typename TLevelSet, typename TAuxValue, unsigned int VAuxDimension = 1>
class ITK_TEMPLATE_EXPORT LevelSetVelocityNeighborhoodExtractor : public LevelSetNeighborhoodExtractor<TLevelSet>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetVelocityNeighborhoodExtractor);

  /** Standard class typdedefs. */
  using Self = LevelSetVelocityNeighborhoodExtractor;
  using Superclass = LevelSetNeighborhoodExtractor<TLevelSet>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetVelocityNeighborhoodExtractor, LevelSetNeighborhoodExtractor);

  /** The type of the level set. */
  using LevelSetType = LevelSetTypeDefault<TLevelSet>;

  /** The dimension of the level set. */
  static constexpr unsigned int SetDimension = LevelSetType::SetDimension;

  /** Index type alias support */
  using Index = ::itk::Index<Self::SetDimension>;

  /** AuxVarType type alias support */
  using AuxVarType = AuxVarTypeDefault<TAuxValue, VAuxDimension, Self::SetDimension>;
  using AuxValueType = typename AuxVarType::AuxValueType;
  using AuxValueVectorType = typename AuxVarType::AuxValueVectorType;
  using AuxValueContainer = typename AuxVarType::AuxValueContainer;
  using AuxImageType = typename AuxVarType::AuxImageType;
  using AuxImagePointer = typename AuxVarType::AuxImagePointer;
  using AuxImageConstPointer = typename AuxVarType::AuxImageConstPointer;

  /** Set the auxiliary images. */
  void
  SetAuxImage(const AuxImageType * ptr, unsigned int idx = 0)
  {
    if (idx < VAuxDimension && m_AuxImage[idx] != ptr)
    {
      m_AuxImage[idx] = ptr;
    }
    this->Modified();
  }

  /** Get the auxiliary images. */
  AuxImageConstPointer
  GetAuxImage(unsigned int idx = 0)
  {
    if (idx >= VAuxDimension)
    {
      return nullptr;
    }
    else
    {
      return m_AuxImage[idx];
    }
  }

  /** Get the container of auxiliary values associated with the inside
   *  points. */
  itkGetModifiableObjectMacro(AuxInsideValues, AuxValueContainer);

  /** Get the container of auxiliary values associate with the outside
   *  points. */
  itkGetModifiableObjectMacro(AuxOutsideValues, AuxValueContainer);

protected:
  LevelSetVelocityNeighborhoodExtractor();
  ~LevelSetVelocityNeighborhoodExtractor() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  Initialize() override;

  double
  CalculateDistance(Index & index) override;

private:
  typename AuxValueContainer::Pointer m_AuxInsideValues;
  typename AuxValueContainer::Pointer m_AuxOutsideValues;
  AuxImageConstPointer                m_AuxImage[VAuxDimension];
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetVelocityNeighborhoodExtractor.hxx"
#endif

#endif
