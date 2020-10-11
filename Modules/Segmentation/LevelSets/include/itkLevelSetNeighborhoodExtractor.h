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
#ifndef itkLevelSetNeighborhoodExtractor_h
#define itkLevelSetNeighborhoodExtractor_h

#include "itkLightProcessObject.h"
#include "itkLevelSet.h"
#include "itkIndex.h"

namespace itk
{
/** \class LevelSetNeighborhoodExtractor
 * \brief Locate pixels of a particular level set.
 *
 * LevelSetNeighborhoodExtractor locates a particular level set in the input
 * level set. Specifically, the method Locate() fills two containers: one
 * containing pixels immediately inside the contour defined by the level set
 * and the other containing pixels immediately outside.  For each located
 * pixel, an estimated distance to the particular level set is also
 * calculated.
 *
 * The containers InsidePoints and OutsidePoints can then be used
 * in FastMarchingImageFilter to produce a signed distance map from
 * the specified level set.
 *
 * This class is templated over the image type representing
 * the level set.
 *
 * This class supports narrowbanding. If a input narrowband is
 * provided, the algorithm will only search pixels within the
 * narrowband.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation
 *
 * \ingroup ITKLevelSets
 */
template <typename TLevelSet>
class ITK_TEMPLATE_EXPORT LevelSetNeighborhoodExtractor : public LightProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevelSetNeighborhoodExtractor);

  /** Standard class typdedefs. */
  using Self = LevelSetNeighborhoodExtractor;
  using Superclass = LightProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetNeighborhoodExtractor, Object);

  /** LevelSetType type alias support */
  using LevelSetType = LevelSetTypeDefault<TLevelSet>;
  using LevelSetImageType = typename LevelSetType::LevelSetImageType;
  using LevelSetPointer = typename LevelSetType::LevelSetPointer;
  using LevelSetConstPointer = typename LevelSetType::LevelSetConstPointer;
  using PixelType = typename LevelSetType::PixelType;
  using NodeType = typename LevelSetType::NodeType;
  using NodeContainer = typename LevelSetType::NodeContainer;
  using NodeContainerPointer = typename LevelSetType::NodeContainerPointer;

  /** SetDimension enumeration. */
  static constexpr unsigned int SetDimension = LevelSetType::SetDimension;

  /** Index type alias support */
  using IndexType = Index<Self::SetDimension>;

  /** Get/Set the input level set. */
  itkSetConstObjectMacro(InputLevelSet, LevelSetImageType);
  itkGetConstObjectMacro(InputLevelSet, LevelSetImageType);

  /** Set the value of the level set to be located. Default is 0. */
  itkSetMacro(LevelSetValue, double);

  /** Get the value of the level set to be located. */
  itkGetConstMacro(LevelSetValue, double);

  /** Set the narrow band width. Default is 12. */
  itkSetClampMacro(NarrowBandwidth, double, 0.0, NumericTraits<double>::max());

  /** Get the narrow band width. */
  itkGetConstMacro(NarrowBandwidth, double);

  /** Set the narrowbanding flag. */
  itkSetMacro(NarrowBanding, bool);

  /** Get the narrowbanding flag. */
  itkGetConstMacro(NarrowBanding, bool);
  itkBooleanMacro(NarrowBanding);

  /** Set/Get the input narrowband. A narrowband is represented as
   * a VectorContainer of LevelSetNodes. */
  void
  SetInputNarrowBand(NodeContainer * ptr);
  itkGetModifiableObjectMacro(InputNarrowBand, NodeContainer);

  /** Get the container of inside points. The inside points are
   * stored in a VectorContainer of LevelSetNodes. */
  NodeContainerPointer
  GetInsidePoints()
  {
    return m_InsidePoints;
  }

  /** Get the container of outside points. The outside points are
   * stored in a VectorContainer of LevelSetNodes. */
  NodeContainerPointer
  GetOutsidePoints()
  {
    return m_OutsidePoints;
  }

  /** Locate the level set. This method evokes the level set
   * location algorithm. */
  void
  Locate();

protected:
  LevelSetNeighborhoodExtractor();
  ~LevelSetNeighborhoodExtractor() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LevelSetImageType::PixelType
  GetLargeValue() const
  {
    return m_LargeValue;
  }

  const NodeType &
  GetNodeUsedInCalculation(unsigned int idx) const
  {
    return m_NodesUsed[idx];
  }

  virtual void
  Initialize();

  virtual double
  CalculateDistance(IndexType & index);

  void
  GenerateData() override;

  bool
  GetLastPointIsInside() const
  {
    return m_LastPointIsInside;
  }

private:
  void
  GenerateDataFull();

  void
  GenerateDataNarrowBand();

  double m_LevelSetValue{ 0.0 };

  NodeContainerPointer m_InsidePoints;
  NodeContainerPointer m_OutsidePoints;

  LevelSetConstPointer m_InputLevelSet;

  bool                 m_NarrowBanding{ false };
  double               m_NarrowBandwidth{ 12.0 };
  NodeContainerPointer m_InputNarrowBand;

  typename LevelSetImageType::RegionType m_ImageRegion;
  typename LevelSetImageType::PixelType  m_LargeValue;

  std::vector<NodeType> m_NodesUsed;

  bool m_LastPointIsInside{ false };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetNeighborhoodExtractor.hxx"
#endif

#endif
