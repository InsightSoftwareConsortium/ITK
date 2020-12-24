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

#ifndef itkFastMarchingImageFilterBase_h
#define itkFastMarchingImageFilterBase_h

#include "itkFastMarchingBase.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"
#include "itkArray.h"
#include <bitset>

namespace itk
{
/**
 * \class FastMarchingImageFilterBase
 * \brief Apply the Fast Marching method to solve an Eikonal equation on an image.
 *
 * The speed function can be specified as a speed image or a
 * speed constant. The speed image is set using the method
 * SetInput(). If the speed image is nullptr, a constant speed function
 * is used and is specified using method the SetSpeedConstant().
 *
 * If the speed function is constant and of value one, fast marching results
 * is an approximate distance function from the initial alive points.
 *
 * There are two ways to specify the output image information
 * (LargestPossibleRegion, Spacing, Origin):
 * \li it is copied directly from the input speed image
 * \li it is specified by the user.
 * Default values are used if the user does not specify all the information.
 *
 * The output information is computed as follows.
 *
 * If the speed image is nullptr or if the OverrideOutputInformation is set to
 * true, the output information is set from user specified parameters. These
 * parameters can be specified using methods
 * \li FastMarchingImageFilterBase::SetOutputRegion(),
 * \li FastMarchingImageFilterBase::SetOutputSpacing(),
 * \li FastMarchingImageFilterBase::SetOutputDirection(),
 * \li FastMarchingImageFilterBase::SetOutputOrigin().
 *
 * Else the output information is copied from the input speed image.
 *
 * Implementation of this class is based on Chapter 8 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * For an alternative implementation, see itk::FastMarchingImageFilter.
 *
 * \tparam TTraits traits
 *
 * \sa FastMarchingImageFilter
 * \sa ImageFastMarchingTraits
 * \sa ImageFastMarchingTraits2
 *
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT FastMarchingImageFilterBase : public FastMarchingBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingImageFilterBase);

  using Self = FastMarchingImageFilterBase;
  using Superclass = FastMarchingBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Traits = typename Superclass::Traits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingImageFilterBase, FastMarchingBase);


  using InputImageType = typename Superclass::InputDomainType;
  using InputImagePointer = typename Superclass::InputDomainPointer;
  using InputPixelType = typename Superclass::InputPixelType;

  using OutputImageType = typename Superclass::OutputDomainType;
  using OutputImagePointer = typename Superclass::OutputDomainPointer;
  using OutputPixelType = typename Superclass::OutputPixelType;
  using OutputSpacingType = typename OutputImageType::SpacingType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using OutputRegionType = typename OutputImageType::RegionType;
  using OutputPointType = typename OutputImageType::PointType;
  using OutputDirectionType = typename OutputImageType::DirectionType;

  using NodeType = typename Traits::NodeType;
  using NodePairType = typename Traits::NodePairType;
  using NodePairContainerType = typename Traits::NodePairContainerType;
  using NodePairContainerPointer = typename Traits::NodePairContainerPointer;
  using NodePairContainerConstIterator = typename Traits::NodePairContainerConstIterator;

  using LabelType = typename Superclass::LabelType;

  static constexpr unsigned int ImageDimension = Traits::ImageDimension;


  using LabelImageType = Image<unsigned char, ImageDimension>;
  using LabelImagePointer = typename LabelImageType::Pointer;

  using ConnectedComponentImageType = Image<unsigned int, ImageDimension>;
  using ConnectedComponentImagePointer = typename ConnectedComponentImageType::Pointer;

  using NeighborhoodIteratorType = NeighborhoodIterator<LabelImageType>;
  using NeighborhoodRadiusType = typename NeighborhoodIteratorType::RadiusType;

  class InternalNodeStructure;


  using InternalNodeStructureArray = FixedArray<InternalNodeStructure, ImageDimension>;

  itkGetModifiableObjectMacro(LabelImage, LabelImageType);

  /** The output largest possible, spacing and origin is computed as follows.
   * If the speed image is nullptr or if the OverrideOutputInformation is true,
   * the output information is set from user specified parameters. These
   * parameters can be specified using methods SetOutputRegion(),
   * SetOutputSpacing(), SetOutputDirection(), and SetOutputOrigin().
   * Else if the speed image is not nullptr, the output information
   * is copied from the input speed image. */
  virtual void
  SetOutputSize(const OutputSizeType & size)
  {
    m_OutputRegion = size;
  }
  virtual OutputSizeType
  GetOutputSize() const
  {
    return m_OutputRegion.GetSize();
  }
  itkSetMacro(OutputRegion, OutputRegionType);
  itkGetConstReferenceMacro(OutputRegion, OutputRegionType);
  itkSetMacro(OutputSpacing, OutputSpacingType);
  itkGetConstReferenceMacro(OutputSpacing, OutputSpacingType);
  itkSetMacro(OutputDirection, OutputDirectionType);
  itkGetConstReferenceMacro(OutputDirection, OutputDirectionType);
  itkSetMacro(OutputOrigin, OutputPointType);
  itkGetConstReferenceMacro(OutputOrigin, OutputPointType);
  itkSetMacro(OverrideOutputInformation, bool);
  itkGetConstReferenceMacro(OverrideOutputInformation, bool);
  itkBooleanMacro(OverrideOutputInformation);

protected:
  FastMarchingImageFilterBase();

  ~FastMarchingImageFilterBase() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  OutputRegionType m_BufferedRegion;
  NodeType         m_StartIndex;
  NodeType         m_LastIndex;

  OutputRegionType    m_OutputRegion;
  OutputPointType     m_OutputOrigin;
  OutputSpacingType   m_OutputSpacing;
  OutputDirectionType m_OutputDirection;
  bool                m_OverrideOutputInformation{ false };

  /** Generate the output image meta information. */
  void
  GenerateOutputInformation() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  LabelImagePointer              m_LabelImage;
  ConnectedComponentImagePointer m_ConnectedComponentImage;

  IdentifierType
  GetTotalNumberOfNodes() const override;

  void
  SetOutputValue(OutputImageType * oImage, const NodeType & iNode, const OutputPixelType & iValue) override;

  /** Returns the output value for a given node */
  const OutputPixelType
  GetOutputValue(OutputImageType * oImage, const NodeType & iNode) const override;

  /** Returns the label value for a given node */
  unsigned char
  GetLabelValueForGivenNode(const NodeType & iNode) const override;

  /** Set the label value for a given node */
  void
  SetLabelValueForGivenNode(const NodeType & iNode, const LabelType & iLabel) override;

  /** Update values for the neighbors of a given node */
  void
  UpdateNeighbors(OutputImageType * oImage, const NodeType & iNode) override;

  /** Update value for a given node */
  void
  UpdateValue(OutputImageType * oImage, const NodeType & iNode) override;

  /** Make sure the given node does not violate any topological constraint*/
  bool
  CheckTopology(OutputImageType * oImage, const NodeType & iNode) override;
  void
  InitializeOutput(OutputImageType * oImage) override;

  /** Find the nodes were the front will propagate given a node */
  void
  GetInternalNodesUsed(OutputImageType * oImage, const NodeType & iNode, InternalNodeStructureArray & ioNodesUsed);

  /** Solve the quadratic equation */
  double
  Solve(OutputImageType * oImage, const NodeType & iNode, InternalNodeStructureArray & iNeighbors) const;

  //
  // Functions and variables to check for topology changes (2D/3D only).
  //

  // Functions/data for the 2-D case
  void
  InitializeIndices2D();
  bool
  IsChangeWellComposed2D(const NodeType &) const;
  bool
  IsCriticalC1Configuration2D(const std::bitset<9> &) const;
  bool
  IsCriticalC2Configuration2D(const std::bitset<9> &) const;
  bool
  IsCriticalC3Configuration2D(const std::bitset<9> &) const;
  bool
  IsCriticalC4Configuration2D(const std::bitset<9> &) const;

  Array<unsigned char> m_RotationIndices[4];
  Array<unsigned char> m_ReflectionIndices[2];

  // Functions/data for the 3-D case
  void
  InitializeIndices3D();
  bool
  IsCriticalC1Configuration3D(const std::bitset<8> &) const;
  unsigned int
  IsCriticalC2Configuration3D(const std::bitset<8> &) const;
  bool
  IsChangeWellComposed3D(const NodeType &) const;

  Array<unsigned char> m_C1Indices[12];
  Array<unsigned char> m_C2Indices[8];

  // Functions for both 2D/3D cases
  bool
  DoesVoxelChangeViolateWellComposedness(const NodeType &) const;
  bool
  DoesVoxelChangeViolateStrictTopology(const NodeType &) const;

  const InputImageType * m_InputCache;

private:
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastMarchingImageFilterBase.hxx"
#endif

#endif // itkFastMarchingImageFilterBase_h
