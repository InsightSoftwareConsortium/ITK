/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFastMarchingExtensionImageFilterBase_h
#define itkFastMarchingExtensionImageFilterBase_h

#include "itkFastMarchingImageFilterBase.h"
#include "itkVectorContainer.h"

namespace itk
{
/**
 * \class FastMarchingExtensionImageFilterBase
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
 * Implementation of this class is based on \cite sethian1999b.
 *
 * For an alternative implementation, see itk::FastMarchingExtensionImageFilter.
 *
 * \sa FastMarchingExtensionImageFilter
 * \sa FastMarchingImageFilter
 * \sa LevelSetTypeDefault
 * \sa AuxVarTypeDefault
 *
 * \ingroup LevelSetSegmentation
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput, typename TAuxValue, unsigned int VAuxDimension>
class ITK_TEMPLATE_EXPORT FastMarchingExtensionImageFilterBase : public FastMarchingImageFilterBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingExtensionImageFilterBase);

  /** Standard class typedefs. */
  using Self = FastMarchingExtensionImageFilterBase;
  using Superclass = FastMarchingImageFilterBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using typename Superclass::Traits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(FastMarchingExtensionImageFilterBase);

  /** The dimension of the level set. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Number of auxiliary variables to be extended. */
  static constexpr unsigned int AuxDimension = VAuxDimension;

  /** AuxVarType type alias support */
  using AuxValueType = TAuxValue;
  using AuxValueVectorType = Vector<AuxValueType, AuxDimension>;
  using AuxValueContainerType = VectorContainer<AuxValueVectorType>;

  using AuxValueContainerPointer = typename AuxValueContainerType::Pointer;
  using AuxValueContainerConstIterator = typename AuxValueContainerType::ConstIterator;

  using AuxImageType = Image<AuxValueType, ImageDimension>;
  using AuxImagePointer = typename AuxImageType::Pointer;


  /** Index type alias support */
  using typename Superclass::NodeType;
  using typename Superclass::NodePairType;

  //  using typename Superclass::NodeContainerType;
  //  using typename Superclass::NodeContainerPointer;
  //  using typename Superclass::NodeContainerConstIterator;

  using typename Superclass::NodePairContainerType;
  using typename Superclass::NodePairContainerPointer;
  using typename Superclass::NodePairContainerConstIterator;

  using typename Superclass::OutputImageType;
  using typename Superclass::OutputPixelType;
  using typename Superclass::InternalNodeStructure;

  /** Get one of the extended auxiliary variable image. */
  AuxImageType *
  GetAuxiliaryImage(const unsigned int idx);

  /** Set the container auxiliary values at the initial alive points. */
  /** @ITKStartGrouping */
  itkSetObjectMacro(AuxiliaryAliveValues, AuxValueContainerType);
  itkGetModifiableObjectMacro(AuxiliaryAliveValues, AuxValueContainerType);
  /** @ITKEndGrouping */
  /** Set the container of auxiliary values at the initial trial points. */
  /** @ITKStartGrouping */
  itkSetObjectMacro(AuxiliaryTrialValues, AuxValueContainerType);
  itkGetModifiableObjectMacro(AuxiliaryTrialValues, AuxValueContainerType);
  /** @ITKEndGrouping */
  itkConceptMacro(AuxValueHasNumericTraitsCheck, (Concept::HasNumericTraits<TAuxValue>));

protected:
  FastMarchingExtensionImageFilterBase();
  ~FastMarchingExtensionImageFilterBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  InitializeOutput(OutputImageType *) override;

  void
  UpdateValue(OutputImageType * oImage, const NodeType & iNode) override;

  /** Generate the output image meta information */
  void
  GenerateOutputInformation() override;

  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  AuxValueContainerPointer m_AuxiliaryAliveValues{};
  AuxValueContainerPointer m_AuxiliaryTrialValues{};

private:
  AuxImageType * m_AuxImages[VAuxDimension]{};
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastMarchingExtensionImageFilterBase.hxx"
#endif

#endif
