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
#ifndef itkNormalVectorFunctionBase_h
#define itkNormalVectorFunctionBase_h

#include "itkFiniteDifferenceSparseImageFunction.h"

namespace itk
{
/**
 * \class NormalVectorFunctionBase
 *
 * \brief This class defines the common functionality for Sparse Image
 * neighborhoods of unit vectors.
 *
 * \par
 * This class is derived from the FiniteDifferenceSparseImageFunction class and
 * adds the functionality needed to process unit normal vector
 * neighborhoods.
 *
 * \par
 * This class is the parent class of the NormalVectorDiffusionFunction
 * class which defines all the necessary functionality for performing diffusion
 * operations on unit vectors stored in sparse images. Other (non-diffusion)
 * filters (such as median filtering) on unit vectors can also be derived from
 * this base class.
 *
 * \par PARAMETERS
 * This function class has a time step parameter which is returned by the
 * ComputeGloablTimeStep method. Unlike other finite difference function
 * classes, this class does not use the maximum change magnitude to compute the
 * time step, it returns this predetermined time step.
 * \ingroup ITKLevelSets
 */

template <typename TSparseImageType>
class ITK_TEMPLATE_EXPORT NormalVectorFunctionBase : public FiniteDifferenceSparseImageFunction<TSparseImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NormalVectorFunctionBase);

  /** Standard class type alias. */
  using Self = NormalVectorFunctionBase;
  using Superclass = FiniteDifferenceSparseImageFunction<TSparseImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(NormalVectorFunctionBase, FiniteDifferenceSparseImageFunction);

  /** Image dimension derived from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Typedefs from the superclass. */
  using TimeStepType = typename Superclass::TimeStepType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using IndexType = typename Superclass::IndexType;
  using SparseImageType = typename Superclass::SparseImageType;

  /** The node type for the sparse image. */
  using NodeType = typename SparseImageType::NodeType;

  /** The basic floating point type for the variables. */
  using NodeValueType = typename NodeType::NodeValueType;

  /** The vector type for the normals. */
  using NormalVectorType = typename NodeType::NodeDataType;

  /** Globaldata methods are not needed in this class. */
  void *
  GetGlobalDataPointer() const override
  {
    return nullptr;
  }
  void
  ReleaseGlobalDataPointer(void *) const override
  {}

  /** For the global time step, we return the time step parameter. */
  TimeStepType
  ComputeGlobalTimeStep(void *) const override
  {
    return m_TimeStep;
  }

  /** Sets the time step. */
  void
  SetTimeStep(const TimeStepType & ts)
  {
    m_TimeStep = ts;
  }

  /** Returns the time step. */
  TimeStepType
  GetTimeStep() const
  {
    return m_TimeStep;
  }

protected:
  NormalVectorFunctionBase();
  ~NormalVectorFunctionBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The time step for normal vector finite difference computations. */
  TimeStepType m_TimeStep;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalVectorFunctionBase.hxx"
#endif

#endif
