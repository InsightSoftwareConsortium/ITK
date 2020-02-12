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
#ifndef itkLevelSet_h
#define itkLevelSet_h

#include "itkLevelSetNode.h"

namespace itk
{
/**  \class LevelSetTypeDefault
 * \brief Level set type information.
 *
 * LevelSetTypeDefault is a simple class that holds type information
 * useful for level set algorithms. This class is templated over the
 * level set image type.
 *
 * A NodeContainer contains a group or collection of level set
 * node or grid points useful for representing a narrowband or
 * region of interest.
 *
 * \ingroup LevelSetSegmentation
 * \ingroup ITKFastMarching
 */
template <typename TLevelSet>
class LevelSetTypeDefault
{
public:
  /** LevelSetType type alias support */
  using Self = LevelSetTypeDefault;
  using LevelSetImageType = TLevelSet;

  /** SetDimension enumeration. */
  static constexpr unsigned int SetDimension = TLevelSet::ImageDimension;

  /** LevelSetPointer type alias support */
  using LevelSetPointer = typename TLevelSet::Pointer;
  using LevelSetConstPointer = typename TLevelSet::ConstPointer;

  /** PixelType type alias support */
  using PixelType = typename TLevelSet::PixelType;

  /** Node type alias support. */
  using NodeType = LevelSetNode<PixelType, Self::SetDimension>;

  /** NodeContainer type alias support */
  using NodeContainer = VectorContainer<unsigned int, NodeType>;

  /** NodeContainerPointer type alias support */
  using NodeContainerPointer = typename NodeContainer::Pointer;
};

/**
 *\class AuxVarTypeDefault
 * \brief Level set auxiliary variables type information.
 *
 * \brief AuxVarTypeDefault is a simple class that holds type information
 * for auxiliary variables in some level set algorithms. This class is templated
 * over the auxiliary variable data type, the number of auxiliary variables
 * and the level set dimension.
 *
 * A AuxValueContainer contains a collection of auxiliary
 * values vectors. It is used in conjunction with
 * LevelSetTypeDefault::NodeContainer to represent auxiliary variable values
 * in a group or collection of level set nodes or grid positions.
 *
 * \ingroup LevelSetSegmentation
 * \ingroup ITKFastMarching
 */
template <typename TPixel, unsigned int VAuxDimension = 1, unsigned int VSetDimension = 2>
class AuxVarTypeDefault
{
public:
  /** Standard type alias */
  using Self = AuxVarTypeDefault;

  /** PixelType type alias support */
  using AuxValueType = TPixel;

  /** Auxiliary variable dimension. */
  static constexpr unsigned int AuxDimension = VAuxDimension;

  /** Level set dimension. */
  static constexpr unsigned int SetDimension = VSetDimension;

  /** AuxVector type alias support */
  using AuxValueVectorType = Vector<TPixel, VAuxDimension>;

  /** AuxContainer typdef support. */
  using AuxValueContainer = VectorContainer<unsigned int, AuxValueVectorType>;

  /** AuxImage typdef support. */
  using AuxImageType = Image<AuxValueType, VSetDimension>;

  /** AuxImagePointer type alias support */
  using AuxImagePointer = typename AuxImageType::Pointer;
  using AuxImageConstPointer = typename AuxImageType::ConstPointer;
};
} // end namespace itk

#endif
