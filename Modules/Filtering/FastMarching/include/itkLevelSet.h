/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
template< typename TLevelSet >
class LevelSetTypeDefault
{
public:
  /** LevelSetType typedef support. */
  typedef LevelSetTypeDefault Self;
  typedef TLevelSet           LevelSetImageType;

  /** SetDimension enumeration. */
  itkStaticConstMacro(SetDimension, unsigned int, TLevelSet::ImageDimension);

  /** LevelSetPointer typedef support. */
  typedef typename TLevelSet::Pointer      LevelSetPointer;
  typedef typename TLevelSet::ConstPointer LevelSetConstPointer;

  /** PixelType typedef support. */
  typedef typename TLevelSet::PixelType PixelType;

  /** Node typdef support. */
  typedef
  LevelSetNode< PixelType, itkGetStaticConstMacro(SetDimension) > NodeType;

  /** NodeContainer typedef support. */
  typedef VectorContainer< unsigned int, NodeType > NodeContainer;

  /** NodeContainerPointer typedef support. */
  typedef typename NodeContainer::Pointer NodeContainerPointer;
};

/** \class AuxVarTypeDefault
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
template<
  typename TPixel,
  unsigned int VAuxDimension = 1,
  unsigned int VSetDimension = 2
  >
class AuxVarTypeDefault
{
public:
  /** Standard typedefs */
  typedef AuxVarTypeDefault Self;

  /** PixelType typedef support. */
  typedef TPixel AuxValueType;

  /** Auxiliary variable dimension. */
  itkStaticConstMacro(AuxDimension, unsigned int, VAuxDimension);

  /** Level set dimension. */
  itkStaticConstMacro(SetDimension, unsigned int, VSetDimension);

  /** AuxVector typedef support. */
  typedef Vector< TPixel, VAuxDimension > AuxValueVectorType;

  /** AuxContainer typdef support. */
  typedef VectorContainer< unsigned int, AuxValueVectorType > AuxValueContainer;

  /** AuxImage typdef support. */
  typedef Image< AuxValueType, VSetDimension > AuxImageType;

  /** AuxImagePointer typedef support. */
  typedef typename AuxImageType::Pointer      AuxImagePointer;
  typedef typename AuxImageType::ConstPointer AuxImageConstPointer;
};
} // end namespace itk

#endif
