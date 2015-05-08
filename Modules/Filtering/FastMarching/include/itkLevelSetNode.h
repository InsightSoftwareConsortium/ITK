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
#ifndef itkLevelSetNode_h
#define itkLevelSetNode_h

#include "itkIndex.h"
#include "itkImage.h"
#include "itkVectorContainer.h"
#include "itkVector.h"

namespace itk
{
/**
 * \class LevelSetNode
 * \brief Represent a node in a level set.
 *
 * LevelSetNode is a simple templated class that represents a node
 * or grid position of a level set. A group or collection of
 * LevelSetNode can then be used to represents a narrowband or
 * region of interest.
 *
 * LevelSetNode is templated over the data type and dimension of the
 * level set.
 *
 * \ingroup LevelSetSegmentation
 *
 * \ingroup ITKFastMarching
 */
template< typename TPixel, unsigned int VSetDimension = 2 >
class LevelSetNode
{
public:
  /** Standard class typedefs. */
  typedef LevelSetNode Self;

  /** Pixel typedef. */
  typedef TPixel PixelType;

  /** Level set dimension. */
  itkStaticConstMacro(SetDimension, unsigned int, VSetDimension);

  /** Index typedef. */
  typedef Index< VSetDimension > IndexType;

  /** Operator >. A LevelSetNode is sorted by its value field. */
  bool operator>(const Self & node) const
  { return m_Value > node.m_Value; }

  /** Operator <. A LevelSetNode is sorted by its value field. */
  bool operator<(const Self & node) const
  { return m_Value < node.m_Value; }

  /** Operator <=. A LevelSetNode is sorted by its value field. */
  bool operator<=(const Self & node) const
  { return m_Value <= node.m_Value; }

  /** Operator >=. A LevelSetNode is sorted by its value field. */
  bool operator>=(const Self & node) const
  { return m_Value >= node.m_Value; }

  /** Operator =. Two nodes are equal if both their value and index fields
   * are the same. */
  Self & operator=(const Self & rhs)
  {
    if ( this != &rhs )
      {
      m_Value = rhs.m_Value;
      m_Index = rhs.m_Index;
      }
    return *this;
  }

  /** Get/Set level set value. */
  PixelType & GetValue()
  { return m_Value; }
  const PixelType & GetValue() const
  { return m_Value; }
  void SetValue(const PixelType & input)
  { m_Value = input; }

  /** Get/Set index. */
  IndexType & GetIndex()
  { return m_Index; }
  const IndexType & GetIndex() const
  { return m_Index; }
  void SetIndex(const IndexType & input)
  { m_Index = input; }

  /** Default constructor */
  LevelSetNode():m_Value(NumericTraits< PixelType >::ZeroValue())
  {
    m_Index.Fill(0);
  }

  /** Copy constructor */
  LevelSetNode(const Self & node):m_Value(node.m_Value), m_Index(node.m_Index) {}

private:
  PixelType m_Value;
  IndexType m_Index;
};
} // end namespace itk

#endif
