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
#ifndef itkNarrowBand_h
#define itkNarrowBand_h

#include "itkLightObject.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include <vector>

namespace itk
{
/**
 * A data structure used with NarrowBand to store a list of indices
 * (m_Index) that define the band, and an accompanying list of update values
 * (m_Data) computed by CalculateChange
 */

template< typename TIndexType, typename TDataType >
class ITK_TEMPLATE_EXPORT BandNode
{
public:
  TDataType   m_Data;
  TIndexType  m_Index;
  signed char m_NodeState;
  BandNode() :
    m_Data(NumericTraits<TDataType>::ZeroValue()), m_NodeState(0)
  {}
};

/** \class NarrowBand
 * \brief Narrow Band class
 * \ingroup ITKNarrowBand
 */
template< typename NodeType >
class ITK_TEMPLATE_EXPORT NarrowBand:public LightObject
{
public:
  /** Standard class typedefs */
  typedef NarrowBand                 Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NarrowBand, LightObject);

  typedef std::vector< NodeType >                    NodeContainerType;
  typedef typename NodeContainerType::size_type      SizeType;
  typedef typename NodeContainerType::const_iterator ConstIterator;
  typedef typename NodeContainerType::iterator       Iterator;

  /** Begin is the first valid iterator position within the region.  End is ONE
      PAST the last valid iterator position in the region. */
  typedef struct RegionStruct {
    Iterator Begin;
    Iterator End;
  } RegionType;

  /** Returns an array of RegionStructs which represent contiguous
   * arrays of nodes within the narrow band. */
#if !defined( ITK_WRAPPING_PARSER )
  std::vector< RegionType > SplitBand(const SizeType&);

#endif

  Iterator Begin()
  {
    return m_NodeContainer.begin();
  }

  ConstIterator Begin() const
  {
    return m_NodeContainer.begin();
  }

  Iterator End()
  {
    return m_NodeContainer.end();
  }

  ConstIterator End() const
  {
    return m_NodeContainer.end();
  }

  SizeType Size() const
  {
    return m_NodeContainer.size();
  }

  bool Empty() const
  {
    return m_NodeContainer.empty();
  }

  /** Clear the narrow band container. */
  void Clear()
  {
    m_NodeContainer.clear();
  }

  void Reserve(SizeType n)
  {
    m_NodeContainer.reserve(n);
  }

  void PushBack(const NodeType & n)
  {
    m_NodeContainer.push_back(n);
  }

  void PopBack()
  {
    m_NodeContainer.pop_back();
  }

  void Resize(SizeType n)
  {
    m_NodeContainer.resize(n);
  }

  NodeType & operator[](SizeType n)
  {
    return m_NodeContainer[n];
  }

  const NodeType & operator[](SizeType n) const
  {
    return m_NodeContainer[n];
  }

  /** Set/Get the narrow band total radius. The narrow band width will be twice
  this value (positive and negative distance to the zero level set).
  The user of the narrow band container should set up this value properly. */
  void SetTotalRadius(const float& val) { m_TotalRadius = val; }

  float GetTotalRadius() const { return m_TotalRadius; }

  /** Set/Get the narrow band inner radius. The inner radius is the safe are
  where the level set can be computed.*/
  void SetInnerRadius(const float& val) { m_InnerRadius = val; }

  float GetInnerRadius() const { return m_InnerRadius; }

protected:
  NarrowBand() : m_TotalRadius( 0.0 ), m_InnerRadius( 0.0 ) {}

  float m_TotalRadius;
  float m_InnerRadius;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NarrowBand);

  NodeContainerType m_NodeContainer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNarrowBand.hxx"
#endif

#endif
