/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBand.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNarrowBand_h
#define __itkNarrowBand_h

#include "itkLightObject.h"
#include "itkObjectFactory.h"
#include <vector>

namespace itk {
  /** 
   * A data structure used with NarrowBand to store a list of indices 
   * (m_Index) that define the band, and an accompanying list of update values 
   * (m_Data) computed by CalculateChange 
   */
      
template <class TIndexType, class TDataType>
class BandNode
{
public:
  TDataType m_Data;
  TIndexType m_Index;
  signed char m_NodeState;
};  


/** */
template <class NodeType>
class ITK_EXPORT NarrowBand : public LightObject
{
public:
  /** Standard class typedefs */
  typedef NarrowBand   Self;
  typedef LightObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NarrowBand, LightObject);

  typedef std::vector<NodeType> NodeContainerType;
  typedef typename NodeContainerType::size_type SizeType;
  typedef typename NodeContainerType::const_iterator ConstIterator;
  typedef typename NodeContainerType::iterator Iterator;

  /** Begin is the first valid iterator position within the region.  End is ONE
      PAST the last valid iterator position in the region. */
  typedef struct RegionStruct
  {
    Iterator Begin;
    Iterator End;
  } RegionType;

  //typedef struct RegionStruct
  //{
  //  ConstIterator Begin;
  //  ConstIterator End;
 // } ConstRegionType;

  /** Returns an array of RegionStructs which represent contiguous arrays
      of nodes within the narrow band. */
  std::vector<struct RegionStruct> SplitBand( unsigned int );
  
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
  void Reserve( SizeType n)
  {
    m_NodeContainer.reserve( n );
  }
  void PushBack( const NodeType &n)
  {
    m_NodeContainer.push_back(n);
  }
  void PopBack()
  {
    m_NodeContainer.pop_back();
  }
  void Resize( SizeType n )
  {
    m_NodeContainer.resize(n);
  }
  
  NodeType &operator[]( SizeType n )
  {
    return m_NodeContainer[n];
  }
  const NodeType& operator[](SizeType n) const
  {
    return m_NodeContainer[n];
  }

  /** Set/Get the narrow band total radius. The narrow band width will be twice
  this value (positive and negative distance to the zero level set).
  The user of the narrow band container should set up this value properly. */
  void SetTotalRadius(float val) { m_TotalRadius = val;}

  float GetTotalRadius(){return m_TotalRadius;}
 
  /** Set/Get the narrow band inner radius. The inner radius is the safe are
  where the level set can be computed.*/
  void SetInnerRadius(float val) { m_InnerRadius = val;}

  float GetInnerRadius() { return m_InnerRadius;}

  
protected:
  NarrowBand() {m_TotalRadius = 0.0; m_InnerRadius = 0.0;};
  float m_TotalRadius;
  float m_InnerRadius;
  
private:
  NarrowBand(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  NodeContainerType m_NodeContainer;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNarrowBand.txx"
#endif

#endif
