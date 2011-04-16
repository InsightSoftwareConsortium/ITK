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
#ifndef __itkCorrespondenceDataStructure_h
#define __itkCorrespondenceDataStructure_h

#include "itkDataObject.h"
#include "itkNodeList.h"
#include "itkSecondaryNodeList.h"
#include "itkCorrespondingList.h"

namespace itk
{
/**
 * \class CorrespondenceDataStructure
 * \brief A data structure designed to contain medial node clique
 * correspondence data between two images.
 * \ingroup ITK-Blox
 */
template< typename TItemType, int VCliqueSize >
class CorrespondenceDataStructure:public DataObject
{
public:

  /** Standard class typedefs. */
  typedef CorrespondenceDataStructure Self;
  typedef DataObject                  Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CorrespondenceDataStructure, DataObject);

  typedef TItemType ItemType;
  itkStaticConstMacro(CliqueSize, unsigned int, VCliqueSize);

  /** The typedef for the CorrespondingPairList. */
  typedef CorrespondingList< TItemType, VCliqueSize > CorrespondingListType;

  /** The typedef for the NodePairList. */
  typedef SecondaryNodeList< CorrespondingListType, VCliqueSize >
  SecondaryNodeListType;

  /** The typedef for the NodeList. */
  typedef NodeList< SecondaryNodeListType > NodeListType;

  /** Node List. */
  NodeListType *m_NodeList;
protected:
  /** Default Constructor. */
  CorrespondenceDataStructure();

  /** Default Destructor. */
  ~CorrespondenceDataStructure();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrespondenceDataStructure.txx"
#endif

#endif
