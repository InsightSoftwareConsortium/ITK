/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCorrespondenceDataStructure.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
 * \brief A data structure designed to contain medial node clique correspondence data between two images.  
 * 
 *
 * \ingroup 
 *
 * \sa 
 *
 *  */

template<typename TItemType, int VCliqueSize>
class CorrespondenceDataStructure : public DataObject {
public:

  /** Standard class typedefs. */   
  typedef CorrespondenceDataStructure Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CorrespondenceDataStructure, DataObject);

  typedef TItemType ItemType;

  /** Virtual method required by DataObject interface.   */
  void UpdateOutputInformation() {}

  /** The typedef for the CorrespondingPairList. */
  typedef CorrespondingList<TItemType, VCliqueSize> CorrespondingListType;

  /** The typedef for the NodePairList. */
  typedef SecondaryNodeList<CorrespondingListType, VCliqueSize> SecondaryNodeListType;

  /** The typedef for the NodeList. */
  typedef NodeList<SecondaryNodeListType> NodeListType;

  /** Node List. */
  NodeListType * m_NodeList;

protected:
  /** Default Constructor. */
  CorrespondenceDataStructure();

  /** Default Destructor. */
  ~CorrespondenceDataStructure() {};

  /** Virtual methods required by DataObject interface. */
  bool VerifyRequestedRegion() { return true; }
  void SetRequestedRegionToLargestPossibleRegion () {}
  bool RequestedRegionIsOutsideOfTheBufferedRegion () { return false; }
  void SetRequestedRegion (DataObject *) {}
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrespondenceDataStructure.txx"
#endif

#endif 
