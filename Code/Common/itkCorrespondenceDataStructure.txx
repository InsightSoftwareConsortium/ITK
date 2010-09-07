/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCorrespondenceDataStructure.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCorrespondenceDataStructure_txx
#define __itkCorrespondenceDataStructure_txx

#include "itkCorrespondenceDataStructure.h"
#include "itkNodeList.h"

namespace itk
{
/**
 * Constructor.
 */
template< typename TItemType, int VCliqueSize >
CorrespondenceDataStructure< TItemType, VCliqueSize >
::CorrespondenceDataStructure()
{
  // Initialize NodeList.
  m_NodeList = new NodeListType();
}

template< typename TItemType, int VCliqueSize >
CorrespondenceDataStructure< TItemType, VCliqueSize >
::~CorrespondenceDataStructure()
{
  if ( m_NodeList )
    {
    // do we need to delete every item in the list as well?
    delete m_NodeList;
    m_NodeList = 0;
    }
}
} // end namespace itk

#endif
