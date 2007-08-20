/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseImageTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkSparseImage.h"
#include <iostream>

/* This test exercises the itkSparseImage class. */

namespace itk {

template <class TImageType>
class NodeClass 
{
public:
  typedef TImageType ImageType;
  typedef typename ImageType::IndexType IndexType;
  int m_Value;
  IndexType m_Index;
  NodeClass *Next;
  NodeClass *Previous;
};

}

int itkSparseImageTest(int, char* [] )
{
  typedef itk::Image<int, 2> DummyImageType;
  typedef itk::NodeClass<DummyImageType> NodeType;
  typedef itk::SparseImage<NodeType, 2> SparseImageType;
  typedef SparseImageType::Superclass ImageType;
  
  SparseImageType::Pointer im = SparseImageType::New();
  ImageType::RegionType r;
  ImageType::SizeType   sz = {{24, 24}};
  ImageType::IndexType  idx = {{0,0}};
  r.SetSize(sz);
  r.SetIndex(idx);
 
  im->SetLargestPossibleRegion(r);
  im->SetBufferedRegion(r);
  im->SetRequestedRegion(r);
  im->Allocate();

  ImageType::IndexType index;
  NodeType *node;
  int cnt = 0;
  
  for ( index[0]=0; index[0] < 24; index[0]++ )
    for ( index[1]=0; index[1] < 24; index[1]++ )
      {
      if ( (index[0]>=6) && (index[0]<=12) &&
           (index[1]>=6) && (index[1]<=12) )
        {
        node = im->AddNode (index);
        node->m_Value = cnt++;
        }
      }

  typedef SparseImageType::NodeListType NodeListType;
  NodeListType::Pointer nodelist = im->GetNodeList();
  nodelist->Print(std::cout);
  im->Print(std::cout);
  im->Initialize();
  nodelist = im->GetNodeList();
  nodelist->Print(std::cout);
  im->Print(std::cout);
  
  return EXIT_SUCCESS;
}
