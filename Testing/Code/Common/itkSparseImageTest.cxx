#include "itkImage.h"
#include "itkSparseImage.h"
#include <iostream>

/* This test exercises the itkSparseImage class. */

const unsigned int HEIGHT = (24);
const unsigned int WIDTH  = (24);
const int LOW    = (6);
const int HIGH   = (12);

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
  ImageType::SizeType   sz = {{HEIGHT, WIDTH}};
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
  
  for ( index[0]=0; index[0]<WIDTH; index[0]++ )
    for ( index[1]=0; index[1]<HEIGHT; index[1]++ )
      {
      if ( (index[0]>=LOW) && (index[0]<=HIGH) &&
           (index[1]>=LOW) && (index[1]<=HIGH) )
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
  
  return 0;
}
