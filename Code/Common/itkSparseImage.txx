#ifndef __itkSparseImage_txx_
#define __itkSparseImage_txx_

#include "itkSparseImage.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"
#include "itkDataObject.h"

namespace itk {

template <class TNode, unsigned int VImageDimension>
SparseImage < TNode, VImageDimension >
::SparseImage ()
{
  m_NodeList     = NodeListType::New();
  m_NodeStore    = NodeStoreType::New();
}

template <class TNode, unsigned int VImageDimension>
SparseImage < TNode, VImageDimension >
::~SparseImage ()
{
  // this might not be necessary
  while (!m_NodeList->Empty())
    {
    m_NodeStore->Return(m_NodeList->Front());
    m_NodeList->PopFront();
    } 
  }
  
template <class TNode, unsigned int VImageDimension>
void
SparseImage < TNode, VImageDimension >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <class TNode, unsigned int VImageDimension>
void
SparseImage < TNode, VImageDimension >
::Initialize ()
{
  Superclass::Initialize();
  m_NodeList     = NodeListType::New();
  m_NodeStore    = NodeStoreType::New();
}

} // end namespace itk

#endif
