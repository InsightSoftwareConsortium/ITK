/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSparseImage_h_
#define __itkSparseImage_h_

#include "itkImage.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"

namespace itk {
 
/**
 * \class SparseImage
 *
 * \brief This class implements a storage type for sparse image data.
 *
 * \par
 * This class is derived from the Image class. It uses the base class image
 * data for storing pointers to variables of type TNode. The node type must
 * have a member variable m_Index. The node data is
 * stored using the SparseFieldLayer and ObjectStore classes to allow
 * sequential list access to the nodes. This functionality is used in filter
 * classes that process the SparseImage class such as
 * FiniteDifferenceSparseImageFilter.
 *
 * \par
 * This class provides the method AddNode which allocates a node variable,
 * associates it with the image pixel index (sets m_Index in the node variable)
 * and returns the pointer to the node variable. It is suggested that the user
 * call the FillBuffer method to initialize the image to null pointers before
 * any calls to AddNode. This would allow the user later to distinguish between
 * valid and non-valid pixels.
 *
 */

template <class TNode, unsigned int VImageDimension=2>
class ITK_EXPORT SparseImage : public Image <TNode*, VImageDimension>
{
  public:
  /** Standard typedefs. */
  typedef SparseImage Self;
  typedef Image <TNode*, VImageDimension> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SparseImage, Image);

  /** Dimension of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** The actual sparse pixel type. */
  typedef TNode NodeType;

  /** Types derived from the Superclass */
  typedef typename Superclass::IndexType IndexType;
  
  /** The list types for storing the active pixels.*/
  typedef SparseFieldLayer <NodeType> NodeListType;
  typedef ObjectStore      <NodeType> NodeStoreType;

  /** This function should be used to allocate memory for a variable at the
      desired pixel location. */
  NodeType *AddNode(const IndexType &index)
  {
    m_NodeList->PushFront(m_NodeStore->Borrow());
    NodeType *node = m_NodeList->Front();
    node->m_Index=index;
    this->SetPixel(index,node);
    return node;
  }

  /** This function returns the allocated node list which can be used to
      iterate through the valid nodes. */
  NodeListType* GetNodeList() 
  {
   return m_NodeList;
  }

  /** This function initializes the m_NodeList and m_NodeStore variables, and
      calls the superclass Initialize method. */
  virtual void Initialize();
  
protected:
  SparseImage();
  ~SparseImage();
  
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  /** The variables for storing the node variables. */
  typename NodeListType::Pointer     m_NodeList;
  typename NodeStoreType::Pointer    m_NodeStore;
  
  SparseImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseImage.txx"
#endif

#endif
