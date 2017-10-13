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
#ifndef itkSparseImage_h
#define itkSparseImage_h

#include "itkImage.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"

namespace itk
{
/**
 * \class SparseImage
 *
 * \brief A storage type for sparse image data.
 *
 * \par
 * This class is derived from the Image class. It uses the base class image
 * data for storing pointers to variables of type TNode. The node type must
 * have a member variable m_Index. The node data is
 * stored using the SparseFieldLayer and ObjectStore classes to allow
 * sequential list access to the nodes. This functionality is used in filter
 * classes that process the SparseImage class such as
 * FiniteDifferenceSparseImageFilter. The node type must also have members
 * NodeType* Next and NodeType* Previous. A minimal node class which could
 * be used to create the sparse equivalent of an itk::Image<unsigned char, 2>
 * is shown below:
 *
 * \code
 * struct NodeType
 * {
 * NodeType* Next;
 * NodeType* Previous;
 * ImageType::IndexType m_Index;
 * unsigned char m_Data;
 * };
 * typedef itk::SparseImage<NodeType, 2> SparseImageType;
 * \endcode
 *
 * \par
 * This class provides the method AddNode which allocates a node variable,
 * associates it with the image pixel index (sets m_Index in the node variable)
 * and returns the pointer to the node variable. It is suggested that the user
 * call the FillBuffer method to initialize the image to null pointers before
 * any calls to AddNode. This would allow the user later to distinguish between
 * valid and non-valid pixels.
 *
 * \ingroup ITKCommon
 */

template< typename TNode, unsigned int VImageDimension = 2 >
class ITK_TEMPLATE_EXPORT SparseImage:public Image< TNode *, VImageDimension >
{
public:
  /** Standard typedefs. */
  typedef SparseImage                       Self;
  typedef Image< TNode *, VImageDimension > Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;
  typedef WeakPointer< const Self >         ConstWeakPointer;

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

  /** Tyepdef for the functor used to access a neighborhood of pixel
   * pointers. */
  typedef NeighborhoodAccessorFunctor< Self >
  NeighborhoodAccessorFunctorType;

  typedef typename Superclass::IOPixelType IOPixelType;

  /** The list types for storing the active pixels. */
  typedef SparseFieldLayer< NodeType > NodeListType;
  typedef ObjectStore< NodeType >      NodeStoreType;

  /** Return the NeighborhoodAccessor functor. This method is called by the
   * neighborhood iterators. */
  NeighborhoodAccessorFunctorType GetNeighborhoodAccessor()
  { return NeighborhoodAccessorFunctorType(); }

  /** Return the NeighborhoodAccessor functor. This method is called by the
   * neighborhood iterators. */
  const NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() const
  { return NeighborhoodAccessorFunctorType(); }

  /** This function should be used to allocate memory for a variable at the
      desired pixel location. */
  NodeType * AddNode(const IndexType & index)
  {
    m_NodeList->PushFront( m_NodeStore->Borrow() );
    NodeType *node = m_NodeList->Front();
    node->m_Index = index;
    this->SetPixel(index, node);
    return node;
  }

  /** This function returns the allocated node list which can be used to
      iterate through the valid nodes. */
  NodeListType * GetNodeList()
  {
    return m_NodeList;
  }

  /** This function initializes the m_NodeList and m_NodeStore variables, and
      calls the superclass Initialize method. */
  virtual void Initialize() ITK_OVERRIDE;

protected:
  SparseImage();
  ~SparseImage() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  /** The variables for storing the node variables. */
  typename NodeListType::Pointer m_NodeList;

  typename NodeStoreType::Pointer m_NodeStore;

  ITK_DISALLOW_COPY_AND_ASSIGN(SparseImage);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseImage.hxx"
#endif

#endif
