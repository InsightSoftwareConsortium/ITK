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
#ifndef __itkBloxCoreAtomImage_h
#define __itkBloxCoreAtomImage_h

#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_vector.h"
#include "itkVector.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxCoreAtomPixel.h"

namespace itk
{
/** \class BloxCoreAtomImage
 * \brief N-dimensional image class which handles BloxCoreAtomItems
 *
 * A BloxCoreAtomImage stores and processes BloxCoreAtomItem's (in BloxPixel
 * linked lists). The primary task of BloxCoreAtomImage is finding core atoms
 * and storing them in the correct blox location.
 * \ingroup ImageObjects
 * \ingroup ITK-Blox
 */
template< unsigned int NDimension >
class ITK_EXPORT BloxCoreAtomImage:
  public BloxImage< BloxCoreAtomPixel< NDimension >, NDimension >
{
public:
  /** Standard class typedefs. */
  typedef BloxCoreAtomImage Self;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  itkStaticConstMacro(NDimensions, unsigned int, NDimension);

  typedef BloxImage< BloxCoreAtomPixel< NDimension >, NDimension > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef WeakPointer< const Self >  ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BloxCoreAtomImage, BloxImage);

  /** The type of boundary point item we process * */
  typedef BloxBoundaryPointItem< NDimension > BPItemType;

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef BloxCoreAtomPixel< NDimension > PixelType;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external
   * representation. */
  typedef PixelType InternalPixelType;

  /** Accessor type that convert data between internal and external
   * representations. */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /** The type of Point used to convert between physical and blox space */
  typedef Point< double, NDimension > PositionType;

  /** The vector between two points */
  typedef typename PositionType::VectorType VectorType;

  /** How we represent gradients. */
  typedef CovariantVector< double, NDimension > GradientType;

  /** get macro for m_MedialNodeCount. */
  itkGetConstMacro(MedialNodeCount, int);

  typedef std::vector< PixelType * > NodePointerListType;

  typedef std::vector< PixelType * > *NodePointerListPointer;

  /** get macro for m_NodePointerList. */
  itkGetConstMacro(NodePointerList, NodePointerListPointer);

  /** Convenient typedefs obtained from Superclass.
   * Note: Unlike "normal" images BloxCoreAtomImages support neither Scalar nor
   * Vector calls!!! Scalar and vector traits are not defined and do not
   * make sense for linked lists (at the present time). */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename Superclass::SizeType       SizeType;
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::OffsetType     OffsetType;
  typedef typename Superclass::RegionType     RegionType;

  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Do eigenanalysis on all pixels in the image. */
  void DoEigenanalysis();

  /** Core atom voting routine. */
  void DoCoreAtomVoting();

protected:
  BloxCoreAtomImage();
  virtual ~BloxCoreAtomImage();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  BloxCoreAtomImage(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented

  /** The number of medial nodes found. */
  int m_MedialNodeCount;

  /** List of pointers to all the medial nodes. */
  NodePointerListPointer m_NodePointerList;
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxCoreAtomImage(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                     \
  {                                                                 \
  _( 1 ( class EXPORT BloxCoreAtomImage< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                               \
  {                                                                 \
  typedef BloxCoreAtomImage< ITK_TEMPLATE_1 TypeX >                 \
  BloxCoreAtomImage##TypeY;                                       \
  }                                                                 \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkBloxCoreAtomImage+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBloxCoreAtomImage.txx"
#endif

#endif
