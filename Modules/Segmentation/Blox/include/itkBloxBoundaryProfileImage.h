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
#ifndef __itkBloxBoundaryProfileImage_h
#define __itkBloxBoundaryProfileImage_h

#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxBoundaryPointImage.h"

#include "itkBloxBoundaryProfilePixel.h"
#include "itkImage.h"

namespace itk
{
/** \class BloxBoundaryProfileImage
 * \brief N-dimensional image class which handles BloxBoundaryProfileItems
 *
 * A BloxBoundaryProfileImage stores and processes BloxBoundaryProfileItem's
 * (in BloxPixel linked lists). The primary task of BloxBoundaryProfileImage
 * is finding boundary profilesand storing them in the correct blox location.
 * \ingroup ImageObjects
 * \ingroup ITK-Blox
 */
template< unsigned int TImageDimension >
class ITK_EXPORT BloxBoundaryProfileImage:
  public BloxImage< BloxBoundaryProfilePixel< TImageDimension >, TImageDimension >
{
public:

  /** Standard image class typedefs. */
  typedef BloxBoundaryProfileImage Self;
  typedef BloxImage< BloxBoundaryProfilePixel< TImageDimension >,
                     TImageDimension >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  itkNewMacro(Self);

  itkTypeMacro(BloxBoundaryProfileImage, BloxImage);

  typedef BloxBoundaryProfilePixel< TImageDimension > PixelType;
  typedef PixelType                                   InternalPixelType;

  typedef DefaultPixelAccessor< PixelType > AccessorType;

  typedef Point< double, TImageDimension > PositionType;

  typedef typename PositionType::VectorType VectorType;

  typedef CovariantVector< double, TImageDimension > GradientType;

  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename Superclass::SizeType       SizeType;
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::OffsetType     OffsetType;
  typedef typename Superclass::RegionType     RegionType;

  typedef typename PixelContainer::Pointer PixelContainerPointer;
protected:
  BloxBoundaryProfileImage();
  virtual ~BloxBoundaryProfileImage();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  BloxBoundaryProfileImage(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxBoundaryProfileImage(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                            \
  {                                                                        \
  _( 1 ( class EXPORT BloxBoundaryProfileImage< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                                      \
  {                                                                        \
  typedef BloxBoundaryProfileImage< ITK_TEMPLATE_1 TypeX >                 \
  BloxBoundaryProfileImage##TypeY;                                       \
  }                                                                        \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkBloxBoundaryProfileImage+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBloxBoundaryProfileImage.txx"
#endif

#endif
