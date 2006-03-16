/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryProfileImage_h
#define __itkBloxBoundaryProfileImage_h

#include "vnl/vnl_vector_fixed.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxBoundaryPointPixel.h"
#include "itkBloxBoundaryPointImage.h"

#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryProfilePixel.h"
#include "itkBloxImage.h"
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
 */
template <unsigned int TImageDimension>
class ITK_EXPORT BloxBoundaryProfileImage :
public BloxImage<BloxBoundaryProfilePixel<TImageDimension>, TImageDimension>
{
public:

  /** Standard image class typedefs. */
  typedef BloxBoundaryProfileImage     Self;
  typedef BloxImage<BloxBoundaryProfilePixel<TImageDimension>,
                    TImageDimension >  Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
  
  itkNewMacro(Self);

  itkTypeMacro(BloxBoundaryProfileImage, BloxImage);

  typedef BloxBoundaryProfilePixel<TImageDimension> PixelType;
  typedef PixelType                                 InternalPixelType;

  typedef DefaultPixelAccessor< PixelType > AccessorType;

  typedef Point<double, TImageDimension> PositionType;

  typedef typename PositionType::VectorType VectorType;

  typedef CovariantVector<double, TImageDimension> GradientType;

  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename Superclass::SizeType       SizeType;
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::OffsetType     OffsetType;
  typedef typename Superclass::RegionType     RegionType;
  
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
protected:
  BloxBoundaryProfileImage();
  virtual ~BloxBoundaryProfileImage();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BloxBoundaryProfileImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfileImage.txx"
#endif

#endif
