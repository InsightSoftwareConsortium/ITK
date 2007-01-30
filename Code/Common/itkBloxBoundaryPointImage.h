/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryPointImage_h
#define __itkBloxBoundaryPointImage_h

#include "itkPoint.h"
#include "itkBloxBoundaryPointPixel.h"
#include "itkBloxImage.h"

namespace itk
{

/**
 * \class BloxBoundaryPointImage
 * \brief Templated n-dimensional image class used to store linked lists.
 * \ingroup ImageObjects
 *
 */
template <unsigned int TImageDimension>
class ITK_EXPORT BloxBoundaryPointImage :
  public BloxImage< BloxBoundaryPointPixel<TImageDimension >, TImageDimension >
{
public:
  /** Standard class typedefs. */
  typedef BloxBoundaryPointImage       Self;
  typedef BloxImage<BloxBoundaryPointPixel<TImageDimension>,
                    TImageDimension >  Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BloxBoundaryPointImage, BloxImage);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef BloxBoundaryPointPixel<TImageDimension> PixelType;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation. */
  typedef PixelType InternalPixelType;

  typedef typename Superclass::IOPixelType   IOPixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /** Convenient typedefs obtained from Superclass. */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename Superclass::SizeType       SizeType;
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::OffsetType     OffsetType;
  typedef typename Superclass::RegionType     RegionType;
  
  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Set the number of boundary points in the image (done by a filter) */
  itkSetMacro(NumBoundaryPoints, unsigned long int);

  /** Get the number of boundary points in the image */
  itkGetConstReferenceMacro(NumBoundaryPoints, unsigned long int);

protected:
  BloxBoundaryPointImage();
  virtual ~BloxBoundaryPointImage();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BloxBoundaryPointImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The total number of boundary points stored in the image */
  unsigned long int m_NumBoundaryPoints;
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxBoundaryPointImage(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT BloxBoundaryPointImage< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef BloxBoundaryPointImage< ITK_TEMPLATE_1 x > \
                                            BloxBoundaryPointImage##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkBloxBoundaryPointImage+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkBloxBoundaryPointImage.txx"
#endif

#endif
