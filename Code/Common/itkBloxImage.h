/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxImage_h
#define __itkBloxImage_h

#include "itkBloxPixel.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class BloxImage
 * \brief Templated n-dimensional image class used to store linked lists.
 *
 * The itk::BloxImage object is a regular, rectilinear lattice of "blocks" in 
 * n-dimensional space.  The word "blox" was chosen to bring to mind a set of
 * "city blocks" in 2D or "building blocks" in 3D.  Being a regular lattice,
 * itkBloxImage logically derives from itkImage.  In an itkBloxImage, each
 * pixel represents an isometric space-filling block of geometric space, called
 * an itkBloxPixel.  Each itk::BloxPixel generally covers many pixels in the
 * underlying image and is used to store a variable number of image primitives
 * (such as boundary points) or features (such as medial nodes) gathered within
 * that region of geometric space.  To do this, each itkBloxPixel contains a 
 * linked list.
 *
 * The itk::BloxImage object facilitates certain forms of analysis by providing
 * geometric hashing.  For example, if boundary points are stored in an 
 * itk::BloxImage, pairs of boundary points that face each other 
 * (called "core atoms") can be found by searching relatively small regions of 
 * geometric space that face each boundary point for appropriate mates.  
 * Because an itk::BloxImage is rectilinear in geometric space (even though the
 * underlying image may not be) subsequent analysis can be invariant to 
 * rotation and translation.
 *
 * itk::BloxImage is templated over pixel type; in general, the pixel type 
 * should be derived from itk::BloxPixel, however this is not enforced and 
 * other implementations are possible.
 *
 * Note that itk::BloxPixel is itself templated over item type (the type of 
 * object stored in the linked list).
 *
 * \ingroup ImageObjects
 *
 */
template <typename TBloxPixelType, unsigned int TImageDimension=3>
class ITK_EXPORT BloxImage : public Image<TBloxPixelType, TImageDimension>
{
public:
  /** Standard class typedefs. */
  typedef BloxImage                               Self;
  typedef Image<TBloxPixelType, TImageDimension>  Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
  typedef WeakPointer<const Self>                 ConstWeakPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(BloxImage, Image);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef TBloxPixelType PixelType;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation. */
  typedef TBloxPixelType InternalPixelType;

  typedef typename Superclass::IOPixelType   IOPixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /** Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImageDimension);

  /** Convenient typedefs obtained from Superclass. */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename Superclass::SizeType       SizeType;
  typedef typename Superclass::IndexType      IndexType;
  typedef typename Superclass::OffsetType     OffsetType;
  typedef typename Superclass::RegionType     RegionType;
  
  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Traverse the entire image and empty all linked lists.
   * This is used in filters prior to generating new data, to
   * avoid appending the new data onto the old
   */
  void EmptyImage();

protected:
  BloxImage();
  virtual ~BloxImage();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BloxImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxImage(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT BloxImage< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef BloxImage< ITK_TEMPLATE_2 x > \
                               BloxImage##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkBloxImage+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkBloxImage.txx"
#endif

#endif
