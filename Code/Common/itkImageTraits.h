/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkImageTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageTraits_h
#define __itkImageTraits_h


namespace itk
{

/**
 * \class ImageTraits
 * \brief Helper class for managing the template arguments to objects
 * that are templated over image type, i.e. ImageAdaptor.
 *
 * ImageTraits is a helper class which "copies" typedef and
 * enum information from its template parameter into its own class. This
 * class is used by classes that are templated over image type, providing
 * a means to access this information during the declaration of a templated
 * class. For instance, ImageAdaptor uses this class in its declaration
 * to extract the ImageDimension from its template parameter to use as a
 * template parameter for its superclass.
 *
 * template <class TImage, class TAccessor >
 * class ITK_EXPORT ImageAdaptor : public ImageBase<ImageTraits<TImage>::ImageDimension> {};
 */
template<class TImage>
struct ITK_EXPORT ImageTraits
{
  /**
   * Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image.
   */
  enum {ImageDimension = TImage::ImageDimension};

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef typename TImage::PixelType PixelType;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef typename TImage::InternalPixelType InternalPixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef typename TImage::AccessorType   AccessorType;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef typename TImage::IndexType  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef typename TImage::SizeType  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef typename TImage::RegionType  RegionType;

  /** 
   * PixelContainer typedef support. Used to construct a container for
   * the pixel data.
   */
  typedef typename TImage::PixelContainer        PixelContainer;
  typedef typename TImage::PixelContainerPointer PixelContainerPointer;
};

  
} // end namespace itk

  
  

#endif

