/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhysicalImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPhysicalImage_h
#define __itkPhysicalImage_h

#include "itkImage.h"
#include "itkAffineTransform.h"

namespace itk
{

/**
 * \class PhysicalImage
 * \brief Templated n-dimensional image class.
 *
 * Images are templated over a pixel type (modeling the dependent
 * variables), a dimension (number of independent variables) and a
 * container type for the pixel data. The container for the pixel data
 * must satisfy the ImageContainerInterface.  The default pixel container
 * uses a valarray to store pixel values (ValarrayImageContainer).  An
 * alternative pixel container is ImportImageFilterContainer, which allows an
 * application to pass a block of memory to the Image class to use as its
 * initial storage.
 *
 * Within, the pixel container, images are modeled as arrays, defined by a
 * start index and a size.
 *
 * There are three sets of meta-data describing an image. These are "Region"
 * objects that define a portion of an image via a starting index for the image
 * array and a size. The ivar LargestPossibleRegion defines the size and
 * starting index of the image dataset. The entire image dataset, however,
 * may not resident in memory. The region of the image that is resident in
 * memory is defined by the "BufferedRegion". The Buffer is a contiguous block
 * of memory.  The third set of meta-data defines a region of interest, called
 * the "RequestedRegion". The RequestedRegion is used by the pipeline
 * execution model to define what a filter is requested to produce. 
 *
 * [RegionIndex, RegionSize] C [BufferIndex, BufferSize]
 *                           C [ImageIndex, ImageSize]
 *
 * Pixels can be accessed direcly using the SetPixel() and GetPixel()
 * methods or can be accessed via iterators.  Begin() creates
 * an iterator that can walk a specified region of a buffer.
 *
 * The pixel type may be one of the native types; or it can be a 
 * Insight-defined class type such as Scalar or Vector; or a
 * user-defined type. Note that depending on the type of pixel that you use,
 * the process objects (i.e., those filters processing data objects), may not
 * operate on the image and/or pixel type. This becomes apparant at 
 * compile-time either because operator overloading (for the pixel type) is 
 * not supported; or the filter may only process scalars (meaning it supports 
 * the GetScalar() method), or vectors (supports GetVector()).  
 *
 * The data in an image is arranged in a 1D array as [][][][slice][row][col]
 * with Index[0] = col, Index[1] = row, Index[2] = slice, ...
 *
 * Accessing an image pixel through the corresponding index values is
 * referred as using index coordinates.  The image class also includes
 * origin and spacing fields which allow the user to define physical
 * coordinates within an image.  Along axis i, the pixel with index
 * coordinate k[i] has the physical coordinate x[i] = origin[i] +
 * k[i]*spacing[i].
 *
 *
 * \sa ImageContainerInterface
 * */

template <class TPixel, unsigned int VImageDimension=2, class TPixelContainer=ValarrayImageContainer<unsigned long, TPixel> >
class ITK_EXPORT PhysicalImage : public Image<TPixel, VImageDimension, TPixelContainer>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PhysicalImage               Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Image<TPixel, VImageDimension, TPixelContainer>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Typedef for associated AffineTransform
   *
   * This is used specifically as the type of the index-to-physical and
   * physical-to-index transforms associated with the origin and spacing
   * for the image, and more generally as any affine transformation of
   * the image.
   */
  typedef AffineTransform<double, VImageDimension> AffineTransformType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(PhysicalImage, Image);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Set the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float.
   * \sa GetSpacing()
   */
  virtual void SetSpacing( const double values[VImageDimension] );
  virtual void SetSpacing( const float values[VImageDimension] );

  /** 
   * Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * \sa SetSpacing()
   */
  virtual const double* GetSpacing() const;
  
  /** 
   * Set the origin of the image. The origin is the geometric
   * coordinates of the image origin.  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin()
   */
  virtual void SetOrigin( const double values[VImageDimension] );
  virtual void SetOrigin( const float values[VImageDimension] );

  /** 
   * Get the origin of the image. The origin is the geometric
   * coordinates of the image origin.  The value returned is
   * a pointer to a double array.
   * \sa SetOrigin()
   */
  virtual const double * GetOrigin() const;

  /** 
   * Get the index-to-physical coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from index coordinates to physical coordinates
   * determined by the origin and spacing of this image.
   */
  AffineTransformType GetIndexToPhysicalTransform();

  /** 
   * Get the physical-to-index coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from physical coordinates to index coordinates
   * determined by the origin and spacing of this image.
   */
  AffineTransformType GetPhysicalToIndexTransform();

protected:
  PhysicalImage();
  virtual ~PhysicalImage();
  PhysicalImage(const Self&) {}
  void operator=(const Self&) {}
  
private:

  double              m_Spacing[VImageDimension];
  double              m_Origin[VImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhysicalImage.txx"
#endif

#endif

