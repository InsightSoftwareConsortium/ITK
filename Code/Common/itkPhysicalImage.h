/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhysicalImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * traits class.  The container for the pixel data is specified as a
 * "PixelContainer" member of the image traits, and must satisfy the
 * ImageContainerInterface.  The default pixel container uses a
 * valarray to store pixel values (ValarrayImageContainer).  An
 * alternative pixel container is ImportImageFilterContainer, which
 * allows an application to pass a block of memory to the Image class
 * to use as its initial storage.
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

template <class TPixel, unsigned int VImageDimension=2,
          class TImageTraits = DefaultImageTraits<TPixel, VImageDimension> >
class ITK_EXPORT PhysicalImage : public Image<TPixel, VImageDimension, TImageTraits>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PhysicalImage               Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Image<TPixel, VImageDimension, TImageTraits>  Superclass;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef TPixel PixelType;

  /** 
   * Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation.
   */
  typedef TPixel InternalPixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef DefaultDataAccessor< PixelType > AccessorType;

  /** 
   * Pixel (scalar) value typedef support. The scalar value is the native
   * type that the scalar portion of the pixels are composed of; usually 
   * something like float, int, etc. ScalarTraits typically needs to be
   * specialized for a given PixelType. If a PixelType does not have
   * scalar values, ScalarTraits provides an opportunity to define what
   * the scalar type would be if the PixelType had scalars.  For instance,
   * the ScalarValueType for a PixelType that only has a vector may be
   * defined to match the vector value type.
   */
  typedef typename ScalarTraits<TPixel>::ValueType ScalarValueType;

  /** 
   * Pixel (vector) value typedef support. The vector value is the native
   * type that the vector portion of the pixels are composed of; usually 
   * something like float, int, etc. VectorTraits typically needs to be
   * specialized for a given PixelType. If a PixelType does not have
   * vector values, VectorTraits provides an opportunity to define what
   * the vector value type would be if the PixelType had a vector.  For
   * instance, the VectorValueType for a PixelType that only has a scalar may
   * be defined to match the vector value type.
   */
  typedef typename VectorTraits<TPixel>::ValueType VectorValueType;

  /**
   * Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image.
   */
  enum { ImageDimension = VImageDimension };

  /**
   * The ImageTraits for this image.
   */
  typedef TImageTraits ImageTraits;

  /*@{
   * Convenient typedefs obtained from TImageTraits template parameter.
   */
  typedef typename ImageTraits::PixelContainer PixelContainer;
  typedef typename ImageTraits::SizeType SizeType;
  typedef typename ImageTraits::IndexType IndexType;
  typedef typename ImageTraits::OffsetType OffsetType;
  typedef typename ImageTraits::RegionType RegionType;
  //@}

  /**
   * A pointer to the pixel container.
   */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** 
   * Typedef for associated AffineTransform
   *
   * This is used specifically as the type of the index-to-physical and
   * physical-to-index transforms associated with the origin and spacing
   * for the image, and more generally as any affine transformation of
   * the image.
   */
  typedef AffineTransform<double, ImageDimension> AffineTransformType;

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
  virtual void SetSpacing( const double values[ImageDimension] );
  virtual void SetSpacing( const float values[ImageDimension] );

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
  virtual void SetOrigin( const double values[ImageDimension] );
  virtual void SetOrigin( const float values[ImageDimension] );

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
  AffineTransformType GetIndexToPhysicalTransform(void) const;

  /** 
   * Get the physical-to-index coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from physical coordinates to index coordinates
   * determined by the origin and spacing of this image.
   */
  AffineTransformType GetPhysicalToIndexTransform(void) const;

protected:
  PhysicalImage();
  virtual ~PhysicalImage();
  PhysicalImage(const Self&) {}
  void operator=(const Self&) {}
  
private:

  double              m_Spacing[ImageDimension];
  double              m_Origin[ImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhysicalImage.txx"
#endif

#endif

