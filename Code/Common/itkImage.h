/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.h
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
#ifndef __itkImage_h
#define __itkImage_h

#include "itkImageBase.h"
#include "itkDefaultImageTraits.h"
#include "itkDefaultPixelAccessor.h"
#include "itkAffineTransform.h"
#include "itkPoint.h"
#include "itkContinuousIndex.h"

namespace itk
{

/** \class Image
 *  \brief Templated n-dimensional image class.
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
 * need not be resident in memory. The region of the image that is resident in
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
 * The pixel type may be one of the native types; a Insight-defined
 * class type such as Vector; or a user-defined type. Note that
 * depending on the type of pixel that you use, the process objects
 * (i.e., those filters processing data objects) may not operate on
 * the image and/or pixel type. This becomes apparent at compile-time
 * because operator overloading (for the pixel type) is not supported.
 *
 * The data in an image is arranged in a 1D array as [][][][slice][row][col]
 * with the column index varying most rapidly.  The Index type reverses
 * the order so that with Index[0] = col, Index[1] = row, Index[2] = slice, 
 * ...
 *
 * \sa ImageContainerInterface
 *
 * \ingroup ImageObjects
 */
template <class TPixel, unsigned int VImageDimension=2,
          class TImageTraits=DefaultImageTraits< TPixel, VImageDimension > >
class ITK_EXPORT Image : public ImageBase<VImageDimension>
{
public:
  /** Standard class typedefs */
  typedef Image               Self;
  typedef ImageBase<VImageDimension>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(Image, ImageBase);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef TPixel PixelType;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation. */
  typedef TPixel InternalPixelType;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /** Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  enum { ImageDimension = VImageDimension };
  
  /** The ImageTraits for this image. */
  typedef TImageTraits ImageTraits;

  /** Convenient typedefs obtained from TImageTraits template parameter. */
  typedef typename ImageTraits::PixelContainer PixelContainer;
  typedef typename ImageTraits::SizeType SizeType;
  typedef typename ImageTraits::IndexType IndexType;
  typedef typename ImageTraits::OffsetType OffsetType;
  typedef typename ImageTraits::RegionType RegionType;
  
  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Typedefs for the associated AffineTransform.* This is used
   * specifically as the type of the index-to-physical and physical-to-index
   * transforms associated with the origin and spacing for the image, and
   * more generally as any affine transformation of the image. */
  typedef AffineTransform<double, ImageDimension> AffineTransformType;
  typedef typename AffineTransformType::Pointer   AffineTransformPointer;
  
  /** Definition of the point type used for setting the origin */
  typedef typename AffineTransformType::OffsetType OriginOffsetType;

  /** Allocate the image memory. The image Dimension and Size must 
   * already been set. */
  void Allocate();

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize();

  /** \brief Set a pixel value.
   * 
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  void SetPixel(const IndexType &index, const TPixel& value)
    {
    OffsetValueType offset = this->ComputeOffset(index);
    (*m_Buffer)[offset] = value;
    }
  
  /** \brief Get a pixel (read only version).
   * 
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel& GetPixel(const IndexType &index) const
  {
    OffsetValueType offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }

  /** \brief Get a reference to a pixel (e.g. for editing).
   * 
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel& GetPixel(const IndexType &index)
  {
    OffsetValueType offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }
    
  /** \brief Access a pixel. This version can be an lvalue.
   * 
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel & operator[](const IndexType &index)
     { return this->GetPixel(index); }
  
  /** \brief Access a pixel. This version can only be an rvalue.
   * 
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel& operator[](const IndexType &index) const
     { return this->GetPixel(index); }

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  TPixel *GetBufferPointer()
    { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; }
  const TPixel *GetBufferPointer() const
    { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; }
  
  /** Return a pointer to the container. */
  PixelContainerPointer GetPixelContainer()
    { return m_Buffer; }

  /** Set the container to use. Note that this does not cause the
   * DataObject to be modified. */
  void SetPixelContainer( PixelContainer *container );
  
  /** Return the Pixel Accessor object */
  AccessorType & GetPixelAccessor( void ) 
    { return m_PixelAccessor; }
    
  /** Return the Pixel Accesor object */
  const AccessorType & GetPixelAccessor( void ) const
    { return m_PixelAccessor; }
    
  /** Set the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float.
   * \sa GetSpacing() */
  virtual void SetSpacing( const double values[ImageDimension] );
  virtual void SetSpacing( const float values[ImageDimension] );
  
  /** Get the spacing (size of a pixel) of the image. The
   * spacing is the geometric distance between image samples.
   * The value returned is a pointer to a double array.
   * \sa SetSpacing() */
  virtual const double* GetSpacing() const;
  
  /** Set the origin of the image. The origin is the geometric
   * coordinates of the image origin.  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  virtual void SetOrigin( const double values[ImageDimension] );
  virtual void SetOrigin( const float values[ImageDimension] );
  virtual void SetOrigin( const OriginOffsetType & origin );
  
  /** Get the origin of the image. The origin is the geometric
   * coordinates of the index (0,0).  The value returned is
   * a pointer to a double array.
   * \sa SetOrigin() */
  virtual const double * GetOrigin() const;

  /** Get the index-to-physical coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from index coordinates to physical coordinates
   * determined by the origin and spacing of this image. */
  AffineTransformPointer GetIndexToPhysicalTransform(void);

  /** Get the physical-to-index coordinate transformation
   *
   * This method returns an AffineTransform which defines the
   * transformation from physical coordinates to index coordinates
   * determined by the origin and spacing of this image. */
  AffineTransformPointer GetPhysicalToIndexTransform(void);

  /** Gets and sets for affine transforms. */
  itkSetMacro(IndexToPhysicalTransform, AffineTransformPointer);
  itkSetMacro(PhysicalToIndexTransform, AffineTransformPointer);
    
  /** Rebuild affine transforms based on origin and spacing */
  void RebuildTransforms();

  /** \brief Get the continuous index from a physical point
   *
   * Returns true if the resulting index is within the image, false otherwise.
   * Since this function internally uses AffineTranform, it is
   * templated over coordinate value type (TCoordRep); using float or
   * double for the coordinate value type is recommended.
   * \todo
   * In future, when MS Visual C++ supports out-of-class member templates,
   * move function definition to itkImage.txx file.
   * \sa AffineTransform */
  template<class TCoordRep> 
  bool TransformPhysicalPointToContinuousIndex(Point<TCoordRep, VImageDimension>& point, 
    ContinuousIndex<TCoordRep, VImageDimension>& index)
    {
    if ( !m_PhysicalToIndexTransform ) {this->RebuildTransforms();}
    
    AffineTransformType::InputPointType inputPoint = 
      m_PhysicalToIndexTransform->TransformPoint(point) ;

    for (unsigned int i = 0 ; i < VImageDimension ; i++)
      {index[i] = inputPoint[i];}

    // Now, check to see if the index is within allowed bounds
    // Get the image size
    SizeType sizeObject = this->GetLargestPossibleRegion().GetSize();
    const unsigned long* mySize = sizeObject.GetSize();

    // Check to see if the index is valid
    for (int ii = 0; ii < VImageDimension; ++ii)
      {
      if( (index[ii] < 0) || (index[ii] >= mySize[ii]) )
        { return false; }
      }
    
    // If we make it to here, then the the index is a valid one
    return true;
    }

  /** Get the index (discrete) from a physical point.
   * Floating point index results are truncated to integers.
   * Returns true if the resulting index is within the image, false otherwise
   * Since this function internally uses AffineTranform, it is
   * templated over coordinate value type (TCoordRep); using float or
   * double for the coordinate value type is recommended.
   * \todo
   * In future, when MS Visual C++ supports out-of-class member templates,
   * move function definition to itkImage.txx file.
   * \sa AffineTransform */
  template<class TCoordRep> 
  bool TransformPhysicalPointToIndex(Point<TCoordRep, VImageDimension>& point, 
    Index<VImageDimension>& index)
    {

    // If no current transforms exist, rebuild using the origin and spacing
    if ( !m_PhysicalToIndexTransform ) { this->RebuildTransforms(); }

    // Transform the point
    AffineTransformType::InputPointType inputPoint =
      m_PhysicalToIndexTransform->TransformPoint(point) ;

    // Update the output index
    for (unsigned int i = 0 ; i < VImageDimension ; i++)
      { index[i] = static_cast<long>(inputPoint[i]); }
    
    // Now, check to see if the index is within allowed bounds
    // Get the image size
    SizeType sizeObject = this->GetLargestPossibleRegion().GetSize();
    const unsigned long* mySize = sizeObject.GetSize();

    // Check to see if the index is valid
    for (unsigned int ii = 0; ii < VImageDimension; ++ii)
      {
      if( (index[ii] < 0) || (index[ii] >= static_cast<long>(mySize[ii])) )
        { return false; }
      }
    
    // If we make it to here, then the the index is a valid one
    return true;
    }

  /** Get a physical point (in the space which 
   * the origin and spacing infomation comes from) 
   * from a continuous index (in the index space) 
   *
   * Since this function internally uses AffineTranform, it is
   * templated over coordinate value type (TCoordRep); using float or
   * double for the coorinate value type is recommended.
   *
   * \todo In future, when MS Visual C++ supports out-of-class member 
   * templates, move function definition to itkImage.txx file.
   * \sa AffineTransform */
  template<class TCoordRep> 
  void TransformContinuousIndexToPhysicalPoint(ContinuousIndex<TCoordRep, VImageDimension>& index, 
    Point<TCoordRep, VImageDimension>& point)
    {
    // If no current transforms exist, rebuild using the origin and spacing
    if ( !m_IndexToPhysicalTransform ) { this->RebuildTransforms(); }
    
    AffineTransformType::InputPointType inputPoint;

    // Update the input index
    for (unsigned int i = 0 ; i < VImageDimension ; i++)
      { inputPoint[i] = index[i]; }

    // Transform the point
    AffineTransformType::OutputPointType outputPoint =
      m_IndexToPhysicalTransform->TransformPoint(inputPoint) ;
  
    // Update the output point
    point = outputPoint;
    }

  /**
   * Get a physical point (in the space which 
   * the origin and spacing infomation comes from) 
   * from a discrete index (in the index space) 
   *
   * Since this function internally uses AffineTranform, it is
   * templated over coordinate value type (TCoordRep); using float or
   * double for the coorinate value type is recommended.
   *
   * \todo In future, when MS Visual C++ supports out-of-class member 
   * templates, move function definition to itkImage.txx file.
   *
   * \sa AffineTransform */
  template<class TCoordRep> 
  void TransformIndexToPhysicalPoint(Index<VImageDimension>& index, 
    Point<TCoordRep, VImageDimension>& point)
    {
    // If no current transforms exist, build them using the origin and spacing
    if ( !m_IndexToPhysicalTransform ) { this->RebuildTransforms(); }
    
    AffineTransformType::InputPointType inputPoint;

    // Update the input index
    for (unsigned int i = 0 ; i < VImageDimension ; i++)
      { inputPoint[i] = index[i]; }

    // Transform the point
    AffineTransformType::OutputPointType outputPoint =
      m_IndexToPhysicalTransform->TransformPoint(inputPoint) ;
  
    // Update the output point
    point = outputPoint;
    }

  /** \brief Copy information from the specified data set.  
   *
   * This method is part of the pipeline execution model. By default,
   * a ProcessObject will copy meta-data from the first input to all
   * of its outputs. See ProcessObject::GenerateOutputInformation().
   * Each subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.  Image
   * has more meta-data than its superclasses Image or ImageBase.
   * Thus, it must provide its own version of CopyInformation() in
   * order to set its Spacing and Origin to match the input parameter.
   * This implementation of CopyInformation() casts the input
   * parameter to an ImageBase. If successful, this method call
   * GetSpacing() and GetOrigin() on its input (since all subclasses
   * of ImageBase are guarenteed to respond to GetSpacing() and
   * GetOrigin()). If "data" is another datatype, this method may not
   * do anything.   */
  virtual void CopyInformation(DataObject *data);

protected:
  Image();
  virtual ~Image();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  Image(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;

  /** Pixel accessor object - it converts the presentation of a pixel. */
  AccessorType          m_PixelAccessor;

  /** Origin and spacing of physical coordinates. */
  double                m_Spacing[ImageDimension];
  double                m_Origin[ImageDimension];

  /** Affine transforms used to convert between data and physical space. */
  AffineTransformPointer m_IndexToPhysicalTransform;
  AffineTransformPointer m_PhysicalToIndexTransform;
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

