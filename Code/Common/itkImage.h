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

/**
 * \class Image
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
 * Insight-defined class type such as Vector; or a
 * user-defined type. Note that depending on the type of pixel that you use,
 * the process objects (i.e., those filters processing data objects), may not
 * operate on the image and/or pixel type. This becomes apparant at 
 * compile-time because operator overloading (for the pixel type) is 
 * not supported.
 *
 * The data in an image is arranged in a 1D array as [][][][slice][row][col]
 * with Index[0] = col, Index[1] = row, Index[2] = slice, ...
 *
 * \sa ImageContainerInterface
 * */

template <class TPixel, unsigned int VImageDimension=2,
          class TImageTraits=DefaultImageTraits< TPixel, VImageDimension > >
class ITK_EXPORT Image : public ImageBase<VImageDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Image               Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageBase<VImageDimension>  Superclass;

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
  typedef DefaultPixelAccessor< PixelType > AccessorType;

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
   * Definition of the Point type used for settin the origin
   */
  typedef typename AffineTransformType::PointType    PointType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Image, ImageBase);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  


  /**
   * Allocate the image memory. The image Dimension and Size must 
   * be set a priori.
   */
  void Allocate();


  /**
   * Set a pixel value.
   */
  void SetPixel(const IndexType &index, const TPixel& value)
  {
    unsigned long offset = this->ComputeOffset(index);
    (*m_Buffer)[offset] = value;
  }
  
  /**
   * Get a pixel (read only version).
   */
  const TPixel& GetPixel(const IndexType &index) const
  {
    // std::cerr << "Const GetPixel()" << std::endl;
    unsigned long offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }

  /**
   * Get a pixel for editing. 
   */
  TPixel& GetPixel(const IndexType &index)
  {
    unsigned long offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }
    
  /**
   * Access a pixel. This version can be an lvalue.
   */
  TPixel & operator[](const IndexType &index)
     { return this->GetPixel(index); }
  
  /**
   * Access a pixel. This version can only be an rvalue.
   */
  const TPixel& operator[](const IndexType &index) const
     { return this->GetPixel(index); }


  /**
   * Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class.
   */
  TPixel *GetBufferPointer()
    { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; };

  const TPixel *GetBufferPointer() const
    { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; };

  /**
   * Return a pointer to the container 
   */
  PixelContainerPointer GetPixelContainer()
    { return m_Buffer; };


  /**
   * Return the Pixel Accesor object
   */
  AccessorType & GetPixelAccessor( void ) 
    { return m_PixelAccessor; }
    
  /**
   * Return the Pixel Accesor object
   */
  const AccessorType & GetPixelAccessor( void ) const
    { return m_PixelAccessor; }
    
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
  virtual void SetOrigin( const PointType & point );

  /** 
   * Get the origin of the image. The origin is the geometric
   * coordinates of the index (0,0).  The value returned is
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

  /**
   * Get the cotinuous index from a physical point
   *
   * Since this function internally use AffineTranform, even this function 
   * is templated over coordinate value type (TCoordRep), using float or
   * double for the coorinate value type is recommended.
   *
   * TO DO:
   * In future, when MS Visual C++ supports out-of-class member templates,
   * move function definition to itkImage.txx file.
   * \sa AffineTransform 
   */
  template<class TCoordRep> 
  void GetContinuousIndex(Point<TCoordRep, VImageDimension>& point, 
   ContinuousIndex<TCoordRep, VImageDimension>& index)
  {
    typedef AffineTransform<TCoordRep, VImageDimension> TransformType ;
    typedef TransformType::InputPointType InputPointType ;
    typedef TransformType::OutputPointType OutputPointType ;
    
    TransformType::MatrixType matrix;
    TransformType::VectorType offset;
    for (unsigned int i = 0; i < VImageDimension; i++)
      {
        for (unsigned int j = 0; j < VImageDimension; j++)
          {
            matrix[i][j] = 0.0;
          }
        matrix[i][i] = m_Spacing[i];
        offset[i]    = m_Origin [i];
      }
    
    TransformType transform(matrix, offset);
    
    InputPointType inputPoint = transform.BackTransform(point) ;

    for (unsigned int i = 0 ; i < VImageDimension ; i++)
      index[i] = inputPoint[i] ;
  }


  /**
   * Get a physical point (in the space which 
   * the origin and spacing infomation comes from) 
   * from a continous index (in the index space) 
   *
   * Since this function internally use AffineTranform, even this function 
   * is templated over coordinate value type (TCoordRep), using float or
   * double for the coorinate value type is recommended.
   *
   * TO DO:
   * In future, when MS Visual C++ supports out-of-class member templates,
   * move function definition to itkImage.txx file.
   *
   * \sa AffineTransform 
   */
  template<class TCoordRep> 
  void GetPhysicalPoint(ContinuousIndex<TCoordRep, VImageDimension>& index, 
   Point<TCoordRep, VImageDimension>& point)
  {
    typedef AffineTransform<TCoordRep, VImageDimension> TransformType ;
    typedef TransformType::InputPointType InputPointType ;
    typedef TransformType::OutputPointType OutputPointType ;
    
    TransformType::MatrixType matrix;
    TransformType::VectorType offset;
    
    for (unsigned int i = 0; i < VImageDimension; i++)
      {
        for (unsigned int j = 0; j < VImageDimension; j++)
          {
            matrix[i][j] = 0.0;
          }
        matrix[i][i] = m_Spacing[i] ;
        offset[i]    = m_Origin[i] ;
      }
    
    TransformType transform(matrix, offset);
    InputPointType inputPoint ;
    for (unsigned int i = 0 ; i < VImageDimension ; i++)
      inputPoint[i] = index[i] ;

    OutputPointType outputPoint = transform.TransformPoint(inputPoint) ;
    
    //for (unsigned int i = 0 ; i < VImageDimension ; i++)
    //point[i] = outputPoint[i] ;
    point = outputPoint ;
  }

  /**
   * Copy information from the specified data set.  This method is
   * part of the pipeline execution model. By default, a ProcessObject
   * will copy meta-data from the first input to all of its
   * outputs. See ProcessObject::GenerateOutputInformation().  Each
   * subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.
   * Image has more meta-data than its superclasses Image or
   * ImageBase.  Thus, it must provide its own version of
   * CopyInformation() in order to set its Spacing and Origin to match
   * the input parameter.  This implementation of CopyInformation()
   * casts the input parameter to an ImageBase. If successful,
   * this method call GetSpacing() and GetOrigin() on its input
   * (since all subclasses of ImageBase are guarenteed to respond to
   * GetSpacing() and GetOrigin()). If "data" is another datatype, this
   * method may not do anything.
   */
  virtual void CopyInformation(DataObject *data);

protected:
  Image();
  virtual ~Image();
  Image(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  
private:
  // memory for the current buffer
  PixelContainerPointer m_Buffer;

  // Pixel accessor object, 
  // it converts the presentation of a pixel
  AccessorType          m_PixelAccessor;
  double              m_Spacing[ImageDimension];
  double              m_Origin[ImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImage.txx"
#endif

#endif

