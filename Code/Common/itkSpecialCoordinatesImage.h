/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpecialCoordinatesImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpecialCoordinatesImage_h
#define __itkSpecialCoordinatesImage_h

#include "itkImageBase.h"
#include "itkImageRegion.h"
#include "itkImportImageContainer.h"
#include "itkDefaultPixelAccessor.h"
#include "itkDefaultPixelAccessorFunctor.h"
#include "itkPoint.h"
#include "itkContinuousIndex.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class SpecialCoordinatesImage
 *  \brief Templated n-dimensional nonrectilinear-coordinate image base class.
 *
 * SpecialCoordinatesImages are templated over a pixel type (modeling the
 * dependent variables), and a dimension (number of independent variables).
 * The container for the pixel data is the ImportImageContainer.
 *
 * Within the pixel container, images are modeled as arrays, defined by a
 * start index and a size.
 * 
 * Almost arbitrary mappings between index space & Cartesian physical space are
 * possible, and so m_Origin and m_Spacing should be ignored.  They exist only
 * to allow the possibility of running a "spatially-aware" filter in raw index
 * space, as if the SpecialCoordinatesImage data was laid out on a regular grid.
 * Note that this may or may not produce useful results, and it is up the the
 * user to determine the appropriateness of running a filter designed for normal
 * images on special-coordinates images.
 * 
 * The only correct generic method for operating on a SpecialCoordinatesImage in
 * physical space is to use the virtual functions TransformPhysicalPointToIndex,
 * TransformPhysicalPointToContinuousIndex, TransformIndexToPhysicalPoint, and
 * TransformContinuousIndexToPhysicalPoint.  All of these methods transform
 * points in Cartesian physical space to and from indices in the special
 * (typically non-Cartesian) index space.  It is also possible to check the
 * type of coordinate representation being used by a SpecialCoordinatesImage,
 * and then use representation-specific code to speed up the filter for certain
 * coordinate representations, falling back to the generic method for
 * unrecognized and/or unoptimized coordinate representations.
 *
 * There are three sets of meta-data describing portians of a
 * SpecialCoordinatesImages. These are "Region" objects that define a portion of
 * an image via a starting index for the image array and a size. The ivar
 * LargestPossibleRegion defines the size and starting index of the image
 * dataset. The entire image dataset, however, need not be resident in memory.
 * The region of the image that is resident in memory is defined by the
 * "BufferedRegion". The Buffer is a contiguous block of memory.  The third set
 * of meta-data defines a region of interest, called the "RequestedRegion". The
 * RequestedRegion is used by the pipeline execution model to define what a
 * filter is requested to produce.
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
 * \sa Image
 *
 * \ingroup ImageObjects */
template <class TPixel, unsigned int VImageDimension=2>
class ITK_EXPORT SpecialCoordinatesImage : public ImageBase<VImageDimension>
{
public:
  /** Standard class typedefs */
  typedef SpecialCoordinatesImage     Self;
  typedef ImageBase<VImageDimension>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef WeakPointer<const Self>  ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpecialCoordinatesImage, ImageBase);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef TPixel PixelType;

  /** Typedef alias for PixelType */
  typedef TPixel ValueType ;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external
   * representation. */
  typedef TPixel InternalPixelType;

  typedef PixelType  IOPixelType;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /** Accessor functor to choose between accessors: DefaultPixelAccessor for
   * the Image, and DefaultVectorPixelAccessor for the vector image. The 
   * functor provides a generic API between the two accessors.*/
  typedef DefaultPixelAccessorFunctor< Self > AccessorFunctorType;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Container used to store pixels in the image. */
  typedef ImportImageContainer<unsigned long, PixelType> PixelContainer;

  /** Index typedef support. An index is used to access pixel values. */
  typedef typename Superclass::IndexType  IndexType;

  /** Offset typedef support. An offset is used to access pixel values. */
  typedef typename Superclass::OffsetType OffsetType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef typename Superclass::SizeType  SizeType;

  /** Region typedef support. A region is used to specify a subset of an image. */
  typedef typename Superclass::RegionType  RegionType;

  /** Spacing typedef support.  Spacing holds the "fake" size of a pixel, making
   * each pixel look like a 1 unit hyper-cube to filters that were designed for
   * normal images and that therefore use m_Spacing.  The spacing is the
   * geometric distance between image samples. */
  typedef typename Superclass::SpacingType SpacingType;

  /** Origin typedef support.  The origin is the "fake" geometric coordinates
   * of the index (0,0).  Also for use w/ filters designed for normal images. */
  typedef typename Superclass::PointType PointType;

  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  typedef typename PixelContainer::ConstPointer PixelContainerConstPointer;

  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions(). */
  void Allocate();

  /** Convenience methods to set the LargestPossibleRegion,
   *  BufferedRegion and RequestedRegion. Allocate must still be called.
   */
  void SetRegions(RegionType region)
    {
    this->SetLargestPossibleRegion(region);
    this->SetBufferedRegion(region);
    this->SetRequestedRegion(region);
    };

  void SetRegions(SizeType size)
    {
    RegionType region; region.SetSize(size);
    this->SetLargestPossibleRegion(region);
    this->SetBufferedRegion(region);
    this->SetRequestedRegion(region);
    };

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize();

  /** Fill the image buffer with a value.  Be sure to call Allocate()
   * first. */
  void FillBuffer (const TPixel& value);

  /** \brief Set a pixel value.
   *
   * Allocate() needs to have been called first -- for efficiency,
   * this function does not check that the image has actually been
   * allocated yet. */
  void SetPixel(const IndexType &index, const TPixel& value)
    {
    typename Superclass::OffsetValueType offset = this->ComputeOffset(index);
    (*m_Buffer)[offset] = value;
    }

  /** \brief Get a pixel (read only version).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const TPixel& GetPixel(const IndexType &index) const
  {
    typename Superclass::OffsetValueType offset = this->ComputeOffset(index);
    return ( (*m_Buffer)[offset] );
  }

  /** \brief Get a reference to a pixel (e.g. for editing).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  TPixel& GetPixel(const IndexType &index)
    {
    typename Superclass::OffsetValueType offset = this->ComputeOffset(index);
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
  PixelContainer* GetPixelContainer()
    { return m_Buffer.GetPointer(); }

  const PixelContainer* GetPixelContainer() const
    { return m_Buffer.GetPointer(); }

  /** Set the container to use. Note that this does not cause the
   * DataObject to be modified. */
  void SetPixelContainer( PixelContainer *container );

  /** Return the Pixel Accessor object */
  AccessorType GetPixelAccessor( void )
    { return AccessorType(); }

  /** Return the Pixel Accesor object */
  const AccessorType GetPixelAccessor( void ) const
    { return AccessorType(); }
  
  /** These functions do NOTHING!  They exist only to not break the pipeline.
   * It is vital that the user specify any and all physical-spacing parameters
   * to the output of a normal filter which is being used to output a
   * special-coordinates image.  Filters designed to produce a particular kind
   * of special-coordinates image should do this automatically. */
  virtual void SetSpacing( const SpacingType ) {}
  virtual void SetSpacing( const double [VImageDimension] ) {}
  virtual void SetSpacing( const float [VImageDimension] ) {}
  virtual void SetOrigin( const PointType ) {}
  virtual void SetOrigin( const double [VImageDimension] ) {}
  virtual void SetOrigin( const float [VImageDimension] ) {}
  
  /* It is ILLEGAL in C++ to make a templated member function virtual! */
  /* Therefore, we must just let templates take care of everything.    */
  /*
  template<class TCoordRep>
  virtual bool TransformPhysicalPointToContinuousIndex(
              const Point<TCoordRep, VImageDimension>& point,
              ContinuousIndex<TCoordRep, VImageDimension>& index   ) const = 0;
  
  template<class TCoordRep>
  virtual bool TransformPhysicalPointToIndex(
            const Point<TCoordRep, VImageDimension>&,
            IndexType & index                                ) const = 0;

  template<class TCoordRep>
  virtual void TransformContinuousIndexToPhysicalPoint(
            const ContinuousIndex<TCoordRep, VImageDimension>& index,
            Point<TCoordRep, VImageDimension>& point        ) const = 0;

  template<class TCoordRep>
  virtual void TransformIndexToPhysicalPoint(
                      const IndexType & index,
                      Point<TCoordRep, VImageDimension>& point ) const = 0;
  */

protected:
  SpecialCoordinatesImage();
  void PrintSelf(std::ostream& os, Indent indent) const;
  virtual ~SpecialCoordinatesImage() {};
private:
  SpecialCoordinatesImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;
};
#ifdef ITK_EXPLICIT_INSTANTIATION
   extern template class SpecialCoordinatesImage<float         ,2>;
   extern template class SpecialCoordinatesImage<double        ,2>;
   extern template class SpecialCoordinatesImage<unsigned char ,2>;
   extern template class SpecialCoordinatesImage<unsigned short,2>;
   extern template class SpecialCoordinatesImage<unsigned int  ,2>;
   extern template class SpecialCoordinatesImage<signed char   ,2>;
   extern template class SpecialCoordinatesImage<signed short  ,2>;
   extern template class SpecialCoordinatesImage<signed int    ,2>;
   extern template class SpecialCoordinatesImage<float         ,3>;
   extern template class SpecialCoordinatesImage<double        ,3>;
   extern template class SpecialCoordinatesImage<unsigned char ,3>;
   extern template class SpecialCoordinatesImage<unsigned short,3>;
   extern template class SpecialCoordinatesImage<unsigned int  ,3>;
   extern template class SpecialCoordinatesImage<signed char   ,3>;
   extern template class SpecialCoordinatesImage<signed short  ,3>;
   extern template class SpecialCoordinatesImage<signed int    ,3>;
#endif
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpecialCoordinatesImage.txx"
#endif

#endif

