/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorImage_h
#define __itkVectorImage_h

#include "itkImage.h"
#include "itkImageRegion.h"
#include "itkImportImageContainer.h"
#include "itkDefaultVectorPixelAccessor.h"
#include "itkDefaultVectorPixelAccessorFunctor.h"
#include "itkVectorImageNeighborhoodAccessorFunctor.h"
#include "itkPoint.h"
#include "itkContinuousIndex.h"
#include "itkVariableLengthVector.h"
#include "itkWeakPointer.h"

namespace itk
{

/** \class VectorImage
 *  \brief Templated n-dimensional vector image class.
 * 
 * This class differs from Image in that it is intended to represent multiple
 * images. Each pixel represents \e k measurements, each of datatype \e TPixel.
 * The memory organization of the resulting image is as follows:
 *   ... Pi0 Pi1 Pi2 Pi3 P(i+1)0 P(i+1)1 P(i+1)2 P(i+1)3 P(i+2)0 ...
 * where Pi0 represents the 0th measurement of the pixel at index i.
 *
 * Conceptually, a <tt>VectorImage< double, 3 ></tt> is the same as a
 * <tt>Image< VariableLengthVector< double >, 3 ></tt>. The difference lies in the memory 
 * organization. The latter results in a fragmented 
 * organization with each location in the Image holding a pointer to an \c VariableLengthVector
 * holding the actual pixel. The former stores the \e k pixels instead of a 
 * pointer reference, which apart from avoiding fragmentation of memory also avoids
 * storing a 8 bytes of pointer reference for each pixel. 
 * The parameter \e k can be set using \c SetVectorLength.
 *
 * The API of the class is such that it returns a pixeltype VariableLengthVector< double > when 
 * queried, with the data internally pointing to the buffer. (the container does not
 * manage the memory). Similarly SetPixel calls can be made with VariableLengthVector< double >.
 *
 * The API of this class is similar to Image.
 *
 * \note
 * When reading VectorImages, make sure you instantiate the reader with pixel 
 * traits of the InternalPixelType rather than the PixelType. For example:
 *
 * \code
 * typedef itk::VectorImage< float, 3 > VectorImageType;
 * tyepdef VectorImageType::InternalPixelType InternalPixelType;
 * typedef itk::ImageFileReader< VectorImageType, 
 *         itk::DefaultConvertPixelTraits<InternalPixelType> > ReaderType;
 * \endcode
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \sa DefaultVectorPixelAccessor
 * \sa DefaultVectorPixelAccessorFunctor
 * \sa VectorImageToImagePixelAccessor
 * \sa VectorImageToImageAdaptor
 * \sa Image
 * \sa ImportImageContainer
 *
 * \example Testing/Code/Common/itkVectorImageTest.cxx
 * 
 * \ingroup ImageObjects 
 */
template <class TPixel, unsigned int VImageDimension=3 >
class ITK_EXPORT VectorImage : 
    public Image< VariableLengthVector< TPixel >, VImageDimension >
{
public:
  /** Standard class typedefs */
  typedef VectorImage                  Self;
  typedef ImageBase<VImageDimension>   Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;
  typedef WeakPointer<const Self>      ConstWeakPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorImage, Image);

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. This is not the actual pixel type contained in 
   * the buffer, ie m_Buffer. The image exhibits an external API of an 
   * VariableLengthVector< T > and internally stores its data as type T. */
  typedef VariableLengthVector< TPixel > PixelType;
  
  /** This is the actual pixel type contained in the buffer. Each vector
   * pixel is composed of 'm_VectorLength' contiguous InternalPixelType.
   */
  typedef TPixel InternalPixelType;

  /** Typedef alias for PixelType */
  typedef TPixel ValueType ;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  typedef DefaultVectorPixelAccessor< InternalPixelType > AccessorType;

  /** Functor to provide a common API between DefaultPixelAccessor and
   * DefaultVectorPixelAccessor */
  typedef DefaultVectorPixelAccessorFunctor< Self >       AccessorFunctorType;

  /** Tyepdef for the functor used to access a neighborhood of pixel pointers.*/
  typedef VectorImageNeighborhoodAccessorFunctor< 
                          Self >              NeighborhoodAccessorFunctorType;

  /** Container used to store pixels in the image. */
  typedef ImportImageContainer<unsigned long, InternalPixelType> PixelContainer;
  
  /** Index typedef support. An index is used to access pixel values. */
  typedef typename Superclass::IndexType  IndexType;

  /** Offset typedef support. An offset is used to access pixel values. */
  typedef typename Superclass::OffsetType OffsetType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef typename Superclass::SizeType  SizeType;

  /** Direction typedef support. A matrix of direction cosines. */
  typedef typename Superclass::DirectionType  DirectionType;

  /** Region typedef support. A region is used to specify a subset of an image. */
  typedef typename Superclass::RegionType  RegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename Superclass::SpacingType SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef typename Superclass::PointType PointType;

  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  typedef typename PixelContainer::ConstPointer PixelContainerConstPointer;

  /** Offset typedef (relative position between indices) */
  typedef typename Superclass::OffsetValueType OffsetValueType;

  typedef unsigned int VectorLengthType;

  /** Allocate the image memory. The size of the image must
   * already be set, e.g. by calling SetRegions(). */
  void Allocate();

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize();

  /** Fill the image buffer with a value.  Be sure to call Allocate()
   * first. */
  void FillBuffer(const PixelType& value);

  void SetPixel( const IndexType &index, const PixelType& value )
    {
    OffsetValueType offset = m_VectorLength * this->ComputeOffset(index);
    for( VectorLengthType i = 0; i < m_VectorLength; i++ )
      {
      (*m_Buffer)[offset + i] = value[i];
      }
    }

  /** \brief Get a pixel (read only version).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  const PixelType GetPixel(const IndexType &index) const
    {
    OffsetValueType offset = m_VectorLength * this->ComputeOffset(index);
    PixelType p( &((*m_Buffer)[offset]), m_VectorLength ); 
    return p;
    }

  /** \brief Get a reference to a pixel (e.g. for editing).
   *
   * For efficiency, this function does not check that the
   * image has actually been allocated yet. */
  PixelType  GetPixel(const IndexType &index )
    {
    OffsetValueType offset = m_VectorLength * this->ComputeOffset(index);
    PixelType p( &((*m_Buffer)[offset]), m_VectorLength ); 
    return p;
    }

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  InternalPixelType *GetBufferPointer()
    { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; }
  const InternalPixelType *GetBufferPointer() const
    { return m_Buffer ? m_Buffer->GetBufferPointer() : 0; }

  /** Return a pointer to the container. */
  PixelContainer* GetPixelContainer()
    { return m_Buffer.GetPointer(); }

  /** Return a pointer to the container. */
  const PixelContainer* GetPixelContainer() const
    { return m_Buffer.GetPointer(); }

  /** Set the container to use. Note that this does not cause the
   * DataObject to be modified. */
  void SetPixelContainer( PixelContainer *container );

  /** Graft the data and information from one image to another. This
   * is a convenience method to setup a second image with all the meta
   * information of another image and use the same pixel
   * container. Note that this method is different than just using two
   * SmartPointers to the same image since separate DataObjects are
   * still maintained. This method is similar to
   * ImageSource::GraftOutput(). The implementation in ImageBase
   * simply calls CopyInformation() and copies the region ivars.
   * The implementation here refers to the superclass' implementation
   * and then copies over the pixel container. */
  virtual void Graft(const DataObject *data);
  
  /** Return the Pixel Accessor object */
  AccessorType GetPixelAccessor( void ) 
    { return AccessorType( m_VectorLength ); }

  /** Return the Pixel Accesor object */
  const AccessorType GetPixelAccessor( void ) const
    { return AccessorType( m_VectorLength ); }

  /** Return the NeighborhoodAccessor functor */
  NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() 
    { return NeighborhoodAccessorFunctorType( m_VectorLength ); }
  
  /** Return the NeighborhoodAccessor functor */
  const NeighborhoodAccessorFunctorType GetNeighborhoodAccessor() const
    { return NeighborhoodAccessorFunctorType(m_VectorLength); }

  
  /** Set/Get macros for the length of each vector in the vector image */
  itkSetMacro( VectorLength, VectorLengthType );
  itkGetConstReferenceMacro( VectorLength, VectorLengthType );

protected:
  VectorImage();
  void PrintSelf( std::ostream& os, Indent indent ) const;
  virtual ~VectorImage() {};

private:
  VectorImage( const Self & ); // purposely not implementated
  void operator=(const Self&); //purposely not implemented
  

private:
  /** Length of the "vector pixel" */
  VectorLengthType m_VectorLength;
  
  /** Memory for the current buffer. */
  PixelContainerPointer m_Buffer;
}; 


} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorImage.txx"
#endif

#endif
