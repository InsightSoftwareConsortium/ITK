/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageListSampleAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageListSampleAdaptor_h
#define __itkImageListSampleAdaptor_h

#include <typeinfo>

#include "itkImage.h"
#include "itkListSample.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIterator.h"

namespace itk{ 
  namespace Statistics{

/** \class ScalarAccessor
 *  \brief converts scalar pixel type to measurement vector type
 */
template< class TImage, class TMeasurement, unsigned int VMeasurementVectorSize >
class ScalarAccessor
{
public:
  typedef Point< TMeasurement, VMeasurementVectorSize > MeasurementVectorType ;
  typedef typename TImage::PixelType PixelType ;

  ScalarAccessor() {}
  virtual ~ScalarAccessor() {} 

  void SetMeasurementVector(typename TImage::Pointer image, 
                  typename TImage::IndexType index, 
                  MeasurementVectorType measurement)
  {
    PixelType pixel = measurement[0] ;
    image->SetPixel(index, pixel) ;
  }

  MeasurementVectorType GetMeasurementVector(PixelType pixel) 
  {
    MeasurementVectorType measurement ;
    measurement[0] = pixel ;
    return measurement ;
  }
private:
} ; // end of class

/** \class VectorAccessor
 *  \brief converts any vector pixel type to measurement vector type
 */
template< class TImage, class TMeasurement, unsigned int VMeasurementVectorSize >
class VectorAccessor
{
public:
  typedef Point< TMeasurement, VMeasurementVectorSize > MeasurementVectorType ;
  typedef typename TImage::PixelType PixelType ;

  VectorAccessor() {}
  virtual ~VectorAccessor() {}

  void SetMeasurementVector(typename TImage::Pointer image, 
                  typename TImage::IndexType index, 
                  MeasurementVectorType measurement)
  {
    PixelType pixel ;
    for (size_t i = 0 ; i < VMeasurementVectorSize ; i++)
      {
        pixel[i] = measurement[i] ;
      }

    image->SetPixel(index, pixel) ;
  }

  MeasurementVectorType GetMeasurementVector(PixelType pixel) 
  {
    MeasurementVectorType measurement ;
    for (size_t i = 0 ; i < VMeasurementVectorSize ; i++)
      {
        measurement[i] = pixel[i] ;
      }
    return measurement ;
  }
private:
} ; // end of class

/** \class ImageListSampleAdaptor
 *  \brief This class provides ListSample interfaces to ITK Image
 *
 * After calling SetImage(Image::Pointer) method to plug in the image object,
 * users can use Sample interfaces to access Image data.
 * However, the resulting data are a list of measurement vectors. The type of
 * data is measurement vector. For example, if the pixel type of Image object 
 * is STL vector< float > and each pixel has two different types of 
 * measurements, intensity and gradient magnitude, this adaptor has
 * measurement vector of type ITK Point< float, 2>, and one element of the Point
 * is intensity and the other is gradient magnitude.
 *
 * There are two concepts of dimensions for this container. One is for Image 
 * object, and the other is for measurement vector dimension.
 * Only when using ITK Index to access data, the former concept is applicable
 * Otherwise, dimensions means dimensions of measurement vectors. 
 *
 * From the above example, there were two elements in a pixel and each pixel
 * provides [] operator for accessing its elements. However, in many cases,
 * The pixel might be a scalar value such as int or float. In this case,
 * The pixel doesn't support [] operator. To deal with this problem,
 * This class has two companion classes, ScalarAccessor and VectorAccessor.
 * If the pixel type is a scalar type, then you don't have change the third
 * template argument. If you have pixel type is vector one and supports
 * [] operator, then replace third argument with VectorAccessor
 *
 * \sa Sample, ListSample
 */
  
template < class TImage, class TMeasurement = float, 
           unsigned int VMeasurementVectorSize = 1, 
           class TAccessor = 
           ScalarAccessor< TImage, TMeasurement, VMeasurementVectorSize > >
class ITK_EXPORT ImageListSampleAdaptor :
      public ListSample< TMeasurement, VMeasurementVectorSize >
{
public:
  /** Standard class typedefs */
  typedef ImageListSampleAdaptor Self;
  typedef ListSample<TMeasurement, VMeasurementVectorSize> Superclass;
  typedef SmartPointer<Self> Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageListSampleAdaptor, ListSample) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  
  /** Image typedef support */
  typedef TImage ImageType;
  
  /**  * Image Pointer typedef support */
  typedef typename ImageType::Pointer ImagePointer ;
  
  /** Dimension of the Data */
  enum { MeasurementVectorSize = VMeasurementVectorSize } ;

  /** MeasurementVector Coordinate Representation typedef support */
  typedef TMeasurement MeasurementType ;

  /** Image Iterator typedef support */
  typedef ImageRegionIterator<ImageType> IteratorType ; 
  
  /** Image class typedefs for index and pixel */
  typedef typename ImageType::IndexType IndexType ;
  typedef typename ImageType::PixelType PixelType ;

  /** Superclass typedefs for Measurement vector, measurement, Instance Identifier, 
   * frequency, size, size element value */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::SizeType SizeType ;
  typedef typename Superclass::SizeValueType SizeValueType ;

  /** returns a Size object with the size of each dimension */
  SizeType GetSize() 
  {
    unsigned long length = m_Image->GetPixelContainer()->Size() ;
    SizeType size ;
    for (int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        size[i] = length ;
      }

    return size ;
  }

  /** returns the size of the 'dimension' dimension 
   * in the ListSample subclasses, the size of each dimension 
   * is equal to the number of instance. And the size is all same over all dimensions. */
  SizeValueType GetSize(unsigned int dimension) 
  {
    return static_cast< SizeValueType >(m_Image->GetPixelContainer()->Size()) ;
  }

  /** returns the index that is uniquely labelled by an instance identifier 
   * The corresponding id is the offset of the index  
   * This method uses ImageBase::ComputeIndex() method */
  IndexType GetIndex(const InstanceIdentifier id)  ;

  /** returns the instance identifier of the cell that is indexed by the  
   * index. The corresponding instance identifier is the offset of the index  
   * This method uses ImageBase::ComputeIndex() method */
  InstanceIdentifier GetInstanceIdentifier(const IndexType index)  ;

  /** Method to set the image */
  void SetImage(ImagePointer image) { m_Image = image ; }

  /** Method to get the image */
  ImagePointer GetImage() { return m_Image ; }  
  
  void SetMeasurementVector(IndexType index, MeasurementVectorType measurement)
  { m_Accessor.SetMeasurementVector(m_Image, index, measurement) ; }
  
  void SetMeasurement(const IndexType index, 
                         const unsigned int dim,
                         const MeasurementType value) 
  {
    MeasurementVectorType f = GetMeasurementVector(index) ;
    f[dim] = value ;
    SetMeasurementVector(index, f) ;
  }

  MeasurementVectorType GetMeasurementVector(const IndexType index) 
  {
    return m_Accessor.GetMeasurementVector(m_Image->GetPixel(index)) ;
  }

  /** Method to get the measurement from list using an instance identifier */
  MeasurementVectorType GetMeasurementVector(const InstanceIdentifier id) 
  { return GetMeasurementVector(GetIndex(id)) ; }

  /** Method to get a measurement element the measurement from this container */
  MeasurementType GetMeasurement(const IndexType index, 
                                       const unsigned int dim) 
  { 
    return GetMeasurementVector(index)[dim] ; 
  }
  

  /** Method to get the frequency from list.  It always returns 1 if there is 
   * an element, if not, error will occur. */
  FrequencyType GetFrequency(const IndexType index) 
  {
    m_Image->GetPixel(index);
    return 1 ;
  }
  
  FrequencyType GetFrequency(const InstanceIdentifier id)  
  {
    return ( GetFrequency(GetIndex(id)) ) ;
  }


  void SetMeasurement(const unsigned int dimension,
                         const unsigned int n,
                         const MeasurementType value) 
  {
    SetMeasurement(GetIndex(n), dimension, value ) ;
  }

  /** Method to get the measurement from list using an instance identifier */
  MeasurementType GetMeasurement(const unsigned int dimension,
                                       const unsigned long n) 
  { return GetMeasurement(GetIndex(n), dimension) ; }

  /** returns the frequency of the 'n'-th element in the 'd' dimension  
   * of the measurement vector */
  FrequencyType GetFrequency(const unsigned int dimension,
                             const unsigned long n)  
  { return GetFrequency(GetIndex(n)) ; }

  FrequencyType GetTotalFrequency(const unsigned int dimension) 
  { return GetSize(dimension) ; }

  class Iterator;
  friend class Iterator;
  
  Iterator Begin()
  { 
    IteratorType it(m_Image,  m_Image->GetBufferedRegion());
    Iterator iter(IndexType::ZeroIndex, it.Begin(), this);
    return iter; 
  }
  
  Iterator  End()        
  {
    IteratorType it(m_Image, m_Image->GetBufferedRegion());
    Iterator iter(it.End().GetIndex(), it.End(), this); 
    return iter; 
  }
  
  class Iterator
  {
  public:
    
    Iterator(){}
    
    Iterator(Pointer h)
    { 
      IteratorType it(h->m_Image, h->m_Image->GetBufferedRegion());
      m_Iter = it.Begin();
      m_Pos = IndexType::ZeroIndex;
      m_ImageListSampleAdaptor = h; 
    } 
    
    Iterator(IndexType d, IteratorType i, Pointer h)
      :m_Pos(d), m_Iter(i), m_ImageListSampleAdaptor(h)
    {}
    
    const float GetFrequency() 
    {
      return  m_ImageListSampleAdaptor->GetFrequency(m_Pos); 
    }
    
    MeasurementVectorType GetMeasurementVector()
    { 
      MeasurementVectorType feat;
      feat = m_ImageListSampleAdaptor->GetMeasurementVector(m_Pos); 
      return feat; 
    } 
    
    MeasurementType GetMeasurement(int dim)
    {
      MeasurementVectorType feat;
      feat = m_ImageListSampleAdaptor->GetMeasurementVector(m_Pos); 
      return feat[dim]; 
    } 
    
    IndexType GetIndex()   
    { return m_Pos;  }
    
    InstanceIdentifier GetInstanceIdentifier()   
    { return m_ImageListSampleAdaptor->GetInstanceIdentifier(m_Pos) ;  }

    Iterator& operator++() 
    { 
      ++m_Iter ;
      m_Pos = m_Iter.GetIndex() ;
      return *this ;
    }
    
    bool operator!=(const Iterator& it) 
    { return (m_Pos != it.m_Pos); }
    
    bool operator==(const Iterator& it) 
    { return (m_Pos == it.m_Pos); }
    
    Iterator& operator=(const Iterator& iter)
    {
      m_Pos = iter.m_Pos;
      m_Iter = iter.m_Iter;
      m_ImageListSampleAdaptor = iter.m_ImageListSampleAdaptor; 
    }
    
  private:
    IndexType m_Pos;  // Current index of iterator
    IteratorType m_Iter;       // Iterator pointing to ImageListSampleAdaptor
    Pointer m_ImageListSampleAdaptor;       // Pointer to ImageListSampleAdaptor
  } ;
  
protected:
  ImageListSampleAdaptor() {}
  virtual ~ImageListSampleAdaptor() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

private:
  ImageListSampleAdaptor(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  ImagePointer m_Image ;
  TAccessor m_Accessor ;
} ;

  } // end of namespace Statistics
} // end of namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageListSampleAdaptor.txx"
#endif

#endif
