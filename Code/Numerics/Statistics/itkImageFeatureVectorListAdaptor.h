/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFeatureVectorListAdaptor.h
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
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCyH DAMAGE.

=========================================================================*/ 
#ifndef __itkImageFeatureVectorListAdaptor_h
#define __itkImageFeatureVectorListAdaptor_h

#include <typeinfo>

#include "itkImage.h"
#include "itkFeatureVectorList.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIterator.h"

namespace itk{ 
  namespace Statistics{

/** \class ScalarAccessor
 *  \brief converts scalar pixel type to feature vector type
 */
template< class TImage, class TFeatureElement, unsigned int VFeatureDimension >
class ScalarAccessor
{
public:
  typedef Point< TFeatureElement, VFeatureDimension > FeatureVectorType ;
  typedef typename TImage::PixelType PixelType ;

  ScalarAccessor() {}
  virtual ~ScalarAccessor() {} 

  void SetFeature(typename TImage::Pointer image, 
                  typename TImage::IndexType index, 
                  FeatureVectorType feature)
  {
    PixelType pixel = feature[0] ;
    image->SetPixel(index, pixel) ;
  }

  FeatureVectorType GetFeature(PixelType pixel) 
  {
    FeatureVectorType feature ;
    feature[0] = pixel ;
    return feature ;
  }
private:
} ; // end of class

/** \class VectorAccessor
 *  \brief converts any vector pixel type to feature vector type
 */
template< class TImage, class TFeatureElement, unsigned int VFeatureDimension >
class VectorAccessor
{
public:
  typedef Point< TFeatureElement, VFeatureDimension > FeatureVectorType ;
  typedef typename TImage::PixelType PixelType ;

  VectorAccessor() {}
  virtual ~VectorAccessor() {}

  void SetFeature(typename TImage::Pointer image, 
                  typename TImage::IndexType index, 
                  FeatureVectorType feature)
  {
    PixelType pixel ;
    for (size_t i = 0 ; i < VFeatureDimension ; i++)
      {
        pixel[i] = feature[i] ;
      }

    image->SetPixel(index, pixel) ;
  }

  FeatureVectorType GetFeature(PixelType pixel) 
  {
    FeatureVectorType feature ;
    for (size_t i = 0 ; i < VFeatureDimension ; i++)
      {
        feature[i] = pixel[i] ;
      }
    return feature ;
  }
private:
} ; // end of class

/** \class ImageFeatureVectorListAdaptor
 *  \brief This class provides FeatureVectorList interfaces to ITK Image
 *
 * After calling SetImage(Image::Pointer) method to plug in the image object,
 * users can use FeatureVectorContainer interfaces to access Image data.
 * However, the resulting data are a list of feature vectors. The type of
 * data is feature vector. For example, if the pixel type of Image object 
 * is STL vector< float > and each pixel has two different types of 
 * measurements, intensity and gradient magnitude, this adaptor has
 * feature vector of type ITK Point< float, 2>, and one element of the Point
 * is intensity and the other is gradient magnitude.
 *
 * There are two concepts of dimensions for this container. One is for Image 
 * object, and the other is for feature vector dimension.
 * Only when using ITK Index to access data, the former concept is applicable
 * Otherwise, dimensions means dimensions of feature vectors. 
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
 * \sa FeatureVectorContainer, FeatureVectorList
 */
  
template < class TImage, class TFeatureElement = float, 
           unsigned int VFeatureDimension = 1, 
           class TAccessor = 
           ScalarAccessor< TImage, TFeatureElement, VFeatureDimension > >
class ITK_EXPORT ImageFeatureVectorListAdaptor :
      public FeatureVectorList< TFeatureElement, VFeatureDimension >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageFeatureVectorListAdaptor Self;
  
  /**
   * Standard "Superclass" typedef
   */
  typedef FeatureVectorList<TFeatureElement, VFeatureDimension> Superclass;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  
  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageFeatureVectorListAdaptor, FeatureVectorList) ;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self) ;
  
  /**
   * Image typedef support
   */
  typedef TImage ImageType;
  
  /** 
   * Image Pointer typedef support
   */
  typedef typename ImageType::Pointer ImagePointer ;
  
  /**
   * Dimension of the Data
   */
  enum { FeatureDimension = VFeatureDimension } ;

  /**
   * Feature Coordinate Representation typedef support
   */
  typedef TFeatureElement FeatureElementType;

  /**
   * Image Iterator typedef support
   */
  typedef ImageRegionIterator<ImageType> IteratorType; 
  
  typedef typename ImageType::IndexType IndexType ;
  typedef typename ImageType::PixelType PixelType ;
  typedef typename Superclass::FeatureVectorType FeatureVectorType;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;
  
  /**
   * 
   */
  typedef Superclass::FrequencyType FrequencyType ;
  typedef Superclass::SizeType SizeType ;
  typedef Superclass::SizeValueType SizeValueType ;

  /**
   * returns a Size object with the size of each dimension
   */
  SizeType GetSize() 
  {
    unsigned long length = m_Image->GetPixelContainer()->Size() ;
    SizeType size ;
    for (int i = 0 ; i < FeatureDimension ; i++)
      {
        size[i] = length ;
      }

    return size ;
  }

  /**
   * returns the size of the 'dimension' dimension
   * in the FeatureVectorList subclasses, the size of each dimension
   * is equal to the number of instance. And the size is all same over
   * all dimensions.
   */
  SizeValueType GetSize(unsigned int dimension) 
  {
    return static_cast< SizeValueType >(m_Image->GetPixelContainer()->Size()) ;
  }

  /**
   * returns the index that is uniquely labelled by an instance identifier
   * The corresponding id is the offset of the index 
   * This method uses ImageBase::ComputeIndex() method
   */
  IndexType GetIndex(const InstanceIdentifier id)  ;

  /**
   * returns the instance identifier of the cell that is indexed by the 
   * index.
   * The corresponding instance identifier is the offset of the index 
   * This method uses ImageBase::ComputeIndex() method
   */
  InstanceIdentifier GetInstanceIdentifier(const IndexType index)  ;

  /**
   * Method to set the image
   */
  void SetImage(ImagePointer image) { m_Image = image ; }

  /**
   * Method to get the image
   */
  ImagePointer GetImage() { return m_Image ; }  
  
  void SetFeature(IndexType index, FeatureVectorType feature)
  { m_Accessor.SetFeature(m_Image, index, feature) ; }
  
  void SetFeatureElement(const IndexType index, 
                         const unsigned int dim,
                         const FeatureElementType value) 
  {
    FeatureVectorType f = GetFeature(index) ;
    f[dim] = value ;
    SetFeature(index, f) ;
  }

  FeatureVectorType GetFeature(const IndexType index) 
  {
    return m_Accessor.GetFeature(m_Image->GetPixel(index)) ;
  }

  /**
   * Method to get the feature from list using an instance identifier
   */
  FeatureVectorType GetFeature(const InstanceIdentifier id) 
  { return GetFeature(GetIndex(id)) ; }

  /**
   * Method to get a feature element the feature 
   * from this container
   */
  FeatureElementType GetFeatureElement(const IndexType index, 
                                       const unsigned int dim) 
  { 
    return GetFeature(index)[dim] ; 
  }
  

  /**
   * Method to get the frequency from list.  It always returns 1 if there is
   * an element, if not, error will occur.
   */
  FrequencyType GetFrequency(const IndexType index) 
  {
    m_Image->GetPixel(index);
    return 1 ;
  }
  
  FrequencyType GetFrequency(const InstanceIdentifier id)  
  {
    return ( GetFrequency(GetIndex(id)) ) ;
  }


  void SetFeatureElement(const unsigned int dimension,
                         const unsigned int n,
                         const FeatureElementType value) 
  {
    SetFeatureElement(GetIndex(n), dimension, value ) ;
  }

  /**
   * Method to get the feature from list using an instance identifier
   */
  FeatureElementType GetFeatureElement(const unsigned int dimension,
                                       const unsigned long n) 
  { return GetFeatureElement(GetIndex(n), dimension) ; }

  /**
   * returns the frequency of the 'n'-th element in the 'd' dimension 
   * of the feature vector
   */
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
      m_ImageFeatureVectorListAdaptor = h; 
    } 
    
    Iterator(IndexType d, IteratorType i, Pointer h)
      :m_Pos(d), m_Iter(i), m_ImageFeatureVectorListAdaptor(h)
    {}
    
    const float GetFrequency() 
    {
      return  m_ImageFeatureVectorListAdaptor->GetFrequency(m_Pos); 
    }
    
    FeatureVectorType GetFeature()
    { 
      FeatureVectorType feat;
      feat = m_ImageFeatureVectorListAdaptor->GetFeature(m_Pos); 
      return feat; 
    } 
    
    FeatureElementType GetFeature(int dim)
    {
      FeatureVectorType feat;
      feat = m_ImageFeatureVectorListAdaptor->GetFeature(m_Pos); 
      return feat[dim]; 
    } 
    
    IndexType GetPosition()   
    { return m_Pos;  }
    
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
      m_ImageFeatureVectorListAdaptor = iter.m_ImageFeatureVectorListAdaptor; 
    }
    
  private:
    IndexType m_Pos;  // Current index of iterator
    InstanceIdentifier m_Id ;  // Current instance identifier of iterator
    IteratorType m_Iter;       // Iterator pointing to ImageFeatureVectorListAdaptor
    Pointer m_ImageFeatureVectorListAdaptor;       // Pointer to ImageFeatureVectorListAdaptor
  } ;
  
protected:
  ImageFeatureVectorListAdaptor() {}
  virtual ~ImageFeatureVectorListAdaptor() {}
  
private:
  ImageFeatureVectorListAdaptor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ImagePointer m_Image ;
  TAccessor m_Accessor ;
} ;

  } // end of namespace Statistics
} // end of namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageFeatureVectorListAdaptor.txx"
#endif

#endif
