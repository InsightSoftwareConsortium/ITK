/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJointDomainImageToListAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJointDomainImageToListAdaptor_h
#define __itkJointDomainImageToListAdaptor_h

#include "itkMacro.h"
#include "itkFixedArray.h"
#include "itkPoint.h"
#include "itkPixelTraits.h"
#include "itkImageToListAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkEuclideanDistance.h"
#include "itkListSample.h"

namespace itk{ 
namespace Statistics{

template< class TImage >
struct ImageJointDomainTraits
{
  typedef ImageJointDomainTraits Self ;
  typedef PixelTraits< typename TImage::PixelType > PixelTraitsType ;
  typedef typename PixelTraitsType::ValueType RangeDomainMeasurementType ;
  typedef typename TImage::IndexType::IndexValueType IndexValueType ;
  itkStaticConstMacro(Dimension, 
                      unsigned int, 
                      TImage::ImageDimension +
                      PixelTraitsType::Dimension ) ;
  typedef JoinTraits< RangeDomainMeasurementType, float > JoinTraitsType ;

  typedef typename JoinTraitsType::ValueType MeasurementType ;

  typedef FixedArray< MeasurementType, itkGetStaticConstMacro(Dimension) >
  MeasurementVectorType ;
} ; // end of ImageJointDomainTraits

/** \class JointDomainImageToListAdaptor
 *  \brief This class provides ListSampleBase interfaces to ITK Image
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
 * This class has two companion classes, JointDomainAccessor and VectorAccessor.
 * If the pixel type is a scalar type, then you don't have change the third
 * template argument. If you have pixel type is vector one and supports
 * [] operator, then replace third argument with VectorAccessor
 *
 * \sa Sample, ListSampleBase
 */

template < class TImage >
class ITK_EXPORT JointDomainImageToListAdaptor 
  : public ImageToListAdaptor< 
  TImage, 
  typename ImageJointDomainTraits< TImage >::MeasurementVectorType >
{
public:
  typedef ImageJointDomainTraits< TImage >
  ImageJointDomainTraitsType ;
  typedef typename ImageJointDomainTraitsType::MeasurementVectorType
  MeasurementVectorType ;

  /** Standard class typedefs */
  typedef JointDomainImageToListAdaptor Self;
  typedef ImageToListAdaptor< TImage, MeasurementVectorType > 
  Superclass;
  typedef SmartPointer< Self > Pointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(JointDomainImageToListAdaptor, ImageToListAdaptor) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  
  /** the number of components in a measurement vector */
  itkStaticConstMacro(MeasurementVectorSize, 
                      unsigned int, 
                      ImageJointDomainTraitsType::Dimension) ;

  /** Superclass typedefs for Measurement vector, measurement, 
   * Instance Identifier, frequency, size, size element value */
  typedef typename ImageJointDomainTraitsType::MeasurementType
  MeasurementType ;
  typedef typename ImageJointDomainTraitsType::RangeDomainMeasurementType
  RangeDomainMeasurementType ;

  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  typedef typename TImage::IndexType ImageIndexType ;
  typedef typename TImage::SizeType ImageSizeType ;
  typedef typename TImage::RegionType ImageRegionType ;
  typedef ImageRegionIteratorWithIndex< TImage > ImageIteratorType ;

  typedef MeasurementVectorType ValueType ;
  typedef EuclideanDistance< MeasurementVectorType > DistanceMetricType ;
 
  itkStaticConstMacro(RangeDomainDimension, 
                      unsigned int, 
                      itk::PixelTraits< 
                      typename TImage::PixelType >::Dimension) ;

  typedef FixedArray< RangeDomainMeasurementType, 
                      itkGetStaticConstMacro( RangeDomainDimension ) > 
  RangeDomainMeasurementVectorType ;

  typedef ListSample< MeasurementVectorType > CacheType ;

  typedef std::vector< InstanceIdentifier > InstanceIdentifierVectorType ; 

  typedef FixedArray< float, itkGetStaticConstMacro(MeasurementVectorSize) >
  NormalizationFactorsType ;
  
  void SetNormalizationFactors(NormalizationFactorsType& factors) ;

  inline MeasurementVectorType GetMeasurementVector(const InstanceIdentifier &id) ;
  inline void ComputeRegion(const MeasurementVectorType& mv, 
                            const double radius,
                            ImageRegionType& region) ;

  inline void Search(const MeasurementVectorType& mv, 
                     const double radius, 
                     InstanceIdentifierVectorType& result) ;

  void GenerateCache() ;

protected:
  JointDomainImageToListAdaptor() ;
  virtual ~JointDomainImageToListAdaptor() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

private:
  JointDomainImageToListAdaptor(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  NormalizationFactorsType m_NormalizationFactors ;
  typename DistanceMetricType::Pointer m_DistanceMetric ;
  int m_IndexSpaceRadius[TImage::ImageDimension] ;
  double m_PreviousRadius ;

  typename CacheType::Pointer m_Cache ;
  bool m_CacheAvailable ;

  MeasurementVectorType m_TempVector ;
  RangeDomainMeasurementVectorType m_TempRangeVector ;
} ; // end of class JointDomainImageToListAdaptor

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointDomainImageToListAdaptor.txx"
#endif

#endif
