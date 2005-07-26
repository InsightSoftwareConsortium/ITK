/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJointDomainImageToListAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkEuclideanDistance.h"
#include "itkListSample.h"

namespace itk{ 
namespace Statistics{

/** \class ImageJointDomainTraits
 *  \brief This class provides the type defintion for the measurement
 *  vector in the joint domain (range domain -- pixel values + spatial
 *  domain -- pixel's physical coordinates).
 *
 * \sa JointDomainImageToListAdaptor
 */
template< class TImage >
struct ImageJointDomainTraits
{
  typedef ImageJointDomainTraits Self ;
  typedef PixelTraits< typename TImage::PixelType > PixelTraitsType ;
  typedef typename PixelTraitsType::ValueType RangeDomainMeasurementType ;
  typedef typename TImage::IndexType::IndexValueType IndexValueType ;
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension) ;
  itkStaticConstMacro(Dimension, 
                      unsigned int, 
                      TImage::ImageDimension +
                      PixelTraitsType::Dimension ) ;
  typedef float CoordinateRepType ;
  typedef Point< CoordinateRepType, itkGetStaticConstMacro(ImageDimension) > PointType ;
  typedef JoinTraits< RangeDomainMeasurementType, CoordinateRepType > 
  JoinTraitsType ;
  typedef typename JoinTraitsType::ValueType MeasurementType ;
  typedef FixedArray< MeasurementType, itkGetStaticConstMacro(Dimension) >
  MeasurementVectorType ;
} ; // end of ImageJointDomainTraits

/** \class JointDomainImageToListAdaptor
 *  \brief This adaptor returns measurement vectors composed of an
 *  image pixel's range domain value (pixel value) and spatial domain
 *  value (pixel's physical coordiantes).
 *
 * This class is a derived class of the ImageToListAdaptor. This class
 * overrides the GetMeasurementVector method. The GetMeasurementVector
 * returns a measurement vector that consist of a pixel's physical
 * coordinates and intensity value. For example, if the image
 * dimension is 3, and the pixel value is two component vector, the
 * measurement vector is a 5 component vector. The first three
 * component will be x, y, z physical coordinates (not index) and the
 * rest two component is the pixel values. The type of component is
 * float or which is determined by the ImageJointDomainTraits
 * class. When the pixel value type is double, the component value
 * type of a measurement vector is double. In other case, the
 * component value type is float becase the physical coordinate value
 * type is float. Since the measurment vector is a composition of
 * spatial domain and range domain, for many statistical analysis, we
 * want to normalize the values from both domains. For this purpose,
 * there is the SetNormalizationFactors method. With the above example
 * (5 component measurement vector), you can specify a 5 component
 * normalization factor array. With such factors, the
 * GetMeasurementVector method returns a measurement vector whose each
 * component is divided by the corresponding component of the factor array. 
 *
 * \sa Sample, ListSampleBase, ImageToListAdaptor
 */

template < class TImage >
class ITK_EXPORT JointDomainImageToListAdaptor 
  : public ImageToListAdaptor< 
  TImage, 
  typename ImageJointDomainTraits< TImage >::MeasurementVectorType >
{
public:
  typedef ImageJointDomainTraits< TImage > ImageJointDomainTraitsType ;
  typedef typename ImageJointDomainTraitsType::MeasurementVectorType
  MeasurementVectorType ;
  typedef typename ImageJointDomainTraitsType::MeasurementType
  MeasurementType ;
  typedef typename ImageJointDomainTraitsType::RangeDomainMeasurementType
  RangeDomainMeasurementType ;
  typedef typename ImageJointDomainTraitsType::PointType PointType ;
  typedef typename ImageJointDomainTraitsType::CoordinateRepType 
  CoordinateRepType ;
  /** Standard class typedefs */
  typedef JointDomainImageToListAdaptor Self;
  typedef ImageToListAdaptor< TImage, MeasurementVectorType > 
  Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(JointDomainImageToListAdaptor, ImageToListAdaptor) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  
  /** the number of components in a measurement vector */
  itkStaticConstMacro(MeasurementVectorSize, 
                      unsigned int, 
                      ImageJointDomainTraitsType::Dimension) ;
  
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;

  virtual void SetMeasurementVectorSize( const MeasurementVectorSizeType s ) 
    {
    // Measurement vector size for this class is fixed as the pixel's 
    // dimension. This method should have no effect
    if( s != MeasurementVectorSize )
      {
      itkExceptionMacro( << "Cannot set measurement vector size of "
          << " JointDomainImageToListAdaptor to " << s );
      }
    }

  MeasurementVectorSizeType GetMeasurementVectorSize() const
    {
    return MeasurementVectorSize;
    } 
 
  /** typedefs for Measurement vector, measurement, 
   * Instance Identifier, frequency, size, size element value */
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  typedef typename TImage::IndexType ImageIndexType ;
  typedef typename TImage::IndexType::IndexValueType ImageIndexValueType ;
  typedef typename TImage::SizeType ImageSizeType ;
  typedef typename TImage::RegionType ImageRegionType ;
  typedef ImageRegionConstIteratorWithIndex< TImage > ImageIteratorType ;

  typedef MeasurementVectorType ValueType ;
 
  itkStaticConstMacro(RangeDomainDimension, 
                      unsigned int, 
                      itk::PixelTraits< 
                      typename TImage::PixelType >::Dimension) ;

  typedef FixedArray< RangeDomainMeasurementType, 
                      itkGetStaticConstMacro( RangeDomainDimension ) > 
  RangeDomainMeasurementVectorType ;

  typedef std::vector< InstanceIdentifier > InstanceIdentifierVectorType ; 

  typedef FixedArray< float, itkGetStaticConstMacro(MeasurementVectorSize) >
  NormalizationFactorsType ;

  /** Sets the normalization factors */
  void SetNormalizationFactors(NormalizationFactorsType& factors) ;

  /** Gets the measurement vector specified by the instance
   * identifier. This method overrides superclass method. */
  inline const MeasurementVectorType & GetMeasurementVector(const InstanceIdentifier &id) const;

  /** Computes the image region (rectangular) that enclose the ball
   * defined by the mv (center) and the radius. */
  inline void ComputeRegion(const MeasurementVectorType& mv, 
                            const double radius,
                            ImageRegionType& region) const;

  /** Fills he result id vectors with instances that fall within a
   * ball specified by the mv (center) and radius. This method utilizes
   * the ComputRegion */
  inline void Search(const MeasurementVectorType& mv, 
                     const double radius, 
                     InstanceIdentifierVectorType& result) const;

protected:
  JointDomainImageToListAdaptor() ;
  virtual ~JointDomainImageToListAdaptor() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

private:
  JointDomainImageToListAdaptor(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  NormalizationFactorsType m_NormalizationFactors ;

  mutable MeasurementVectorType m_TempVector ;
  mutable PointType m_TempPoint ;
  mutable ImageIndexType m_TempIndex ;
  mutable RangeDomainMeasurementVectorType m_TempRangeVector ;
} ; // end of class JointDomainImageToListAdaptor

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJointDomainImageToListAdaptor.txx"
#endif

#endif
