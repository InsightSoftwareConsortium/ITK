/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectToImageStatisticsCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectToImageStatisticsCalculator_h
#define __itkSpatialObjectToImageStatisticsCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"

namespace itk
{

/** This calculator computes the mean and the covariance matrice of a certain
 *  region of an image specified by a spatial object.
 * \ingroup Operators
 */
template <class TInputImage, class TInputSpatialObject, unsigned int TSampleDimension=1>            
class ITK_EXPORT SpatialObjectToImageStatisticsCalculator : public Object 
{
public:
  /** Standard class typedefs. */
  typedef SpatialObjectToImageStatisticsCalculator Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Self, Object);

  /** Type definitions for the input image. */
  typedef TInputImage  ImageType;
  typedef typename TInputImage::Pointer  ImagePointer;
  typedef typename TInputImage::ConstPointer ImageConstPointer;
  typedef typename TInputImage::PixelType PixelType;
  typedef typename TInputImage::IndexType IndexType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      ImageType::ImageDimension);
  
  itkStaticConstMacro(SampleDimension, unsigned int,
                      TSampleDimension);

  /** Type definitions for the input spatial object. */
  typedef TInputSpatialObject SpatialObjectType;  
  typedef typename SpatialObjectType::Pointer  SpatialObjectPointer;
  typedef typename SpatialObjectType::ConstPointer SpatialObjectConstPointer;

  /** Type definition of the flood fill iterator */
  typedef itk::FloodFilledSpatialFunctionConditionalConstIterator<ImageType,
                                                 SpatialObjectType> IteratorType;
 
  /** Vector and Matrix Type */
  typedef Vector< double, TSampleDimension> VectorType;
  typedef Matrix< double, TSampleDimension, TSampleDimension > MatrixType ;

  /** Set/Get the direction of the sample */
  itkSetMacro(SampleDirection,unsigned int);
  itkGetMacro(SampleDirection,unsigned int);

  /** Set the input image. */
  itkSetConstObjectMacro(Image,ImageType);

  /** Set the input spatial object. */
  itkSetObjectMacro(SpatialObject,SpatialObjectType);

  /** Get the mean */
  const VectorType & GetMean() const {return m_Mean;}

  /** Get the covariance matrix */
  const MatrixType & GetCovarianceMatrix() const {return m_CovarianceMatrix;}

  /** Compute of the input image. */
  void Update(void);

protected:
  SpatialObjectToImageStatisticsCalculator();
  virtual ~SpatialObjectToImageStatisticsCalculator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SpatialObjectToImageStatisticsCalculator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ImageConstPointer          m_Image;
  SpatialObjectPointer       m_SpatialObject;
  VectorType                 m_Mean;
  MatrixType                 m_CovarianceMatrix;
  unsigned int               m_SampleDirection;
  unsigned long              m_InternalImageTime;
  unsigned long              m_InternalSpatialObjectTime;
  TimeStamp                  m_ModifiedTime;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectToImageStatisticsCalculator.txx"
#endif

#endif /* __itkSpatialObjectToImageStatisticsCalculator_h */
