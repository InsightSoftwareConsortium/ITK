/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMahalanobisDistanceThresholdImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMahalanobisDistanceThresholdImageFunction_h
#define _itkMahalanobisDistanceThresholdImageFunction_h

#include "itkImageFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"

namespace itk
{

/** \class MahalanobisDistanceThresholdImageFunction
 * \brief Returns true if the pixel value of a vector image has a
 * Mahalanobis distance below the value specified by the threshold.
 *
 * This ImageFunction returns true if the pixel value of a vector image has a
 * Mahalanobis distance below the value specided by the threshold. The
 * Mahalanobis distance is computed with the
 * MahalanobisDistanceMembershipFunction class which has to be initialized with
 * the mean an covariance to be used. This class is intended to be used only
 * with images whose pixel type is a vector (array).
 *
 * The input image is set via method SetInputImage().
 *
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex respectively
 * evaluate the function at an geometric point, image index and continuous
 * image index.
 *
 * \ingroup ImageFunctions
 * 
 * */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT MahalanobisDistanceThresholdImageFunction : 
  public ImageFunction<TInputImage,bool,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef MahalanobisDistanceThresholdImageFunction Self;
  typedef ImageFunction<TInputImage,bool,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(MahalanobisDistanceThresholdImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;
  
  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType PixelType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Type used to represent the Covariance matrix of the vector population */
  typedef vnl_matrix<double> CovarianceMatrixType;
  
  /** Type used to represent the Mean Vector of the vector population */
  typedef vnl_vector<double> MeanVectorType;
  
  /** BinaryThreshold the image at a point position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */

  virtual bool Evaluate( const PointType& point ) const
    {
    IndexType index;
    this->ConvertPointToNearestIndex( point, index );
    return ( this->EvaluateAtIndex( index ) );
    }

  /** BinaryThreshold the image at a continuous index position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const
    {
      IndexType nindex;

      this->ConvertContinuousIndexToNearestIndex (index, nindex);
      return this->EvaluateAtIndex(nindex);
    }

  /** BinaryThreshold the image at an index position.
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool EvaluateAtIndex( const IndexType & index ) const
    {
    double mahalanobisDistance = 
      m_MahalanobisDistanceMembershipFunction->Evaluate( 
                                        m_Image->GetPixel( index ) );
    return ( sqrt( mahalanobisDistance ) <= m_Threshold );
    }

  /** Get the lower threshold value. */
  itkGetConstMacro(Threshold,double);
  itkSetMacro(Threshold,double);

  /** Method to set mean */
  void SetMean(const MeanVectorType &mean);
  const MeanVectorType & GetMean() const;
 
  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of 
   * MahalanobisDistance Distribution to speed up GetProbability */
  void SetCovariance(const CovarianceMatrixType &cov); 
  const CovarianceMatrixType & GetCovariance() const;
  

protected:
  MahalanobisDistanceThresholdImageFunction();
  ~MahalanobisDistanceThresholdImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  MahalanobisDistanceThresholdImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  double  m_Threshold;

  // This is intended only for Image of Vector pixel type.
  typedef Statistics::MahalanobisDistanceMembershipFunction<
                                            PixelType 
                                              >  MahalanobisDistanceFunctionType;

  typedef typename MahalanobisDistanceFunctionType::Pointer MahalanobisDistanceFunctionPointer;
  MahalanobisDistanceFunctionPointer m_MahalanobisDistanceMembershipFunction;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMahalanobisDistanceThresholdImageFunction.txx"
#endif

#endif
