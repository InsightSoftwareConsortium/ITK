/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovarianceImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCovarianceImageFunction_h
#define _itkCovarianceImageFunction_h

#include "itkImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{

/**
 * \class CovarianceImageFunction
 * \brief Calculate the covariance matrix in the neighborhood of a pixel in a Vector image.
 *
 * Calculate the covariance matrix  over the standard 8, 26, etc. connected
 * neighborhood.  This calculation uses a ZeroFluxNeumannBoundaryCondition.
 *
 * If called with a ContinuousIndex or Point, the calculation is performed
 * at the nearest neighbor.
 *
 * This class is templated over the input image type and the
 * coordinate representation type (e.g. float or double).
 *
 * \sa VectorMeanImageFunction
 * 
 * \ingroup ImageFunctions
 */
template <class TInputImage, class TCoordRep = float >
class ITK_EXPORT CovarianceImageFunction :
  public ImageFunction< TInputImage, 
    vnl_matrix< 
      ITK_TYPENAME NumericTraits<typename TInputImage::PixelType::ValueType>::RealType >,
    TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef CovarianceImageFunction Self;
  typedef ImageFunction<TInputImage, 
    vnl_matrix< 
      ITK_TYPENAME NumericTraits<typename TInputImage::PixelType::ValueType>::RealType >,
    TCoordRep > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(CovarianceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** OutputType typdef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;
  
  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Datatype used for the covariance matrix */
  typedef vnl_matrix<
    typename NumericTraits<typename InputImageType::PixelType::ValueType>::RealType >
                                                                            RealType;

  /** Evalulate the function at specified index */
  virtual RealType EvaluateAtIndex( const IndexType& index ) const;
  
  /** Evaluate the function at non-integer positions */
  virtual RealType Evaluate( const PointType& point ) const
    { 
      IndexType index;
      this->ConvertPointToNearestIndex( point, index );
      return this->EvaluateAtIndex( index ); 
    }
  virtual RealType EvaluateAtContinuousIndex( 
    const ContinuousIndexType& cindex ) const
    { 
      IndexType index;
      this->ConvertContinuousIndexToNearestIndex( cindex, index );
      return this->EvaluateAtIndex( index ) ; 
    }
  
  /** Get/Set the radius of the neighborhood over which the
      statistics are evaluated */
  itkSetMacro( NeighborhoodRadius, unsigned int );
  itkGetConstReferenceMacro( NeighborhoodRadius, unsigned int );

protected:
  CovarianceImageFunction();
  ~CovarianceImageFunction(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  CovarianceImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  unsigned int m_NeighborhoodRadius;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovarianceImageFunction.txx"
#endif

#endif

