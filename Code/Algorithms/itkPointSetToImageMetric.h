/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToImageMetric_h
#define __itkPointSetToImageMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkInterpolateImageFunction.h"
#include "itkSingleValuedCostFunction.h"
#include "itkExceptionObject.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{
  
/** \class PointSetToImageMetric
 * \brief Computes similarity between a point set and an image.
 *
 * This Class is templated over the type of the input point-set and image.  It
 * expects a Transform and an Interpolator to be plugged in.  This particular
 * class is the base class for a hierarchy of similarity metrics.
 *
 * This class computes a value that measures the similarity between the values
 * associated with points in the point set and the transformed Moving image.
 * The Interpolator is used to compute intensity values on non-grid positions
 * resulting from mapping points through the Transform.
 *
 * \ingroup RegistrationMetrics
 *
 */

template <class TFixedPointSet,  class TMovingImage> 
class ITK_EXPORT PointSetToImageMetric : public SingleValuedCostFunction 
{
public:
  /** Standard class typedefs. */
  typedef PointSetToImageMetric           Self;
  typedef SingleValuedCostFunction        Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Type used for representing point components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToImageMetric, SingleValuedCostFunction);

  /**  Type of the moving Image. */
  typedef TMovingImage                               MovingImageType;
  typedef typename TMovingImage::PixelType           MovingImagePixelType;
  typedef typename MovingImageType::ConstPointer     MovingImageConstPointer;

  /**  Type of the fixed Image. */
  typedef TFixedPointSet                             FixedPointSetType;
  typedef typename FixedPointSetType::ConstPointer   FixedPointSetConstPointer;

  /** Constants for the image dimensions */
  itkStaticConstMacro(MovingImageDimension, unsigned int,
                      TMovingImage::ImageDimension);
  itkStaticConstMacro(FixedPointSetDimension, unsigned int,
                      TFixedPointSet::PointDimension);
  
  typedef typename FixedPointSetType::PointsContainer::ConstIterator        PointIterator;
  typedef typename FixedPointSetType::PointDataContainer::ConstIterator     PointDataIterator;

  /**  Type of the Transform Base class */
  typedef Transform<CoordinateRepresentationType, 
                    itkGetStaticConstMacro(MovingImageDimension),
                    itkGetStaticConstMacro(FixedPointSetDimension)> TransformType;

  typedef typename TransformType::Pointer            TransformPointer;
  typedef typename TransformType::InputPointType     InputPointType;
  typedef typename TransformType::OutputPointType    OutputPointType;
  typedef typename TransformType::ParametersType     TransformParametersType;
  typedef typename TransformType::JacobianType       TransformJacobianType;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction<
                      MovingImageType,
                      CoordinateRepresentationType > InterpolatorType;


  /** Gaussian filter to compute the gradient of the Moving Image */
  typedef typename NumericTraits<MovingImagePixelType>::RealType RealType;
  typedef CovariantVector<RealType,
          itkGetStaticConstMacro(MovingImageDimension)> GradientPixelType;
  typedef Image<GradientPixelType,
               itkGetStaticConstMacro(MovingImageDimension)> GradientImageType;
  typedef SmartPointer<GradientImageType>     GradientImagePointer;
  typedef GradientRecursiveGaussianImageFilter< MovingImageType,
                                                GradientImageType >
          GradientImageFilterType;  
  typedef typename GradientImageFilterType::Pointer GradientImageFilterPointer;


  typedef typename InterpolatorType::Pointer         InterpolatorPointer;

  /**  Type of the measure. */
  typedef Superclass::MeasureType                    MeasureType;

  /**  Type of the derivative. */
  typedef Superclass::DerivativeType                 DerivativeType;

  /**  Type of the parameters. */
  typedef Superclass::ParametersType                 ParametersType;

  /** Connect the Fixed Image.  */
  itkSetConstObjectMacro( FixedPointSet, FixedPointSetType );

  /** Get the Fixed Image. */
  itkGetConstObjectMacro( FixedPointSet, FixedPointSetType );

  /** Connect the Moving Image.  */
  itkSetConstObjectMacro( MovingImage, MovingImageType );

  /** Get the Moving Image. */
  itkGetConstObjectMacro( MovingImage, MovingImageType );

  /** Connect the Transform. */
  itkSetObjectMacro( Transform, TransformType );

  /** Get a pointer to the Transform.  */
  itkGetObjectMacro( Transform, TransformType );
 
  /** Connect the Interpolator. */
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /** Get a pointer to the Interpolator.  */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** Get the number of pixels considered in the computation. */
  itkGetConstMacro( NumberOfPixelsCounted, unsigned long );

  /** Set the parameters defining the Transform. */
  void SetTransformParameters( const ParametersType & parameters ) const;

  /** Return the number of parameters required by the Transform */
  unsigned int GetNumberOfParameters(void) const 
    { return m_Transform->GetNumberOfParameters(); }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void Initialize(void) throw ( ExceptionObject );

  /** Set/Get the sigma value used for computing the 
      gradient image with a derivative of a Gaussian filter */
  itkSetMacro( ScaleGradient, RealType );
  itkGetMacro( ScaleGradient, RealType );

protected:
  PointSetToImageMetric();
  virtual ~PointSetToImageMetric() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  mutable unsigned long       m_NumberOfPixelsCounted;

  FixedPointSetConstPointer   m_FixedPointSet;
  MovingImageConstPointer     m_MovingImage;

  mutable TransformPointer    m_Transform;
  InterpolatorPointer         m_Interpolator;

  bool                        m_ComputeGradient;
  GradientImagePointer        m_GradientImage;

private:
  PointSetToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  // This is the Sigma value to be used by the Gradient Filter
  RealType                    m_ScaleGradient;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToImageMetric.txx"
#endif

#endif



