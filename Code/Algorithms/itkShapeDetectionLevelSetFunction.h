/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeDetectionLevelSetFunction_h_
#define __itkShapeDetectionLevelSetFunction_h_

#include "itkSegmentationLevelSetFunction.h"

namespace itk {

/** \class ShapeDetectionLevelSetFunction
 *    
 * \brief This function is used in ShapeDetectionLevelSetImageFilter to
 * segment structures in images based on user supplied edge potential map.
 *
 * \par ShapeDetectionLevelSetFunction is a subclass of the generic LevelSetFunction.
 * It is useful for segmentations based on a user supplied edge potential map which
 * has values close to zero in regions near edges (or high image gradient) and values
 * close to one in regions with relatively constant intensity. Typically, the edge
 * potential map is a function of the gradient, for example:
 *
 * \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 * \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 * 
 * where \f$ I \f$ is image intensity and
 * \f$ (\nabla * G) \f$ is the derivative of Gaussian operator. 
 *
 * \par In this function both the propagation term \f$ P(\mathbf{x}) \f$
 * and the curvature spatial modifier term \f$ Z(\mathbf{x}) \f$ are taken directly
 * from the edge potential image. The edge potential image is set via the
 * SetFeatureImage() method. Note that there is no advection term in this function.
 *
 * \par This implementation is based on:
 * "Shape Modeling with Front Propagation: A Level Set Approach",
 * R. Malladi, J. A. Sethian and B. C. Vermuri.
 * IEEE Trans. on Pattern Analysis and Machine Intelligence,
 * Vol 17, No. 2, pp 158-174, February 1995
 *
 * \sa LevelSetFunction
 * \sa SegmentationLevelSetImageFunction
 * \sa ShapeDetectionLevelSetImageFilter
 *
 * \ingroup FiniteDifferenceFunctions
 */
template <class TImageType, class TFeatureImageType = TImageType>
class ITK_EXPORT ShapeDetectionLevelSetFunction
  : public SegmentationLevelSetFunction<TImageType, TFeatureImageType>
{
public:
  /** Standard class typedefs. */
  typedef ShapeDetectionLevelSetFunction Self;
  typedef SegmentationLevelSetFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  typedef TFeatureImageType FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ShapeDetectionLevelSetFunction, SegmentationLevelSetFunction );

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  virtual void CalculateSpeedImage();

  /** The curvature speed is same as the propagation speed. */
  virtual ScalarValueType CurvatureSpeed(const NeighborhoodType & neighborhood,
                                         const FloatOffsetType & offset ) const
  { return PropagationSpeed( neighborhood, offset ); }

  virtual void Initialize(const RadiusType &r)
  {
    Superclass::Initialize(r);
    
    this->SetAdvectionWeight( NumericTraits<ScalarValueType>::Zero );
    this->SetPropagationWeight( NumericTraits<ScalarValueType>::One );
    this->SetCurvatureWeight( NumericTraits<ScalarValueType>::One );
  }
  
protected:
  ShapeDetectionLevelSetFunction()
  {
    this->SetAdvectionWeight( NumericTraits<ScalarValueType>::Zero );
    this->SetPropagationWeight( NumericTraits<ScalarValueType>::One );
    this->SetCurvatureWeight( NumericTraits<ScalarValueType>::One );
  }
  virtual ~ShapeDetectionLevelSetFunction() {}

  ShapeDetectionLevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent );
  }
  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapeDetectionLevelSetFunction.txx"
#endif

#endif
