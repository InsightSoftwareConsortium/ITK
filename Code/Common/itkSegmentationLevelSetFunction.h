/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetFunction_h_
#define __itkSegmentationLevelSetFunction_h_

#include "itkLevelSetFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkVectorCastImageFilter.h"

namespace itk {

/** \class SegmentationLevelSetFunction

  \par
  This object defines the API for a class of function objects which perform
  level set based segmentations.  The SegmentationLevelSetImageFilter objects
  use these SegmentationLevelSetFunction objects to perform the numerical
  calculations which move a level set front to lock onto image features.

  \par
  In order to create a working function object, you must subclass the
  CalculateSpeedImage method to produce a "feature image" that is used by the
  parent LevelSetFunction class as the PropagationSpeed for its calculations.

  \sa SegmentationLevelSetImageFilter
  \sa LevelSetFunction
*/
  
template <class TImageType, class TFeatureImageType = TImageType>
class ITK_EXPORT SegmentationLevelSetFunction
  : public LevelSetFunction<TImageType>
{
public:
  /** Standard class typedefs. */
  typedef SegmentationLevelSetFunction Self;
  typedef LevelSetFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( SegmentationLevelSetFunction, LevelSetFunction );

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef TFeatureImageType FeatureImageType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename FeatureImageType::PixelType FeatureScalarType;
  typedef typename ImageType::IndexType IndexType;
  typedef typename Superclass::VectorType VectorType;

  /** Define an image type for the advection field. */
  typedef Image<VectorType, 
                ::itk::GetImageDimension<TImageType>::ImageDimension > VectorImageType;


  /** Define a scalar interpolator */
  typedef LinearInterpolateImageFunction<FeatureImageType>  InterpolatorType;

  /** Define a vector interpolator */
  typedef VectorLinearInterpolateImageFunction<VectorImageType> VectorInterpolatorType;
  
  /** Continuous index type recognized by the interpolator */
  typedef typename InterpolatorType::ContinuousIndexType ContinuousIndexType;
  
  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Set/Get the image which will be used to calculate the speed function. */
  virtual const FeatureImageType *GetFeatureImage() const
  { return m_FeatureImage.GetPointer(); }
  virtual void SetFeatureImage(const FeatureImageType *f)
  {    m_FeatureImage = f;  }
  
  /** Get the image used as the speed function in the level set equation */
  virtual ImageType *GetSpeedImage() 
  { return m_SpeedImage.GetPointer(); }

  /** Get the image used as the advection field in the level set equation */
  virtual VectorImageType *GetAdvectionImage() const
  { return m_AdvectionImage.GetPointer(); } 

  /** This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations. */
  virtual void Initialize(const RadiusType &r);

  /** This method must be defined in a subclass to implement a working function
   * object.  This method is called before the solver begins its work to
   * produce the speed image used as the level set function's Propagation speed
   * term.  See LevelSetFunction for more information. */
  virtual void CalculateSpeedImage() {}

  /** This method must be defined in a subclass to implement a working function
   * object.  This method is called before the solver begins its work to
   * produce the speed image used as the level set function's Advection field
   * term.  See LevelSetFunction for more information. */
  virtual void CalculateAdvectionImage() {}

  /** Allocates the image that will be used for the level set function's
   * Propagation Speed term.  See LevelSetFunction for more information. */
  virtual void AllocateSpeedImage();

  /** Allocates the image that will be used for the level set function's
   * Advection field term.  See LevelSetFunction for more information. */
  virtual void AllocateAdvectionImage();

protected:
  /** The image whose features will be used to create a speed image */
  typename FeatureImageType::ConstPointer m_FeatureImage;

  /** The image holding the speed values for front propagation */
  typename ImageType::Pointer        m_SpeedImage;

  /** The image holding the advection field for front propation */
  typename VectorImageType::Pointer  m_AdvectionImage;

  /** A casting functor to convert between vector types.  */
  Functor::VectorCast< ITK_TYPENAME VectorInterpolatorType::OutputType,
                       VectorType > m_VectorCast;

  /** Returns the propagation speed from the precalculated speed image.*/
  virtual ScalarValueType PropagationSpeed(const NeighborhoodType &,
                                           const FloatOffsetType &) const;

  /** Advection field.  Returns a vector from the computed advectionfield.*/
  virtual VectorType AdvectionField(const NeighborhoodType &,
                                    const FloatOffsetType &) const;
  
  
  virtual ~SegmentationLevelSetFunction() {}
  SegmentationLevelSetFunction()
  {
    m_SpeedImage = ImageType::New();
    m_AdvectionImage = VectorImageType::New();
    m_Interpolator = InterpolatorType::New();
    m_VectorInterpolator = VectorInterpolatorType::New();
  }

  typename InterpolatorType::Pointer m_Interpolator;
  typename VectorInterpolatorType::Pointer m_VectorInterpolator;
  
};

} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationLevelSetFunction.txx"
#endif

#endif
