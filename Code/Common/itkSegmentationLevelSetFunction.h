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

  //  itkNewMacro(Self);
  
  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef TFeatureImageType FeatureImageType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename FeatureImageType::PixelType FeatureScalarType;
  typedef typename ImageType::IndexType IndexType;


  /** Define an interpolator */
  typedef LinearInterpolateImageFunction<FeatureImageType>  InterpolatorType;

  /** Continuous index type recognized by the interpolator */
  typedef typename InterpolatorType::ContinuousIndexType ContinuousIndexType;
  
  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Set/Get the image which will be used to calculate the speed function. */
  virtual FeatureImageType *GetFeatureImage() const
  { return m_FeatureImage.GetPointer(); }
  virtual void SetFeatureImage(FeatureImageType *f)
  {    m_FeatureImage = f;  }
  
  /** Get the image used as the speed function in the level set equation */
  virtual ImageType *GetSpeedImage() const
  { return m_SpeedImage.GetPointer(); }

  /** This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations. */
  virtual void Initialize(const RadiusType &r);

  /** This method must be defined in a subclass to implement a working function
   * object.  This method is called before the solver begins its work to
   * produce the speed image used as the level set function's Propagation speed
   * term.  See LevelSetFunction for more information. */
  virtual void CalculateSpeedImage() = 0;

  /** Allocates the image that will be used for the level set function's
   * Propagation Speed term.  See LevelSetFunction for more information. */
  virtual void AllocateSpeedImage();
  
protected:
  /** The image whose features will be used to create a speed image */
  typename FeatureImageType::Pointer m_FeatureImage;
  typename ImageType::Pointer        m_SpeedImage;

  /** Returns the propagation speed from the precalculated speed image.*/
  virtual ScalarValueType PropagationSpeed(const NeighborhoodType&,
                                           const FloatOffsetType&) const;

  /** Returns the propagation speed from the precalculated speed image. */
  virtual ScalarValueType PropagationSpeed(const BoundaryNeighborhoodType&,
                                           const FloatOffsetType &) const;

  virtual ~SegmentationLevelSetFunction() {}
  SegmentationLevelSetFunction()
  {
    m_SpeedImage = ImageType::New();
    m_Interpolator = InterpolatorType::New();
  }

  typename InterpolatorType::Pointer m_Interpolator;
  
};

} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationLevelSetFunction.txx"
#endif

#endif
