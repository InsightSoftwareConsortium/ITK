/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSegmentationLevelSetFunction_h
#define itkSegmentationLevelSetFunction_h

#include "itkLevelSetFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkCastImageFilter.h"

namespace itk
{
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
 * \ingroup ITKLevelSets
 */

template <typename TImageType, typename TFeatureImageType = TImageType>
class ITK_TEMPLATE_EXPORT SegmentationLevelSetFunction : public LevelSetFunction<TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SegmentationLevelSetFunction);

  /** Standard class type aliases. */
  using Self = SegmentationLevelSetFunction;
  using Superclass = LevelSetFunction<TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(SegmentationLevelSetFunction, LevelSetFunction);

  /** Extract some parameters from the superclass. */
  using ImageType = typename Superclass::ImageType;
  using RadiusType = typename Superclass::RadiusType;
  using PixelRealType = typename Superclass::PixelRealType;
  using FeatureImageType = TFeatureImageType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using ScalarValueType = typename Superclass::ScalarValueType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FeatureScalarType = typename FeatureImageType::PixelType;
  using IndexType = typename ImageType::IndexType;
  using VectorType = typename Superclass::VectorType;
  using GlobalDataStruct = typename Superclass::GlobalDataStruct;

  /** Extract some parameters from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Define an image type for the advection field. */
  using VectorImageType = Image<VectorType, Self::ImageDimension>;

  /** Define a scalar interpolator */
  using InterpolatorType = LinearInterpolateImageFunction<ImageType>;

  /** Define a vector interpolator */
  using VectorInterpolatorType = VectorLinearInterpolateImageFunction<VectorImageType>;

  /** Continuous index type recognized by the interpolator */
  using ContinuousIndexType = typename InterpolatorType::ContinuousIndexType;

  /** Set/Get the image which will be used to calculate the speed function. */
  virtual const FeatureImageType *
  GetFeatureImage() const
  {
    return m_FeatureImage.GetPointer();
  }
  virtual void
  SetFeatureImage(const FeatureImageType * f)
  {
    m_FeatureImage = f;
  }

  /** Get/Set the image used as the speed function in the level set equation */
  virtual ImageType *
  GetSpeedImage()
  {
    return m_SpeedImage.GetPointer();
  }
  void
  SetSpeedImage(ImageType * s);

  /** Get/Set the image used as the advection field in the level set equation */
  virtual VectorImageType *
  GetAdvectionImage() const
  {
    return m_AdvectionImage.GetPointer();
  }
  void
  SetAdvectionImage(VectorImageType * s);

  /** This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations. */
  void
  Initialize(const RadiusType & r) override;

  /** This method must be defined in a subclass to implement a working function
   * object.  This method is called before the solver begins its work to
   * produce the speed image used as the level set function's Propagation speed
   * term.  See LevelSetFunction for more information. */
  virtual void
  CalculateSpeedImage()
  {}

  /** This method must be defined in a subclass to implement a working function
   * object.  This method is called before the solver begins its work to
   * produce the speed image used as the level set function's Advection field
   * term.  See LevelSetFunction for more information. */
  virtual void
  CalculateAdvectionImage()
  {}

  /** Allocates the image that will be used for the level set function's
   * Propagation Speed term.  See LevelSetFunction for more information. */
  virtual void
  AllocateSpeedImage();

  /** Allocates the image that will be used for the level set function's
   * Advection field term.  See LevelSetFunction for more information. */
  virtual void
  AllocateAdvectionImage();

  /** Determines whether Positive or Negative speed terms will cause surface
   * expansion.  This method flips the sign of all of the speed, advection, etc
   * terms.  By convention, filters should be written so that POSITIVE speed
   * terms cause surface expansion.  Calling this method will
   * toggle between the standard POSITIVE EXPANSION convention and the
   * nonstandard NEGATIVE EXPANSION convention.
   *
   * IMPORTANT:  When adding terms to the level-set equation through
   * subclassing you may need to override this function so that your new terms
   * will be properly adjusted. */
  virtual void
  ReverseExpansionDirection();

protected:
  /** The image whose features will be used to create a speed image */
  typename FeatureImageType::ConstPointer m_FeatureImage;

  /** The image holding the speed values for front propagation */
  typename ImageType::Pointer m_SpeedImage;

  /** The image holding the advection field for front propagation */
  typename VectorImageType::Pointer m_AdvectionImage;

  /** Returns the propagation speed from the precalculated speed image.*/
  ScalarValueType
  PropagationSpeed(const NeighborhoodType &, const FloatOffsetType &, GlobalDataStruct * gd) const override;

  /** Advection field.  Returns a vector from the computed advectionfield.*/
  VectorType
  AdvectionField(const NeighborhoodType &, const FloatOffsetType &, GlobalDataStruct * gd) const override;

  ~SegmentationLevelSetFunction() override = default;
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
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSegmentationLevelSetFunction.hxx"
#endif

#endif
