/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkVectorCastImageFilter.h"

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

template< typename TImageType, typename TFeatureImageType = TImageType >
class ITK_TEMPLATE_EXPORT SegmentationLevelSetFunction:
  public LevelSetFunction< TImageType >
{
public:
  /** Standard class typedefs. */
  typedef SegmentationLevelSetFunction   Self;
  typedef LevelSetFunction< TImageType > Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(SegmentationLevelSetFunction, LevelSetFunction);

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::PixelRealType    PixelRealType;
  typedef TFeatureImageType                     FeatureImageType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  typedef typename Superclass::ScalarValueType  ScalarValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename FeatureImageType::PixelType  FeatureScalarType;
  typedef typename ImageType::IndexType         IndexType;
  typedef typename Superclass::VectorType       VectorType;
  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Define an image type for the advection field. */
  typedef Image< VectorType, itkGetStaticConstMacro(ImageDimension) > VectorImageType;

  /** Define a scalar interpolator */
  typedef LinearInterpolateImageFunction< ImageType > InterpolatorType;

  /** Define a vector interpolator */
  typedef VectorLinearInterpolateImageFunction< VectorImageType > VectorInterpolatorType;

  /** Continuous index type recognized by the interpolator */
  typedef typename InterpolatorType::ContinuousIndexType ContinuousIndexType;

  /** Set/Get the image which will be used to calculate the speed function. */
  virtual const FeatureImageType * GetFeatureImage() const
  { return m_FeatureImage.GetPointer(); }
  virtual void SetFeatureImage(const FeatureImageType *f)
  {    m_FeatureImage = f;  }

  /** Get/Set the image used as the speed function in the level set equation */
  virtual ImageType * GetSpeedImage()
  { return m_SpeedImage.GetPointer(); }
  void SetSpeedImage(ImageType *s);

  /** Get/Set the image used as the advection field in the level set equation */
  virtual VectorImageType * GetAdvectionImage() const
  { return m_AdvectionImage.GetPointer(); }
  void SetAdvectionImage(VectorImageType *s);

  /** This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations. */
  virtual void Initialize(const RadiusType & r) ITK_OVERRIDE;

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
  virtual void ReverseExpansionDirection();

protected:
  /** The image whose features will be used to create a speed image */
  typename FeatureImageType::ConstPointer m_FeatureImage;

  /** The image holding the speed values for front propagation */
  typename ImageType::Pointer m_SpeedImage;

  /** The image holding the advection field for front propation */
  typename VectorImageType::Pointer m_AdvectionImage;

  /** A casting functor to convert between vector types.  */
  Functor::VectorCast< typename VectorInterpolatorType::OutputType,
                       VectorType > m_VectorCast;

  /** Returns the propagation speed from the precalculated speed image.*/
  virtual ScalarValueType PropagationSpeed(const NeighborhoodType &,
                                           const FloatOffsetType &, GlobalDataStruct *gd) const ITK_OVERRIDE;

  /** Advection field.  Returns a vector from the computed advectionfield.*/
  virtual VectorType AdvectionField(const NeighborhoodType &,
                                    const FloatOffsetType &, GlobalDataStruct *gd) const ITK_OVERRIDE;

  virtual ~SegmentationLevelSetFunction() ITK_OVERRIDE {}
  SegmentationLevelSetFunction()
  {
    m_SpeedImage = ImageType::New();
    m_AdvectionImage = VectorImageType::New();
    m_Interpolator = InterpolatorType::New();
    m_VectorInterpolator = VectorInterpolatorType::New();
  }

  typename InterpolatorType::Pointer m_Interpolator;

  typename VectorInterpolatorType::Pointer m_VectorInterpolator;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SegmentationLevelSetFunction);
};
} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationLevelSetFunction.hxx"
#endif

#endif
