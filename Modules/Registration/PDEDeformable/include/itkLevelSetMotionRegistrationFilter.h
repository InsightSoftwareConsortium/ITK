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
#ifndef itkLevelSetMotionRegistrationFilter_h
#define itkLevelSetMotionRegistrationFilter_h

#include "itkPDEDeformableRegistrationFilter.h"
#include "itkLevelSetMotionRegistrationFunction.h"

namespace itk
{
/** \class LevelSetMotionRegistrationFilter
 * \brief Deformably register two images using level set motion.
 *
 * LevelSetMotionFilter implements a deformable registration algorithm that
 * aligns a fixed and a moving image under level set motion. The
 * equations of motion are similar to those of the
 * DemonsRegistrationFilter. The main differences are:
 *    (1) Gradients of the moving image are calculated on a smoothed
 *    image while intensity difference are measured on the original images
 *    (2) Magnitude of the motion vector is a function of the
 *    differences in intensity between the fixed and moving pixel. An
 *    adaptive timestep is calculated based on the maximum motion
 *    vector over the entire field to ensure stability.  The timestep
 *    also implictly converts the motion vector measured in units of
 *    intensity to a vector measured in physical units.  Demons, on
 *    the other hand, defines its motion vectors as function of both
 *    the intensity differences and gradient magnitude at each
 *    respective pixel.  Consider two separate pixels with the same
 *    intensity differences between the corresponding fixed and moving
 *    pixel pairs.  In demons, the motion vector of the pixel over a low
 *    gradient region will be larger than the motion vector of the
 *    pixel over a large gradient region. This leads to an unstable
 *    vector field.  In the levelset approach, the motion vectors will
 *    be proportional to the gradients, scaled by the maximum gradient
 *    over the entire field. The pixel with at the lower gradient
 *    position will more less than the pixel at the higher gradient
 *    position.
 *    (3) Gradients are calculated using minmod finite difference
 *    instead of using central differences.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type
 * and the deformation field type.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial deformation field maybe set via
 * SetInitialDisplacementField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The algorithm has one parameters: the number of iteration to be performed.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDisplacementField.
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed in LevelSetMotionFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 *
 * Ref: B.C. Vemuri, J. Ye, Y. Chen, C.M. Leonard. "Image
 * registration via level-set motion: applications to atlas-based
 * segmentation". Medical Image Analysis. Vol. 7. pp. 1-20. 2003.
 *
 * \sa LevelSetMotionRegistrationFunction
 * \sa DemonsRegistrationFilter
 * \ingroup DeformableImageRegistration MultiThreaded
 * \ingroup ITKPDEDeformableRegistration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
class ITK_TEMPLATE_EXPORT LevelSetMotionRegistrationFilter:
  public PDEDeformableRegistrationFilter< TFixedImage, TMovingImage,
                                          TDisplacementField >
{
public:
  /** Standard class typedefs. */
  typedef LevelSetMotionRegistrationFilter                                                Self;
  typedef PDEDeformableRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField > Superclass;
  typedef SmartPointer< Self >                                                            Pointer;
  typedef SmartPointer< const Self >                                                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetMotionRegistrationFilter,
               PDEDeformableRegistrationFilter);

  /** Inherit types from superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** FixedImage image type. */
  typedef typename Superclass::FixedImageType    FixedImageType;
  typedef typename Superclass::FixedImagePointer FixedImagePointer;

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;

  /** Deformation field type. */
  typedef typename Superclass::DisplacementFieldType
  DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer
  DisplacementFieldPointer;

  /** FiniteDifferenceFunction type. */
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** LevelSetMotionFilterFunction type. */
  typedef LevelSetMotionRegistrationFunction< FixedImageType, MovingImageType,
                                              DisplacementFieldType >  LevelSetMotionFunctionType;

  /** Get the metric value. The metric value is the mean square difference
   * in intensity between the fixed image and transforming moving image
   * computed over the the overlapping region between the two images.
   * This is value is only available for the previous iteration and
   * NOT the current iteration. */
  virtual double GetMetric() const;

  /** Set/Get the parameter alpha.  Alpha is added to the calculated
   * gradient magnitude prior to normalizing the gradient to protect
   * against numerical instability as the gradient magnitude
   * approaches zero.  This should be set as a small fraction of the
   * intensity dynamic range, for instance 0.04%. Default is the
   * absolute (not percentage) value of 0.1. */
  virtual void SetAlpha(double);

  virtual double GetAlpha() const;

  /** Set/Get the threshold below which the absolute difference of
   * intensity yields a match. When the intensities match between a
   * moving and fixed image pixel, the update vector (for that
   * iteration) will be the zero vector. Default is 0.001. */
  virtual void SetIntensityDifferenceThreshold(double);

  virtual double GetIntensityDifferenceThreshold() const;

  /** Set/Get the threshold below which the gradient magnitude is
   * considered the zero vector. Default is 1e-9. */
  virtual void SetGradientMagnitudeThreshold(double);

  virtual double GetGradientMagnitudeThreshold() const;

  /** Set/Get the standard deviation used for smoothing the moving
   * image prior to calculating gradients. The standard deviation is
   * measured in physical units (for instance mm).  Note that this
   * smoothing value is not to be confused with the
   * PDEDeformableRegistrationFilter::SetStandardDeviations()
   * method. The method in PDEDeformableRegistrationFilter is for
   * setting the smoothing parameters for regularizing the deformation
   * field between interations.  Those smoothing parameters are set in
   * pixel units not physical units. Deformation field smoothing is
   * not done by default in LevelSetMotionRegistration. This smoothing
   * parameter is to condition the gradient calculation and parameter
   * is specified in physical units. */
  virtual void SetGradientSmoothingStandardDeviations(double sigma);

  virtual double GetGradientSmoothingStandardDeviations() const;

protected:
  LevelSetMotionRegistrationFilter();
  ~LevelSetMotionRegistrationFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the state of filter and equation before each iteration. */
  virtual void InitializeIteration() ITK_OVERRIDE;

  /** Apply update. */
  virtual void ApplyUpdate(const TimeStepType& dt) ITK_OVERRIDE;

  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving.  This version
   * calls the superclass' version but also Halts if the RMSChange is zero.
   */
  virtual bool Halt() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetMotionRegistrationFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetMotionRegistrationFilter.hxx"
#endif

#endif
