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
#ifndef itkLandmarkBasedTransformInitializer_h
#define itkLandmarkBasedTransformInitializer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkVersorRigid3DTransform.h"
#include "itkRigid2DTransform.h"
#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkPoint.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include <vector>

namespace itk
{
/** \class LandmarkBasedTransformInitializer
 * This class computes the transform that aligns the fixed and moving images
 * given a set of pair landmarks. The class is templated over the Transform type
 * as well as fixed image and moving image types.
 * The transform computed gives the best fit transform that maps the fixed
 * and moving images in a least squares sense. The indices are taken to
 * correspond, so point 1 in the first set will get mapped close to point
 * 1 in the second set, etc.
 *
 * Currently, the  following transforms are supported by the class:
 *    VersorRigid3DTransform
 *    Rigid2DTransform
 *    AffineTransform
 *    BSplineTransform
 *
 * An equal number of fixed and moving landmarks need to be specified using
 * SetFixedLandmarks() and SetMovingLandmarks(). Any number of landmarks may
 * be specified. In the case of the Affine transformation the number
 * of landmarks must be greater than the landmark dimensionality. If
 * this is not the case an exception is thrown. In the case of the
 * VersorRigid3DTransform and Rigid2DTransform the number of landmarks
 * must be equal or greater than the landmark dimensionality. If this
 * is not the case, only the translational component of the
 * transformation is computed and the rotation is the identity.
 * In the case of using Affine or BSpline transforms, each landmark pair can
 * contribute in the final transform based on its defined weight. Number of
 * weights should be equal to the number of landmarks and can be specified using
 * SetLandmarkWeight(). By defaults are weights are set to one.
 * Call InitializeTransform() to initialize the transform.
 *
 * The class is based in part on Hybrid/vtkLandmarkTransform originally
 * implemented in python by David G. Gobbi.
 *
 * The solution is based on
 * Berthold K. P. Horn (1987), "Closed-form solution of absolute orientation
 * using unit quaternions,"
 * http://people.csail.mit.edu/bkph/papers/Absolute_Orientation.pdf
 *
 * The Affine Transform initializer is based on an algorithm by H Spaeth,
 * and is described in the Insight Journal Article
 * "Affine Transformation for Landmark Based Registration Initializer
 * in ITK" by Kim E.Y., Johnson H., Williams N.
 * available at  http://midasjournal.com/browse/publication/825
 *
 * \ingroup ITKRegistrationCommon
 *
 * \sphinx
 * \sphinxexample{Registration/Common/RegisterImageToAnotherUsingLandmarks,Register Image To Another Using Landmarks}
 * \endsphinx
 */
template <typename TTransform,
          typename TFixedImage = itk::ImageBase<TTransform::InputSpaceDimension>,
          typename TMovingImage = itk::ImageBase<TTransform::OutputSpaceDimension>>
class ITK_TEMPLATE_EXPORT LandmarkBasedTransformInitializer : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LandmarkBasedTransformInitializer);

  /** Standard class type aliases. */
  using Self = LandmarkBasedTransformInitializer;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LandmarkBasedTransformInitializer, Object);

  /** Type of the transform to initialize */
  using TransformType = TTransform;
  using TransformPointer = typename TransformType::Pointer;

  /** Dimension of parameters. */
  static constexpr unsigned int InputSpaceDimension = TransformType::InputSpaceDimension;
  static constexpr unsigned int OutputSpaceDimension = TransformType::OutputSpaceDimension;

  /** Set the transform to be initialized */
  itkSetObjectMacro(Transform, TransformType);

  /** Image Types to use in the initialization of the transform */
  using FixedImageType = TFixedImage;
  using MovingImageType = TMovingImage;

  /** Set the reference image to define the parametric domain for the BSpline transform */
  itkSetConstObjectMacro(ReferenceImage, FixedImageType);

  /** Set the number of control points to define the parametric domain for the BSpline transform */
  itkSetMacro(BSplineNumberOfControlPoints, unsigned int);

  using FixedImagePointer = typename FixedImageType::ConstPointer;
  using MovingImagePointer = typename MovingImageType::ConstPointer;

  /** Determine the image dimension. */
  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension;

  /** Convenience type alias */
  using InputPointType = typename TransformType::InputPointType;
  using OutputVectorType = typename TransformType::OutputVectorType;

  using LandmarkPointType = Point<double, ImageDimension>;
  using LandmarkPointContainer = std::vector<LandmarkPointType>;
  using PointsContainerConstIterator = typename LandmarkPointContainer::const_iterator;

  using ParametersType = typename TransformType::ParametersType;
  using ParametersValueType = typename ParametersType::ValueType;
  using LandmarkWeightType = std::vector<double>;
  using LandmarkWeightConstIterator = LandmarkWeightType::const_iterator;

  /** Set the Fixed landmark point containers */
  void
  SetFixedLandmarks(const LandmarkPointContainer & fixedLandmarks)
  {
    this->m_FixedLandmarks = fixedLandmarks;
  }

  /** Set the Moving landmark point containers */
  void
  SetMovingLandmarks(const LandmarkPointContainer & movingLandmarks)
  {
    this->m_MovingLandmarks = movingLandmarks;
  }

  /** Set the landmark weight point containers
   *  Weight includes diagonal elements of weight matrix
   */
  void
  SetLandmarkWeight(LandmarkWeightType & landmarkWeight)
  {
    this->m_LandmarkWeight = landmarkWeight;
  }

  /**  Supported Transform type alias */
  using VersorRigid3DTransformType = VersorRigid3DTransform<ParametersValueType>;
  using Rigid2DTransformType = Rigid2DTransform<ParametersValueType>;
  using AffineTransformType = AffineTransform<ParametersValueType, FixedImageType::ImageDimension>;

  constexpr static unsigned int SplineOrder = 3;
  using BSplineTransformType = BSplineTransform<ParametersValueType, FixedImageType::ImageDimension, SplineOrder>;

  /** Initialize the transform from the landmarks */
  virtual void
  InitializeTransform();

protected:
  LandmarkBasedTransformInitializer();
  ~LandmarkBasedTransformInitializer() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** fallback Initializer just sets transform to identity */
  template <typename TTransform2>
  void
  InternalInitializeTransform(TTransform2 *);
  /** Initializer for VersorRigid3D */
  void
  InternalInitializeTransform(VersorRigid3DTransformType *);
  /** Initializer for Rigid2DTransform */
  void
  InternalInitializeTransform(Rigid2DTransformType *);
  /** Initializer for AffineTransform */
  void
  InternalInitializeTransform(AffineTransformType *);
  /** Initializer for BSplineTransform */
  void
  InternalInitializeTransform(BSplineTransformType *);

  FixedImagePointer      m_ReferenceImage;
  TransformPointer       m_Transform;
  LandmarkPointContainer m_FixedLandmarks;
  LandmarkPointContainer m_MovingLandmarks;
  /** weights for affine landmarks */
  LandmarkWeightType m_LandmarkWeight;
  unsigned int       m_BSplineNumberOfControlPoints{ 4 };

}; // class LandmarkBasedTransformInitializer
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLandmarkBasedTransformInitializer.hxx"
#endif

#endif /* itkLandmarkBasedTransformInitializer_h */
