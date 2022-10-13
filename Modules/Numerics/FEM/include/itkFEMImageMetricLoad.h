/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFEMImageMetricLoad_h
#define itkFEMImageMetricLoad_h

#include "itkFEMLoadElementBase.h"

#include "itkImage.h"
#include "itkTranslationTransform.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMath.h"

#include <itkMutualInformationImageToImageMetric.h>
#include <itkMattesMutualInformationImageToImageMetric.h>
#include <itkMeanSquaresImageToImageMetric.h>
#include <itkNormalizedCorrelationImageToImageMetric.h>

namespace itk
{
namespace fem
{
/**
 * \class ImageMetricLoad
 * \brief General image pair load that uses the itkImageToImageMetrics.
 *
 * LoadImageMetric computes FEM gravity loads by using derivatives provided
 * by itkImageToImageMetrics (e.g. mean squares intensity difference.)
 * The function responsible for this is called Fg, as required by the FEMLoad
 * standards.  It takes a vnl_vector as input.
 * We assume the vector input is of size 2*ImageDimension.
 * The 0 to ImageDimension-1 elements contain the position, p,
 * in the reference (moving) image.  The next ImageDimension to 2*ImageDimension-1
 * elements contain the value of the vector field at that point, v(p).
 *
 * Then, we evaluate the derivative at the point p+v(p) with respect to
 * some region of the target (fixed) image by calling the metric with
 * the translation parameters as provided by the vector field at p.
 * The metrics return both a scalar similarity value and vector-valued derivative.
 * The derivative is what gives us the force to drive the FEM registration.
 * These values are computed with respect to some region in the Fixed image.
 * This region size may be set by the user by calling SetMetricRadius.
 * As the metric derivative computation evolves, performance should improve
 * and more functionality will be available (such as scale selection).
 * \ingroup ITKFEM
 */
template <typename TMoving, typename TFixed>
class ITK_TEMPLATE_EXPORT ImageMetricLoad : public LoadElement
{
public:
  /** Standard class type aliases. */
  using Self = ImageMetricLoad;
  using Superclass = LoadElement;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageMetricLoad, LoadElement);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  itk::LightObject::Pointer
  CreateAnother() const override;

  // Necessary type alias for dealing with images BEGIN
  using Float = typename LoadElement::Float;

  using MovingType = TMoving;
  using MovingConstPointer = typename MovingType::ConstPointer;
  using MovingPointer = MovingType *;
  using FixedType = TFixed;
  using FixedPointer = FixedType *;
  using FixedConstPointer = typename FixedType::ConstPointer;

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = MovingType::ImageDimension;

  using RefRegionIteratorType = ImageRegionIteratorWithIndex<MovingType>;
  using TarRegionIteratorType = ImageRegionIteratorWithIndex<FixedType>;

  using MovingNeighborhoodIteratorType = NeighborhoodIterator<MovingType>;
  using MovingNeighborhoodIndexType = typename MovingNeighborhoodIteratorType::IndexType;
  using MovingRadiusType = typename MovingNeighborhoodIteratorType::RadiusType;
  using FixedNeighborhoodIteratorType = NeighborhoodIterator<FixedType>;
  using FixedNeighborhoodIndexType = typename FixedNeighborhoodIteratorType::IndexType;
  using FixedRadiusType = typename FixedNeighborhoodIteratorType::RadiusType;

  // IMAGE DATA
  using RefPixelType = typename MovingType::PixelType;
  using TarPixelType = typename FixedType::PixelType;
  using PixelType = Float;
  using ComputationType = Float;
  using RefImageType = Image<RefPixelType, Self::ImageDimension>;
  using TarImageType = Image<TarPixelType, Self::ImageDimension>;
  using ImageType = Image<PixelType, Self::ImageDimension>;
  using VectorType = vnl_vector<Float>;

  // Necessary type alias for dealing with images END

  // ------------------------------------------------------------
  // Set up the metrics
  // ------------------------------------------------------------
  using CoordinateRepresentationType = double;
  using TransformBaseType = Transform<CoordinateRepresentationType, Self::ImageDimension, Self::ImageDimension>;
  using DefaultTransformType = TranslationTransform<CoordinateRepresentationType, Self::ImageDimension>;

  /**  Type of supported metrics. */
  using MetricBaseType = ImageToImageMetric<FixedType, MovingType>;
  using MetricBaseTypePointer = typename MetricBaseType::Pointer;

  using MutualInformationMetricType = MutualInformationImageToImageMetric<MovingType, FixedType>;

  using MeanSquaresMetricType = MeanSquaresImageToImageMetric<MovingType, FixedType>;

  using NormalizedCorrelationMetricType = NormalizedCorrelationImageToImageMetric<MovingType, FixedType>;

  using DefaultMetricType = MeanSquaresMetricType;
  using ParametersType = typename DefaultTransformType::ParametersType;
  using JacobianType = typename DefaultTransformType::JacobianType;

  using ElementIdentifier = unsigned long;
  using ElementContainerType = VectorContainer<ElementIdentifier, Element::Pointer>;
  // ------------------------------------------------------------
  // Set up an Interpolator
  // ------------------------------------------------------------
  using InterpolatorType = LinearInterpolateImageFunction<MovingType, double>;

  /** Gradient filtering */
  using RealType = float;
  using GradientPixelType = CovariantVector<RealType, Self::ImageDimension>;
  using GradientImageType = Image<GradientPixelType, Self::ImageDimension>;
  using GradientImagePointer = SmartPointer<GradientImageType>;
  using GradientImageFilterType = GradientRecursiveGaussianImageFilter<ImageType, GradientImageType>;
  // using GradientImageFilterPointer = typename GradientImageFilterType::Pointer;

  // FUNCTIONS

  /** Set/Get the Metric.  */
  void
  SetMetric(MetricBaseTypePointer MP)
  {
    m_Metric = MP;
  }

  /** Define the reference (moving) image. */
  void
  SetMovingImage(MovingType * R)
  {
    m_RefImage = R;
    m_RefSize = m_RefImage->GetLargestPossibleRegion().GetSize();
  }

  void
  SetMetricMovingImage(MovingType * R)
  {
    m_Metric->SetMovingImage(R);
    m_RefSize = R->GetLargestPossibleRegion().GetSize();
  }

  /** Define the target (fixed) image. */
  void
  SetFixedImage(FixedType * T)
  {
    m_TarImage = T;
    m_TarSize = T->GetLargestPossibleRegion().GetSize();
  }

  void
  SetMetricFixedImage(FixedType * T)
  {
    m_Metric->SetFixedImage(T);
    m_TarSize = T->GetLargestPossibleRegion().GetSize();
  }

  MovingPointer
  GetMovingImage()
  {
    return m_RefImage;
  }
  FixedPointer
  GetFixedImage()
  {
    return m_TarImage;
  }

  /** Define the metric region size. */
  void
  SetMetricRadius(MovingRadiusType T)
  {
    m_MetricRadius = T;
  }
  /** Get the metric region size. */
  MovingRadiusType
  GetMetricRadius()
  {
    return m_MetricRadius;
  }

  /** Set/Get methods for the number of integration points to use
   * in each 1-dimensional line integral when evaluating the load.
   * This value is passed to the load implementation.
   */
  void
  SetNumberOfIntegrationPoints(unsigned int i)
  {
    m_NumberOfIntegrationPoints = i;
  }
  unsigned int
  GetNumberOfIntegrationPoints()
  {
    return m_NumberOfIntegrationPoints;
  }

  /** Set the direction of the gradient (uphill or downhill).
   * E.g. the mean squares metric should be minimized while NCC and PR should be maximized.
   */
  void
  SetSign(Float s)
  {
    m_Sign = s;
  }

  /** Set the sigma in a gaussian measure. */
  void
  SetTemp(Float s)
  {
    m_Temp = s;
  }

  /** Scaling of the similarity energy term */
  void
  SetGamma(Float s)
  {
    m_Gamma = s;
  }

  /** Set the pointer to the solution vector.
   * \param ptr Pointer to the object of Solution class.
   */
  void
  SetSolution(Solution::ConstPointer ptr) override
  {
    m_Solution = ptr;
  }
  /** Get the pointer to the solution vector.
   * \return Pointer to the object of Solution class.
   */
  Solution::ConstPointer
  GetSolution() override
  {
    return m_Solution;
  }

  /**
   *  This method returns the total metric evaluated over the image with respect to the current solution.
   */
  Float
  GetMetric(VectorType InVec);

  VectorType
  GetPolynomialFitToMetric(VectorType PositionInElement, VectorType SolutionAtPosition);

  VectorType
  MetricFiniteDiff(VectorType PositionInElement, VectorType SolutionAtPosition);

  // FIXME - WE ASSUME THE 2ND VECTOR (INDEX 1) HAS THE INFORMATION WE WANT
  Float
  GetSolution(unsigned int i, unsigned int which = 0)
  {
    return m_Solution->GetSolutionValue(i, which);
  }

  // define the copy constructor
  //  ImageMetricLoad(const ImageMetricLoad& LMS);

  void
  InitializeMetric();

  ImageMetricLoad(); // cannot be private until we always use smart pointers
  Float
  EvaluateMetricGivenSolution(Element::ArrayType * el, Float step = 1.0);

  Float
  EvaluateMetricGivenSolution1(Element::ArrayType * el, Float step = 1.0);

  /**
   * Compute the image based load - implemented with ITK metric derivatives.
   */
  VectorType Fe(VectorType, VectorType);

  static Baseclass *
  NewImageMetricLoad()
  {
    return new ImageMetricLoad;
  }

  /** Set/Get the metric gradient image */
  // void InitializeGradientImage();
  void
  SetMetricGradientImage(GradientImageType * g)
  {
    m_MetricGradientImage = g;
  }
  GradientImageType *
  GetMetricGradientImage()
  {
    return m_MetricGradientImage;
  }

  void
  PrintCurrentEnergy()
  {
    std::cout << " energy " << m_Energy << std::endl;
  }
  double
  GetCurrentEnergy()
  {
    return m_Energy;
  }
  void
  SetCurrentEnergy(double e)
  {
    m_Energy = e;
  }

  // FIXME - Documentation
  void
  ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe) override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  GradientImageType * m_MetricGradientImage;
  MovingPointer       m_RefImage;
  FixedPointer        m_TarImage;
  MovingRadiusType    m_MetricRadius; /** used by the metric to set
                                        region size for fixed image*/
  typename MovingType::SizeType m_RefSize;
  typename FixedType::SizeType  m_TarSize;
  unsigned int                  m_NumberOfIntegrationPoints;
  unsigned int                  m_SolutionIndex;
  unsigned int                  m_SolutionIndex2;
  Float                         m_Sign;
  Float                         m_Temp;
  Float                         m_Gamma;

  typename Solution::ConstPointer     m_Solution;
  MetricBaseTypePointer               m_Metric;
  typename TransformBaseType::Pointer m_Transform;
  typename InterpolatorType::Pointer  m_Interpolator;

  mutable double m_Energy;

private:
};
} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFEMImageMetricLoad.hxx"
#endif

#endif // itkFEMImageMetricLoad_h
