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
#ifndef itkFEMFiniteDifferenceFunctionLoad_h
#define itkFEMFiniteDifferenceFunctionLoad_h

#include "itkFEMLoadElementBase.h"

#include "itkFEMObject.h"
#include "itkImage.h"
#include "itkTranslationTransform.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMath.h"

#include "itkDemonsRegistrationFunction.h"
#include "itkMeanSquareRegistrationFunction.h"
#include "itkNCCRegistrationFunction.h"
#include "itkMIRegistrationFunction.h"

namespace itk
{
namespace fem
{

/**
 * \class FiniteDifferenceFunctionLoad
 * \brief General image pair load that uses the itkFiniteDifferenceFunctions.
 *
 * This load computes FEM gravity loads by using derivatives provided
 * by itkFiniteDifferenceFunctions (e.g. mean squares intensity difference.)
 * The function responsible for this is called Fg, as required by the FEMLoad
 * standards.  It takes a vnl_vector as input.
 * We assume the vector input is of size 2*ImageDimension.
 * The 0 to ImageDimension-1 elements contain the position, p,
 * in the reference (moving) image.  The next ImageDimension to 2*ImageDimension-1
 * elements contain the value of the vector field at that point, v(p).
 * The metrics return both a scalar similarity value and vector-valued derivative.
 * The derivative is what gives us the force to drive the FEM registration.
 * These values are computed with respect to some region in the Fixed image.
 * This region size may be set by the user by calling SetMetricRadius.
 * As the metric derivative computation evolves, performance should improve
 * and more functionality will be available (such as scale selection).
 * \ingroup ITKFEMRegistration
 */
template <typename TMoving, typename TFixed>
class ITK_TEMPLATE_EXPORT FiniteDifferenceFunctionLoad : public LoadElement
{
public:
  /** Standard class type aliases. */
  using Self = FiniteDifferenceFunctionLoad;
  using Superclass = LoadElement;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FiniteDifferenceFunctionLoad, LoadElement);


  /** CreateAnother method will clone the existing instance of this type,
   *  including its internal member variables. */
  itk::LightObject::Pointer
  CreateAnother() const override;

  // Necessary type alias for dealing with images BEGIN
  using Float = typename LoadElement::Float;

  using MovingImageType = TMoving;
  using MovingConstPointer = typename MovingImageType::ConstPointer;
  using MovingPointer = MovingImageType *;
  using FixedImageType = TFixed;
  using FixedPointer = FixedImageType *;
  using FixedConstPointer = typename FixedImageType::ConstPointer;

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = MovingImageType::ImageDimension;

  using MovingRegionIteratorType = ImageRegionIteratorWithIndex<MovingImageType>;
  using FixedRegionIteratorType = ImageRegionIteratorWithIndex<FixedImageType>;

  using MovingNeighborhoodIteratorType = NeighborhoodIterator<MovingImageType>;
  using MovingNeighborhoodIndexType = typename MovingNeighborhoodIteratorType::IndexType;
  using MovingRadiusType = typename MovingNeighborhoodIteratorType::RadiusType;
  using RadiusType = typename MovingNeighborhoodIteratorType::RadiusType;
  using FixedNeighborhoodIteratorType = NeighborhoodIterator<FixedImageType>;
  using FixedNeighborhoodIndexType = typename FixedNeighborhoodIteratorType::IndexType;
  using FixedRadiusType = typename FixedNeighborhoodIteratorType::RadiusType;

  // Typedefs for Image Data
  using MovingPixelType = typename MovingImageType::PixelType;
  using FixedPixelType = typename FixedImageType::PixelType;
  using PixelType = Float;
  using ComputationType = Float;
  using ImageType = Image<PixelType, Self::ImageDimension>;
  using VectorType = itk::Vector<float, Self::ImageDimension>;
  using FEMVectorType = vnl_vector<Float>;
  using DisplacementFieldType = Image<VectorType, Self::ImageDimension>;
  using DisplacementFieldTypePointer = typename DisplacementFieldType::Pointer;

  using FieldIteratorType = NeighborhoodIterator<DisplacementFieldType>;


  /** PDEDeformableRegistrationFilterFunction type. */
  using FiniteDifferenceFunctionType =
    PDEDeformableRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;
  using FiniteDifferenceFunctionTypePointer = typename FiniteDifferenceFunctionType::Pointer;

  using TimeStepType = typename FiniteDifferenceFunctionType::TimeStepType;

  using MeanSquareRegistrationFunctionType =
    MeanSquareRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  using DemonsRegistrationFunctionType =
    DemonsRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  using NCCRegistrationFunctionType = NCCRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  using MIRegistrationFunctionType = MIRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  using ElementIdentifier = unsigned long;
  using ElementContainerType = VectorContainer<ElementIdentifier, Element::Pointer>;


  /* This method sets the pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  void
  SetDifferenceFunction(FiniteDifferenceFunctionTypePointer drfp)
  {
    drfp->SetFixedImage(m_FixedImage);
    drfp->SetMovingImage(m_MovingImage);
    drfp->SetRadius(m_MetricRadius);
    drfp->SetDisplacementField(m_DisplacementField);
    drfp->InitializeIteration();
    this->m_DifferenceFunction = drfp;
  }

  void
  SetMetric(FiniteDifferenceFunctionTypePointer drfp)
  {
    this->SetDifferenceFunction(static_cast<FiniteDifferenceFunctionType *>(drfp.GetPointer()));

    m_FixedSize = m_DisplacementField->GetLargestPossibleRegion().GetSize();
  }

  /** Define the reference (moving) image. */
  void
  SetMovingImage(MovingImageType * R)
  {
    m_MovingImage = R;
    m_MovingSize = m_MovingImage->GetLargestPossibleRegion().GetSize();
    if (this->m_DifferenceFunction)
    {
      this->m_DifferenceFunction->SetMovingImage(m_MovingImage);
    }
  }

  /** Define the target (fixed) image. */
  void
  SetFixedImage(FixedImageType * T)
  {
    m_FixedImage = T;
    m_FixedSize = T->GetLargestPossibleRegion().GetSize();
    if (this->m_DifferenceFunction)
    {
      this->m_DifferenceFunction->SetFixedImage(m_MovingImage);
    }
  }

  MovingPointer
  GetMovingImage()
  {
    return m_MovingImage;
  }

  FixedPointer
  GetFixedImage()
  {
    return m_FixedImage;
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

  /** Set/Get the direction of the gradient (uphill or downhill).
   * E.g. the mean squares metric should be minimized while NCC and PR should be maximized.260
   */
  void
  SetDescentDirectionMinimize()
  {
    m_Sign = 1.0;
  }

  void
  SetDescentDirectionMaximize()
  {
    m_Sign = -1.0;
  }

  /** Scaling of the similarity energy term. */
  void
  SetGamma(Float s)
  {
    m_Gamma = s;
  }

  void
  SetSolution(Solution::ConstPointer ptr) override
  {
    m_Solution = ptr;
  }

  Solution::ConstPointer
  GetSolution() override
  {
    return m_Solution;
  }

  // FIXME - WE ASSUME THE 2ND VECTOR (INDEX 1) HAS THE INFORMATION WE WANT
  Float
  GetSolution(unsigned int i, unsigned int which = 0)
  {
    return m_Solution->GetSolutionValue(i, which);
  }

  Float
  EvaluateMetricGivenSolution(ElementContainerType * el, Float step = 1.0);


  /**
   * Compute the image based load - implemented with ITK metric derivatives.
   */
  FEMVectorType Fe(FEMVectorType);

  /** Set the displacement field. */
  void
  SetDisplacementField(DisplacementFieldTypePointer df)
  {
    m_DisplacementField = df;
  }

  /** Get the displacement field. */
  DisplacementFieldTypePointer
  GetDisplacementField()
  {
    return m_DisplacementField;
  }

  void
  InitializeIteration();

  void
  InitializeMetric();

  void
  PrintCurrentEnergy();

  double
  GetCurrentEnergy();

  void
  SetCurrentEnergy(double e = 0.0);

  void
  ApplyLoad(Element::ConstPointer element, Element::VectorType & Fe) override;

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  FiniteDifferenceFunctionLoad(); // cannot be private until we always use smart pointers

  MovingPointer m_MovingImage;
  FixedPointer  m_FixedImage;

  /** Used by the metric to set the region size for the fixed image. */
  MovingRadiusType m_MetricRadius;

  typename MovingImageType::SizeType  m_MovingSize;
  typename FixedImageType::SizeType   m_FixedSize;
  unsigned int                        m_NumberOfIntegrationPoints{ 0 };
  unsigned int                        m_SolutionIndex{ 1 };
  unsigned int                        m_SolutionIndex2{ 0 };
  Float                               m_Gamma;
  typename Solution::ConstPointer     m_Solution{ nullptr };
  float                               m_GradSigma{ 0.0f };
  float                               m_Sign{ 1.0f };
  float                               m_WhichMetric{ 0.0f };
  FiniteDifferenceFunctionTypePointer m_DifferenceFunction;

  typename DisplacementFieldType::Pointer m_DisplacementField;
};
} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFEMFiniteDifferenceFunctionLoad.hxx"
#endif

#endif
