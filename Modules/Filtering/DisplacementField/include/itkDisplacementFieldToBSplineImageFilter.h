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
#ifndef itkDisplacementFieldToBSplineImageFilter_h
#define itkDisplacementFieldToBSplineImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkPointSet.h"
#include "itkVector.h"

namespace itk
{

/**
 * \class DisplacementFieldToBSplineImageFilter
 * \brief Class which takes a dense displacement field image and/or a set of points
 * with associated displacements and smooths them using B-splines.  The inverse
 * can also be estimated.
 *
 * \author Nick Tustison
 *
 * \ingroup ITKDisplacementField
 */

template <typename TInputImage,
          typename TInputPointSet = PointSet<typename TInputImage::PixelType, TInputImage::ImageDimension>,
          typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT DisplacementFieldToBSplineImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DisplacementFieldToBSplineImageFilter);

  using Self = DisplacementFieldToBSplineImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputFieldType = TInputImage;
  using InputPointSetType = TInputPointSet;
  using OutputFieldType = TOutputImage;

  using DisplacementFieldType = InputFieldType;
  using InverseDisplacementFieldType = OutputFieldType;
  using InputFieldPointType = typename InputFieldType::PointType;

  /** Image type alias support */
  using PixelType = typename OutputFieldType::PixelType;
  using VectorType = typename OutputFieldType::PixelType;
  using RegionType = typename OutputFieldType::RegionType;
  using IndexType = typename OutputFieldType::IndexType;

  using SpacingType = typename OutputFieldType::SpacingType;
  using OriginType = typename OutputFieldType::PointType;
  using SizeType = typename OutputFieldType::SizeType;
  using DirectionType = typename OutputFieldType::DirectionType;

  using RealType = typename VectorType::RealValueType;
  using RealImageType = Image<RealType, ImageDimension>;

  /** Point set type alias support */
  using PointType = typename InputPointSetType::PointType;
  using PointDataType = typename InputPointSetType::PixelType;
  using PointsContainerType = typename InputPointSetType::PointsContainer;
  using PointDataContainerType = typename InputPointSetType::PointDataContainer;

  /** B-sline filter type alias */
  using BSplineFilterType = BSplineScatteredDataPointSetToImageFilter<InputPointSetType, OutputFieldType>;
  using WeightsContainerType = typename BSplineFilterType::WeightsContainerType;
  using DisplacementFieldControlPointLatticeType = typename BSplineFilterType::PointDataImageType;
  using ArrayType = typename BSplineFilterType::ArrayType;

  /** Set the displacement field */
  void
  SetDisplacementField(const InputFieldType * field)
  {
    this->SetInput(0, field);
  }

  /** Get the input displacement field. */
  const InputFieldType *
  GetDisplacementField() const
  {
    return this->GetInput(0);
  }

  /**
   * Set confidence image function.  If a confidence image is specified,
   * estimation of the displacement field weights the contribution of each voxel
   * according the value of the corresponding voxel in the confidence image.
   */
  void
  SetConfidenceImage(const RealImageType * image)
  {
    this->SetNthInput(1, const_cast<RealImageType *>(image));
  }
  void
  SetInput1(const RealImageType * image)
  {
    this->SetConfidenceImage(image);
  }

  /** Get confidence image function. */
  const RealImageType *
  GetConfidenceImage() const
  {
    return static_cast<const RealImageType *>(this->ProcessObject::GetInput(1));
  }

  /** Set the input point set */
  void
  SetPointSet(const InputPointSetType * points)
  {
    this->SetNthInput(2, const_cast<InputPointSetType *>(points));
  }
  void
  SetInput2(const InputPointSetType * points)
  {
    this->SetPointSet(points);
  }

  /** Get the input point set. */
  const InputPointSetType *
  GetPointSet() const
  {
    return static_cast<const InputPointSetType *>(this->ProcessObject::GetInput(2));
  }

  /** Set the confidence weights associated with the input point set*/
  void
  SetPointSetConfidenceWeights(WeightsContainerType * weights);

  /** Get the displacement field control point lattice. */
  const DisplacementFieldControlPointLatticeType *
  GetDisplacementFieldControlPointLattice() const
  {
    return static_cast<const DisplacementFieldControlPointLatticeType *>(this->GetOutput(1));
  }

  /** Define the b-spline domain from an image */
  void
  SetBSplineDomainFromImage(RealImageType *);

  /** Define the b-spline domain from an image */
  void
  SetBSplineDomainFromImage(const RealImageType * image)
  {
    this->SetBSplineDomainFromImage(const_cast<RealImageType *>(image));
  }

  /** Define the b-spline domain from a displacement field */
  void
  SetBSplineDomainFromImage(InputFieldType *);

  /** Define the b-spline domain from a displacement field */
  void
  SetBSplineDomainFromImage(const InputFieldType * field)
  {
    this->SetBSplineDomainFromImage(const_cast<InputFieldType *>(field));
  }

  /** Define the b-spline domain explicitly. */
  void SetBSplineDomain(OriginType, SpacingType, SizeType, DirectionType);

  /* Set/Get b-spline domain origin. */
  itkGetConstMacro(BSplineDomainOrigin, OriginType);

  /* Set/Get b-spline domain spacing. */
  itkGetConstMacro(BSplineDomainSpacing, SpacingType);

  /* Set/Get b-spline domain size. */
  itkGetConstMacro(BSplineDomainSize, SizeType);

  /* Set/Get b-spline domain direction. */
  itkGetConstMacro(BSplineDomainDirection, DirectionType);

  /* Use input field to define the B-spline doain. */
  itkSetMacro(UseInputFieldToDefineTheBSplineDomain, bool);
  itkGetConstMacro(UseInputFieldToDefineTheBSplineDomain, bool);
  itkBooleanMacro(UseInputFieldToDefineTheBSplineDomain);

  /**
   * Set the spline order defining the bias field estimate.  Default = 3.
   */
  itkSetMacro(SplineOrder, unsigned int);

  /**
   * Get the spline order defining the bias field estimate.  Default = 3.
   */
  itkGetConstMacro(SplineOrder, unsigned int);

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro(NumberOfControlPoints, ArrayType);

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro(NumberOfControlPoints, ArrayType);

  /**
   * Set the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 1 level.
   */
  itkSetMacro(NumberOfFittingLevels, ArrayType);

  /**
   * Set the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 1 level.
   */
  void
  SetNumberOfFittingLevels(unsigned int n)
  {
    ArrayType nlevels;

    nlevels.Fill(n);
    this->SetNumberOfFittingLevels(nlevels);
  }

  /**
   * Get the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 1 level.
   */
  itkGetConstMacro(NumberOfFittingLevels, ArrayType);

  /**
   * Estimate the inverse field instead of the forward field.  Default = false.
   */
  itkBooleanMacro(EstimateInverse);
  itkSetMacro(EstimateInverse, bool);
  itkGetConstMacro(EstimateInverse, bool);

  /**
   * Enforce stationary boundary conditions.  Default = false.
   */
  itkBooleanMacro(EnforceStationaryBoundary);
  itkSetMacro(EnforceStationaryBoundary, bool);
  itkGetConstMacro(EnforceStationaryBoundary, bool);

protected:
  /** Constructor */
  DisplacementFieldToBSplineImageFilter();

  /** Deconstructor */
  ~DisplacementFieldToBSplineImageFilter() override = default;

  /** Standard print self function **/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** preprocessing function */
  void
  GenerateData() override;

private:
  bool         m_EstimateInverse{ false };
  bool         m_EnforceStationaryBoundary{ true };
  unsigned int m_SplineOrder{ 3 };
  ArrayType    m_NumberOfControlPoints;
  ArrayType    m_NumberOfFittingLevels;

  typename WeightsContainerType::Pointer m_PointWeights;
  bool                                   m_UsePointWeights{ false };

  OriginType    m_BSplineDomainOrigin;
  SpacingType   m_BSplineDomainSpacing;
  SizeType      m_BSplineDomainSize;
  DirectionType m_BSplineDomainDirection;

  bool m_BSplineDomainIsDefined{ true };
  bool m_UseInputFieldToDefineTheBSplineDomain{ false };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDisplacementFieldToBSplineImageFilter.hxx"
#endif

#endif
