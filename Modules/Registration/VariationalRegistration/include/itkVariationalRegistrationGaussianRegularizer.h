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
#ifndef itkVariationalRegistrationGaussianRegularizer_h
#define itkVariationalRegistrationGaussianRegularizer_h

#include "itkVariationalRegistrationRegularizer.h"

namespace itk
{

/** \class itk::VariationalRegistrationGaussianRegularizer
 *
 *  \brief This class performs Gaussian smoothing of a vector field.
 *
 *  We compute \f$u^{out}=K_{\sigma}\star u^{in}\f$ with
 *  \f$K_{\sigma}\f$ the Gaussian kernel. This regularizer can be used
 *  to implement Demons registration within the variational framework.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationRegularizer
 *  \sa VariationalRegistrationDemonsFunction
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TDisplacementField>
class VariationalRegistrationGaussianRegularizer : public VariationalRegistrationRegularizer<TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VariationalRegistrationGaussianRegularizer);

  /** Standard class type alias */
  using Self = VariationalRegistrationGaussianRegularizer;
  using Superclass = VariationalRegistrationRegularizer<TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationGaussianRegularizer, VariationalRegistrationRegularizer);

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TDisplacementField::ImageDimension;

  /** Deformation field types, inherited from Superclass. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;
  using DisplacementFieldConstPointer = typename Superclass::DisplacementFieldConstPointer;
  using PixelType = typename Superclass::PixelType;

  using ValueType = typename Superclass::ValueType;

  /** Types for buffer image. */
  using BufferImageType = Image<ValueType, ImageDimension>;
  using BufferImagePointer = typename BufferImageType::Pointer;
  using BufferImageRegionType = typename BufferImageType::RegionType;

  /** Array containing standard deviations in each direction. */
  using StandardDeviationsType = FixedArray<double, ImageDimension>;

  /** Set the Gaussian smoothing standard deviations for the
   * displacement field. The values are set with respect to pixel
   * coordinates. */
  itkSetMacro(StandardDeviations, StandardDeviationsType);
  virtual void
  SetStandardDeviations(double value);

  /** Get the Gaussian smoothing standard deviations use for smoothing
   * the displacement field. */
  itkGetConstReferenceMacro(StandardDeviations, StandardDeviationsType);

  /** Set the desired maximum error of the Gaussian kernel approximate.
   * \sa GaussianOperator. */
  itkSetMacro(MaximumError, double);

  /** Get the desired maximum error of the Gaussian kernel approximate.
   * \sa GaussianOperator. */
  itkGetConstMacro(MaximumError, double);

  /** Set the desired limits of the Gaussian kernel width.
   * \sa GaussianOperator. */
  itkSetMacro(MaximumKernelWidth, unsigned int);

  /** Get the desired limits of the Gaussian kernel width.
   * \sa GaussianOperator. */
  itkGetConstMacro(MaximumKernelWidth, unsigned int);

protected:
  VariationalRegistrationGaussianRegularizer();
  ~VariationalRegistrationGaussianRegularizer() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Execute regularization. This method is multi-threaded but does not
   * use ThreadedGenerateData(). */
  void
  GenerateData() override;

  /** Method for initialization. Buffer images are allocated and the matrices
   * calculated in this method. */
  void
  Initialize() override;

private:
  /** Standard deviation for Gaussian smoothing */
  StandardDeviationsType m_StandardDeviations;

  /** Maximum error for Gaussian operator approximation. */
  double m_MaximumError;

  /** Limits of Gaussian kernel width. */
  unsigned int m_MaximumKernelWidth;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationGaussianRegularizer.hxx"
#endif

#endif
