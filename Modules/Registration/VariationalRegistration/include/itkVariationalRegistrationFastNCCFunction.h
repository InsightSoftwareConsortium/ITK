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
#ifndef itkVariationalRegistrationFastNCCFunction_h
#define itkVariationalRegistrationFastNCCFunction_h

#include "itkVariationalRegistrationNCCFunction.h"
#include "itkCovariantVector.h"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{

/** \class VariationalRegistrationFastNCCFunction
 *
 *  \brief This class computes NCC forces in the variational registration framework.
 *
 *  This class implements NCC forces as given in <em> Hermosillo, Chefd'Hotel, and Faugeras.
 *  "Variational methods for multimodal image matching." IJCV 50(3), 2002: 329-343</em>
 *  and <em>Avants et al. "Symmetric diffeomorphic image registration with cross-correlation:
 *  evaluating automated labeling of elderly and neurodegenerative brain." Medical image analysis
 *  12(1), 2008: 26-41</em> (except Jacobian term). We define the derivative of NCC between two
 *  images as:
 *  \f[
 *    f^{NCC}(x)=\tau\kappa\frac{2\sum_w (F-\bar{F})(M-\bar{M})}{\sum_w (F-\bar{F})^2\
 *    \sum_w (M-\bar{M})^2}\left((M-\bar{M}) -
 *    \frac{\sum_w (F-\bar{F})(M-\bar{M})}{\sum_w (F-\bar{F})^2}(F-\bar{F})\right)
 *    \nabla M(x+u(x))
 *  \f]
 *  \f$\tau\f$ is the step size and \f$\kappa\f$ is the mean squared spacing. Use SetRadius()
 *  (see FiniteDifferenceFunction) to set the size of the neighbourhood to compute local mean values
 *  \f$\bar{F}\f$ and \f$\bar{M}\f$ and the local sums.
 *  Alternative, the classical gradient \f$\nabla M(x+u(x))\f$ can be replaced by \f$\nabla F(x)\f$
 *  or \f$\frac{\nabla F(x) + \nabla M(x+u(x))}{2}\f$.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationFunction
 *
 *  \ingroup FiniteDifferenceFunctions
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class VariationalRegistrationFastNCCFunction
  : public VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationFastNCCFunction);

  /** Standard class type alias. */
  using Self = VariationalRegistrationFastNCCFunction;
  using Superclass = VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationFastNCCFunction, VariationalRegistrationNCCFunction);

  /** Get image dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** MovingImage image type. */
  using MovingImageType = typename Superclass::MovingImageType;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** FixedImage image type. */
  using FixedImageType = typename Superclass::FixedImageType;
  using FixedImagePointer = typename Superclass::FixedImagePointer;

  /** MaskImage image type. */
  using MaskImageType = typename Superclass::MaskImageType;
  using MaskImagePointer = typename Superclass::MaskImagePointer;

  using IndexType = typename FixedImageType::IndexType;
  using SizeType = typename FixedImageType::SizeType;
  using SpacingType = typename FixedImageType::SpacingType;

  /** Deformation field type. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer DisplacementFieldTypePointer;

  /** Inherit some types from the superclass. */
  using PixelType = typename Superclass::PixelType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;

  /** Gradient calculator type. */
  using GradientCalculatorType = typename Superclass::GradientCalculatorType;
  using GradientCalculatorPointer = typename GradientCalculatorType::Pointer;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  void *
  GetGlobalDataPointer() const override;

  /** Release memory for global data structure. */
  void
  ReleaseGlobalDataPointer(void * GlobalData) const override;

protected:
  VariationalRegistrationFastNCCFunction();
  ~VariationalRegistrationFastNCCFunction() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using GlobalDataStruct = typename Superclass::GlobalDataStruct;

  /** A global data type for this class of equation. Used to store
   * information for computing the metric. */
  struct NCCGlobalDataStruct
  {
    double              m_SumOfMetricValues;
    SizeValueType       m_NumberOfPixelsProcessed;
    double              m_SumOfSquaredChange;
    IndexType           m_LastIndex;
    bool                bValuesAreValid;
    unsigned int        lastSliceIndex;
    std::vector<double> sfSliceValueList;
    std::vector<double> smSliceValueList;
    std::vector<double> sffSliceValueList;
    std::vector<double> smmSliceValueList;
    std::vector<double> sfmSliceValueList;
    double              sfLastValue;
    double              smLastValue;
    double              sffLastValue;
    double              smmLastValue;
    double              sfmLastValue;
  };
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationFastNCCFunction.hxx"
#endif

#endif
