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
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class VariationalRegistrationFastNCCFunction
  : public VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationFastNCCFunction                                            Self;
  typedef VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField> Superclass;

  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VariationalRegistrationFastNCCFunction, VariationalRegistrationNCCFunction);

  /** Get image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** MovingImage image type. */
  typedef typename Superclass::MovingImageType    MovingImageType;
  typedef typename Superclass::MovingImagePointer MovingImagePointer;

  /** FixedImage image type. */
  typedef typename Superclass::FixedImageType    FixedImageType;
  typedef typename Superclass::FixedImagePointer FixedImagePointer;

  /** MaskImage image type. */
  typedef typename Superclass::MaskImageType    MaskImageType;
  typedef typename Superclass::MaskImagePointer MaskImagePointer;

  typedef typename FixedImageType::IndexType   IndexType;
  typedef typename FixedImageType::SizeType    SizeType;
  typedef typename FixedImageType::SpacingType SpacingType;

  /** Deformation field type. */
  typedef typename Superclass::DisplacementFieldType        DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldTypePointer DisplacementFieldTypePointer;

  /** Inherit some types from the superclass. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Gradient calculator type. */
  typedef typename Superclass::GradientCalculatorType GradientCalculatorType;
  typedef typename GradientCalculatorType::Pointer    GradientCalculatorPointer;

  /** This method is called by a finite difference solver image filter at
   * each pixel that does not lie on a data set boundary */
  virtual PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) ITK_OVERRIDE;

  /** Return a pointer to a global data structure that is passed to
   * this object from the solver at each calculation.  */
  virtual void *
  GetGlobalDataPointer() const ITK_OVERRIDE;

  /** Release memory for global data structure. */
  virtual void
  ReleaseGlobalDataPointer(void * GlobalData) const ITK_OVERRIDE;

protected:
  VariationalRegistrationFastNCCFunction();
  ~VariationalRegistrationFastNCCFunction() {}

  /** Print information about the filter. */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;

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

private:
  VariationalRegistrationFastNCCFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationFastNCCFunction.hxx"
#endif

#endif
