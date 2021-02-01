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
#ifndef itkSymmetricEigenAnalysisImageFilter_h
#define itkSymmetricEigenAnalysisImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkSymmetricEigenAnalysis.h"
#include "ITKImageIntensityExport.h"

namespace itk
{
// This functor class invokes the computation of Eigen Analysis for
// every pixel. The input pixel type must provide the API for the [][]
// operator, while the output pixel type must provide the API for the
// [] operator. Input pixel matrices should be symmetric.
//
// The default operation is to order eigen values in ascending order.
// You may also use OrderEigenValuesBy( ) to order eigen values by
// magnitude as is common with use of tensors in vessel extraction.
namespace Functor
{

#if !defined(ITK_LEGACY_REMOVE)
using OrderTypeOfEigenValue = EigenValueOrderEnum;
#endif

template <typename TInput, typename TOutput>
class SymmetricEigenAnalysisFunction
{
public:
  using RealValueType = typename TInput::RealValueType;
  SymmetricEigenAnalysisFunction() = default;
  ~SymmetricEigenAnalysisFunction() = default;
  using CalculatorType = SymmetricEigenAnalysis<TInput, TOutput>;
  bool
  operator!=(const SymmetricEigenAnalysisFunction &) const
  {
    return false;
  }

  bool
  operator==(const SymmetricEigenAnalysisFunction & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & x) const
  {
    TOutput eigenValues;

    m_Calculator.ComputeEigenValues(x, eigenValues);
    return eigenValues;
  }

  /** Method to explicitly set the dimension of the matrix */
  void
  SetDimension(unsigned int n)
  {
    m_Calculator.SetDimension(n);
  }
  unsigned int
  GetDimension() const
  {
    return m_Calculator.GetDimension();
  }

#if !defined(ITK_LEGACY_REMOVE)
  /** Enables reverse compatibility for enumeration values */
  using EigenValueOrderType = EigenValueOrderEnum;
#endif
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr EigenValueOrderEnum OrderByValue = EigenValueOrderEnum::OrderByValue;
  static constexpr EigenValueOrderEnum OrderByMagnitude = EigenValueOrderEnum::OrderByMagnitude;
  static constexpr EigenValueOrderEnum DoNotOrder = EigenValueOrderEnum::DoNotOrder;
#endif

  /** Order eigen values. Default is to OrderByValue:  lambda_1 <
   * lambda_2 < .... */
  void
  OrderEigenValuesBy(EigenValueOrderEnum order)
  {
    if (order == EigenValueOrderEnum::OrderByMagnitude)
    {
      m_Calculator.SetOrderEigenMagnitudes(true);
    }
    else if (order == EigenValueOrderEnum::DoNotOrder)
    {
      m_Calculator.SetOrderEigenValues(false);
    }
  }
  void
  SetOrderEigenValuesBy(EigenValueOrderEnum order)
  {
    this->OrderEigenValuesBy(order);
  }
  EigenValueOrderEnum
  GetOrderEigenValuesBy() const
  {
    if (m_Calculator.GetOrderEigenMagnitudes())
    {
      return EigenValueOrderEnum::OrderByMagnitude;
    }
    if (m_Calculator.GetOrderEigenValues())
    {
      return EigenValueOrderEnum::OrderByValue;
    }
    return EigenValueOrderEnum::DoNotOrder;
  }


private:
  CalculatorType m_Calculator;
};

template <unsigned int TMatrixDimension, typename TInput, typename TOutput>
class SymmetricEigenAnalysisFixedDimensionFunction
{
public:
  using RealValueType = typename TInput::RealValueType;
  SymmetricEigenAnalysisFixedDimensionFunction() = default;
  ~SymmetricEigenAnalysisFixedDimensionFunction() = default;
  using CalculatorType = SymmetricEigenAnalysisFixedDimension<TMatrixDimension, TInput, TOutput>;
  bool
  operator!=(const SymmetricEigenAnalysisFixedDimensionFunction &) const
  {
    return false;
  }

  bool
  operator==(const SymmetricEigenAnalysisFixedDimensionFunction & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput & x) const
  {
    TOutput eigenValues;

    m_Calculator.ComputeEigenValues(x, eigenValues);
    return eigenValues;
  }

  /** Method to get the dimension of the matrix. Dimension is fixed, no SetDimension is provided */
  unsigned int
  GetDimension() const
  {
    return m_Calculator.GetDimension();
  }

#if !defined(ITK_LEGACY_REMOVE)
  /** Enables reverse compatibility for enumeration values */
  using EigenValueOrderType = EigenValueOrderEnum;
#endif
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr EigenValueOrderEnum OrderByValue = EigenValueOrderEnum::OrderByValue;
  static constexpr EigenValueOrderEnum OrderByMagnitude = EigenValueOrderEnum::OrderByMagnitude;
  static constexpr EigenValueOrderEnum DoNotOrder = EigenValueOrderEnum::DoNotOrder;
#endif

  /** Order eigen values. Default is to OrderByValue:  lambda_1 <
   * lambda_2 < .... */
  void
  OrderEigenValuesBy(EigenValueOrderEnum order)
  {
    if (order == EigenValueOrderEnum::OrderByMagnitude)
    {
      m_Calculator.SetOrderEigenMagnitudes(true);
    }
    else if (order == EigenValueOrderEnum::DoNotOrder)
    {
      m_Calculator.SetOrderEigenValues(false);
    }
  }

private:
  CalculatorType m_Calculator;
};

/** Define how to print enumerations */
extern ITKImageIntensity_EXPORT std::ostream &
                                operator<<(std::ostream & out, const EigenValueOrderEnum value);

} // end namespace Functor

/**
 *\class SymmetricEigenAnalysisImageFilter
 * \brief Computes the eigen-values of every input symmetric matrix pixel.
 *
 * SymmetricEigenAnalysisImageFilter applies pixel-wise the invocation for
 * computing the eigen-values and eigen-vectors of the symmetric matrix
 * corresponding to every input pixel.
 *
 * The OrderEigenValuesBy( .. ) method can be used to order eigen values
 * in ascending order by value or magnitude or no ordering.
 * OrderByValue:      lambda_1 < lambda_2 < ....
 * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
 * DoNotOrder:        Default order of eigen values obtained after QL method
 *
 * \ingroup IntensityImageFilters  MultiThreaded  TensorObjects
 *
 * \ingroup ITKImageIntensity
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class SymmetricEigenAnalysisImageFilter
  : public UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::SymmetricEigenAnalysisFunction<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SymmetricEigenAnalysisImageFilter);

  /** Standard class type aliases. */
  using Self = SymmetricEigenAnalysisImageFilter;
  using Superclass = UnaryFunctorImageFilter<
    TInputImage,
    TOutputImage,
    Functor::SymmetricEigenAnalysisFunction<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputImageType = typename Superclass::OutputImageType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputValueType = typename InputPixelType::ValueType;
  using FunctorType = typename Superclass::FunctorType;

#if !defined(ITK_LEGACY_REMOVE)
  /** Enables reverse compatibility for enumeration values */
  using EigenValueOrderType = EigenValueOrderEnum;
#endif

  /** Order eigen values. Default is to OrderByValue:  lambda_1 <
   * lambda_2 < .... */
  void
  OrderEigenValuesBy(EigenValueOrderEnum order)
  {
    this->GetFunctor().OrderEigenValuesBy(order);
  }
  void
  SetOrderEigenValuesBy(EigenValueOrderEnum order)
  {
    this->OrderEigenValuesBy(order);
  }
  EigenValueOrderEnum
  GetOrderEigenValuesBy() const
  {
    return this->GetFunctor().GetOrderEigenValuesBy();
  }

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SymmetricEigenAnalysisImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    this->Superclass::PrintSelf(os, indent);
  }

  /** Set the dimension of the tensor. (For example the SymmetricSecondRankTensor
   * is a pxp matrix) */
  void
  SetDimension(unsigned int p)
  {
    this->GetFunctor().SetDimension(p);
  }
  unsigned int
  GetDimension() const
  {
    return this->GetFunctor().GetDimension();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputValueType>));
  // End concept checking
#endif

protected:
  SymmetricEigenAnalysisImageFilter() { this->SetDimension(TInputImage::ImageDimension); }
  ~SymmetricEigenAnalysisImageFilter() override = default;
};

/**
 *\class SymmetricEigenAnalysisFixedDimensionImageFilter
 * \brief Computes the eigen-values of every input symmetric matrix pixel.
 *
 * SymmetricEigenAnalysisImageFilter applies pixel-wise the invokation for
 * computing the eigen-values and eigen-vectors of the symmetric matrix
 * corresponding to every input pixel.
 *
 * The OrderEigenValuesBy( .. ) method can be used to order eigen values
 * in ascending order by value or magnitude or no ordering.
 * OrderByValue:      lambda_1 < lambda_2 < ....
 * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
 * DoNotOrder:        Default order of eigen values obtained after QL method
 *
 * \ingroup IntensityImageFilters  MultiThreaded  TensorObjects
 *
 * \ingroup ITKImageIntensity
 */
template <unsigned int TMatrixDimension, typename TInputImage, typename TOutputImage = TInputImage>
class SymmetricEigenAnalysisFixedDimensionImageFilter
  : public UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::SymmetricEigenAnalysisFixedDimensionFunction<TMatrixDimension,
                                                            typename TInputImage::PixelType,
                                                            typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SymmetricEigenAnalysisFixedDimensionImageFilter);

  /** Standard class type aliases. */
  using Self = SymmetricEigenAnalysisFixedDimensionImageFilter;
  using Superclass =
    UnaryFunctorImageFilter<TInputImage,
                            TOutputImage,
                            Functor::SymmetricEigenAnalysisFixedDimensionFunction<TMatrixDimension,
                                                                                  typename TInputImage::PixelType,
                                                                                  typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputImageType = typename Superclass::OutputImageType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputValueType = typename InputPixelType::ValueType;
  using FunctorType = typename Superclass::FunctorType;

  /** expose public enumeration class as member  for backward compatibility */
  using EigenValueOrderEnum = itk::EigenValueOrderEnum;

  /** Order eigen values. Default is to OrderByValue:  lambda_1 <
   * lambda_2 < .... */
  void
  OrderEigenValuesBy(EigenValueOrderEnum order)
  {
    this->GetFunctor().OrderEigenValuesBy(order);
  }

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SymmetricEigenAnalysisFixedDimensionImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    this->Superclass::PrintSelf(os, indent);
  }

  /** GetDimension of the matrix. Dimension is fixed by template parameter, no SetDimension. */
  unsigned int
  GetDimension() const
  {
    return this->GetFunctor().GetDimension();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputValueType>));
  // End concept checking
#endif

protected:
  SymmetricEigenAnalysisFixedDimensionImageFilter() = default;
  ~SymmetricEigenAnalysisFixedDimensionImageFilter() override = default;
};
} // end namespace itk

#endif
