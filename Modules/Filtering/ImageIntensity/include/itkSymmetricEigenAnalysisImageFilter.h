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
#ifndef itkSymmetricEigenAnalysisImageFilter_h
#define itkSymmetricEigenAnalysisImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkSymmetricEigenAnalysis.h"

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
template< typename TInput, typename TOutput >
class SymmetricEigenAnalysisFunction
{
public:
  typedef typename TInput::RealValueType RealValueType;
  SymmetricEigenAnalysisFunction() {}
  ~SymmetricEigenAnalysisFunction() {}
  typedef SymmetricEigenAnalysis< TInput, TOutput > CalculatorType;
  bool operator!=(const SymmetricEigenAnalysisFunction &) const
  {
    return false;
  }

  bool operator==(const SymmetricEigenAnalysisFunction & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & x) const
  {
    TOutput eigenValues;

    m_Calculator.ComputeEigenValues(x, eigenValues);
    return eigenValues;
  }

  /** Method to explicitly set the dimension of the matrix */
  void SetDimension(unsigned int n)
  {
    m_Calculator.SetDimension(n);
  }
  unsigned int GetDimension() const
  {
    return m_Calculator.GetDimension();
  }

  /** Typdedefs to order eigen values.
   * OrderByValue:      lambda_1 < lambda_2 < ....
   * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
   * DoNotOrder:        Default order of eigen values obtained after QL method
   */
  typedef enum {
    OrderByValue = 1,
    OrderByMagnitude,
    DoNotOrder
    } EigenValueOrderType;

  /** Order eigen values. Default is to OrderByValue:  lambda_1 <
   * lambda_2 < .... */
  void OrderEigenValuesBy(EigenValueOrderType order)
  {
    if ( order == OrderByMagnitude )
      {
      m_Calculator.SetOrderEigenMagnitudes(true);
      }
    else if ( order == DoNotOrder )
      {
      m_Calculator.SetOrderEigenValues(false);
      }
  }

private:
  CalculatorType m_Calculator;
};
}  // end namespace functor

/** \class SymmetricEigenAnalysisImageFilter
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
 * The user of this class is explicitly supposed to set the dimension of the
 * 2D matrix using the SetDimension() method.
 *
 * \ingroup IntensityImageFilters  MultiThreaded  TensorObjects
 *
 * \ingroup ITKImageIntensity
 */
template< typename  TInputImage, typename  TOutputImage = TInputImage >
class SymmetricEigenAnalysisImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::SymmetricEigenAnalysisFunction<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef SymmetricEigenAnalysisImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::SymmetricEigenAnalysisFunction<
      typename TInputImage::PixelType,
      typename TOutputImage::PixelType > >   Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename TOutputImage::PixelType     OutputPixelType;
  typedef typename TInputImage::PixelType      InputPixelType;
  typedef typename InputPixelType::ValueType   InputValueType;
  typedef typename Superclass::FunctorType     FunctorType;

  /** Typdedefs to order eigen values.
   * OrderByValue:      lambda_1 < lambda_2 < ....
   * OrderByMagnitude:  |lambda_1| < |lambda_2| < .....
   * DoNotOrder:        Default order of eigen values obtained after QL method
   */
  typedef typename FunctorType::EigenValueOrderType EigenValueOrderType;

  /** Order eigen values. Default is to OrderByValue:  lambda_1 <
   * lambda_2 < .... */
  void OrderEigenValuesBy(EigenValueOrderType order)
  {
    this->GetFunctor().OrderEigenValuesBy(order);
  }

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SymmetricEigenAnalysisImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Print internal ivars */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { this->Superclass::PrintSelf(os, indent); }

  /** Set the dimension of the tensor. (For example the SymmetricSecondRankTensor
   * is a pxp matrix) */
  void SetDimension(unsigned int p)
  {
    this->GetFunctor().SetDimension(p);
  }
  unsigned int GetDimension() const
  {
    return this->GetFunctor().GetDimension();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputValueType > ) );
  // End concept checking
#endif

protected:
  SymmetricEigenAnalysisImageFilter() {}
  virtual ~SymmetricEigenAnalysisImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SymmetricEigenAnalysisImageFilter);
};
} // end namespace itk

#endif
