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
#ifndef itkGaussianDerivativeImageFunction_h
#define itkGaussianDerivativeImageFunction_h

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkGaussianDerivativeSpatialFunction.h"
#include "itkGaussianSpatialFunction.h"

namespace itk
{
/**
 * \class GaussianDerivativeImageFunction
 * \brief Compute the gaussian derivatives of an the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TOutput = double >
class ITK_TEMPLATE_EXPORT GaussianDerivativeImageFunction:
  public ImageFunction< TInputImage,
                        Vector< TOutput, TInputImage::ImageDimension >,
                        TOutput >
{
public:

  /** Standard class typedefs. */
  typedef GaussianDerivativeImageFunction Self;
  typedef ImageFunction< TInputImage,
                         Vector< TOutput, TInputImage::ImageDimension >,
                         TOutput >        Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianDerivativeImageFunction, ImageFunction);

  /** InputImageType typedef support. */
  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;
  typedef typename InputImageType::IndexType IndexType;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension2, unsigned int,
                      InputImageType::ImageDimension);

  typedef ContinuousIndex< SpacePrecisionType, itkGetStaticConstMacro(ImageDimension2) >
  ContinuousIndexType;

  typedef Neighborhood< InputPixelType, itkGetStaticConstMacro(ImageDimension2) > NeighborhoodType;
  typedef Neighborhood< TOutput, itkGetStaticConstMacro(ImageDimension2) >        OperatorNeighborhoodType;

  typedef Vector< TOutput, itkGetStaticConstMacro(ImageDimension2) >                         VectorType;
  typedef typename Superclass::OutputType                                                    OutputType;
  typedef FixedArray< OperatorNeighborhoodType, 2 * itkGetStaticConstMacro(ImageDimension2) > OperatorArrayType;
  typedef NeighborhoodOperatorImageFunction< InputImageType,
                                             TOutput > OperatorImageFunctionType;
  typedef typename OperatorImageFunctionType::Pointer OperatorImageFunctionPointer;

  typedef GaussianDerivativeSpatialFunction< TOutput, 1 >  GaussianDerivativeFunctionType;
  typedef typename GaussianDerivativeFunctionType::Pointer GaussianDerivativeFunctionPointer;

  typedef GaussianSpatialFunction< TOutput, 1 >  GaussianFunctionType;
  typedef typename GaussianFunctionType::Pointer GaussianFunctionPointer;

  /** Point typedef support. */
  // typedef Point< TOutput, itkGetStaticConstMacro(ImageDimension2) > PointType;
  typedef typename InputImageType::PointType PointType;

  /** Evaluate the function at the specifed point. */
  OutputType Evaluate(const PointType & point) const override;

  /** Evaluate the function at specified Index position. */
  OutputType EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at specified ContinuousIndex position. */
  OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const override;

  /**
   * UseImageSpacing controls the extent of the computations.
   * Set UseImageSpacing to true to set the units to physical units of the image.
   * Set UseImageSpacing to false to set the units of pixels. */
  void SetUseImageSpacing(const bool val)
  {
    if(val != this->m_UseImageSpacing)
    {
      this->m_UseImageSpacing = val;
      this->RecomputeGaussianKernel();
    }
  }
  itkBooleanMacro(UseImageSpacing);
  itkGetMacro(UseImageSpacing,bool);

  /** The variance for the discrete Gaussian kernel. Sets the variance
   * independently for each dimension, but see also
   * SetVariance(const double v). The default is 0.0 in each dimension.
   * The extent of the kernel is controlled by UseImageSpacing.
   */
  void SetSigma(const double *sigma);

  void SetSigma(const double sigma);

  const double * GetSigma() const { return m_Sigma; }

  /** Set the extent of the discrete Gaussian kernel. */
  void SetExtent(const double *extent);

  void SetExtent(const double extent);

  const double * GetExtent() const { return m_Extent; }

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  void SetInputImage(const InputImageType *ptr) override;

protected:
  GaussianDerivativeImageFunction();
  GaussianDerivativeImageFunction(const Self &);

  ~GaussianDerivativeImageFunction() override {}

  void operator=(const Self &);

  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** Recompute the Gaussian kernel used to evaluate indexes. This should use
   * a fastest Derivative Gaussian operator. */
  void RecomputeGaussianKernel();

  /** Recompute the Gaussian kernel used to evaluate indexes. The variance
   * should be uniform. */
  void RecomputeContinuousGaussianKernel(
    const double *offset) const;

private:

  double                            m_Sigma[ImageDimension2];

  /** Array of 1D operators. Contains a derivative kernel and a gaussian kernel for
   * each dimension. */
  mutable OperatorArrayType         m_OperatorArray;
  mutable OperatorArrayType         m_ContinuousOperatorArray;

  /** OperatorImageFunction */
  OperatorImageFunctionPointer      m_OperatorImageFunction;
  double                            m_Extent[ImageDimension2];

  /** Flag to indicate whether to use image spacing. */
  bool m_UseImageSpacing;

  /** Neighborhood Image Function. */
  GaussianDerivativeFunctionPointer m_GaussianDerivativeFunction;
  GaussianFunctionPointer           m_GaussianFunction;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianDerivativeImageFunction.hxx"
#endif

#endif
