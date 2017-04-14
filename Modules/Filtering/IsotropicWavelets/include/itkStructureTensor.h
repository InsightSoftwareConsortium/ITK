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
#ifndef itkStructureTensor_h
#define itkStructureTensor_h

#include <itkImageToImageFilter.h>
#include <itkImageScanlineConstIterator.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkArray.h>
#include <itkVariableSizeMatrix.h>
#include <itkSymmetricSecondRankTensor.h>
#include <itkGaussianImageSource.h>
namespace itk
{
/** \class StructureTensor
 * StructureTensor provides a procedure to estimate the orientation (or linear combination of inputs) that maximizes
--at every pixel-- the response.
 *
\f[
 \mathbf{u}({\mathbf{x}_0}) = \arg \max_{\Vert\mathbf{u}\Vert =1 }  \int_{\mathbb{R}^d} g(\mathbf{x} - \mathbf{x}_0)
\left| \mathbf{I}_{\mathbf{u}}(\mathbf{x})\right|^2
\f]
\f[
 \left| \mathbf{I}_{\mathbf{u}}(\mathbf{x})\right|^2 =
 \mathbf{u}^T \cdot \mathbf{I}(\mathbf{x}) \cdot (\mathbf{I}(\mathbf{x}))^T \cdot \mathbf{u}
\f]
 * \f$ \matbf{I}\f$ is the required std::vector of input images. These images might be the output
 * after applying a directional filter to an image (for example, directional derivatives from an image, or the basis of
a steerable filter, such as a RieszImageFilter).
 * Instead of just select the max response at every pixel, it uses the response over
 * a local neighborhood that is specified using an isotropic Gaussian window \f$g(\mathbf{x}\f$.
 * This approach is more robust against noise. The user can control the radius and sigma of this gaussian kernel.
 * Estimation of the local orientation this way results in an eigen system with matrix:
 * \f[
 [\mathbf{J}(\mathbf{x}_0)]_{mn} = \sum_{\mathbf{x} \in \mathbb{Z}^d} g(\mathbf{x} - \mathbf{x}_0)
I_m[\mathbf{x}]I_n[\mathbf{x}]
 * \f]
 * where \f$I_m, I_n \f$ are input images, \f$ m,n \in {0,N-1} \f$ and \f$N\f$ is the total number of inputs.
 * \f$g\f$ is a gaussian kernel of radius SetGaussianWindowRadius()
 *
 * The solution of the EigenSystem defined by \f$\mathbf{J}\f$ are the N EigenValues and EigenVectors.
 * The output of StructureTensor is a 2D Matrix of size (N,N+1), where the submatrix (N,N) are the EigenVectors, and the
last column (N+1) are the EigenValues.
 * The orientation that maximixes the response: \f$u\f$ is the EigenVector with largest EigenValue, which is is the Nth
column of the output matrix.
 * We can use the calculated direction \f$u\f$ to get a new image with max response from the inputs at each pixel.
 * \see ComputeProjectionImageWithLargestResponse(),
 * or any other direction from other eigen vectors with \see ComputeProjectionImage(unsigned int eigen_index)
 *
 * Also we can compare eigen values to study the local coherency of each pixel:
 \f[
 \chi (\mathbf{x}_0)= \frac{\lambda_N(\mathbf{x}_0) - A(\mathbf{x}_0)}{\lambda_N(\mathbf{x}_0) + A(\mathbf{x}_0)}
\f]
where \f$\lambda_N(\mathbf{x}_0)\f$ is the largest eigen value at pixel \f$\mathbf{x}_0\f$ ,
and
\f$A(\mathbf{x}_0) = \frac{1}{N-1}\sum_{i=1}^{N-1}\lambda_i(\mathbf{x}_0)\f$ is the average of the other eigen values.
 * \see RieszImageFilter
 * \see SymmetricEigenAnalysis
 *
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage>
class StructureTensor
  : public ImageToImageFilter<TInputImage, Image<itk::VariableSizeMatrix<double>, TInputImage::ImageDimension>>
{
public:
  /** Some convenient typedefs. */
  /** Standard class typedefs. */
  typedef StructureTensor Self;
  typedef ImageToImageFilter<TInputImage, Image<itk::VariableSizeMatrix<double>, TInputImage::ImageDimension>>
                                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StructureTensor, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef double                                FloatType;
  typedef itk::Image<FloatType, ImageDimension> FloatImageType;
  typedef typename FloatImageType::Pointer      FloatImagePointer;

  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  typedef typename InputImageType::SpacingType    SpacingType;
  typedef typename InputImageRegionType::SizeType SizeType;

  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

#ifdef ITK_USE_CONCEPT_CHECKING
  /// This ensure that PixelType is float||double, and not complex.
  // itkConceptMacro( OutputPixelTypeIsFloatCheck,
  //                ( Concept::IsFloatingPoint< typename TOutputImage::PixelType > ) );
#endif
  typedef OutputImageType                                               EigenMatrixImageType;
  typedef itk::VariableSizeMatrix<FloatType>                            EigenMatrixType;
  typedef itk::Array<FloatType>                                         EigenValuesType;
  typedef itk::SymmetricEigenAnalysis<EigenMatrixType, EigenValuesType> SymmetricEigenAnalysisType;
  typedef GaussianImageSource<FloatImageType>                           GaussianSourceType;

  void
  SetInputs(const std::vector<InputImagePointer> & inputs);

  /**
   * Set/Get Radius of the gaussian window.
   * The window determines the size of the local neighborhood of each pixel.
   */
  itkSetMacro(GaussianWindowRadius, FloatType);
  itkGetConstMacro(GaussianWindowRadius, FloatType);
  /**
   * Set/Get Sigma of the GaussianSource.
   * \sa GaussianImageSource
   */
  itkSetMacro(GaussianWindowSigma, FloatType);
  itkGetConstMacro(GaussianWindowSigma, FloatType);
  /**
   * Pointer to the GaussianSource.
   * \sa GaussianImageSource
   */
  itkGetModifiableObjectMacro(GaussianSource, GaussianSourceType);

  /**
   * Compute a new image which is a linear combination of the inputs.
   * The weights of the linear combination are given by the eigenVector
   * associated to the input eigen_number.
   *
   * @param eigen_number column of the eigenVector, note that the largest eigenValue is in Nth column.
   *
   * @return Image where pixels are filled with the linear combination of inputs associated to the input eigen number.
   */
  InputImagePointer
  ComputeProjectionImage(unsigned int eigen_number) const;

  /**
   * Call ComputeProjectionImage with the position of the largest eigenValue (Nth column).
   *
   * @return Image where pixels are filled with the linear combination of inputs associated to the largest eigen value.
   * \sa ComputeProjectionImage
   */
  InputImagePointer
  ComputeProjectionImageWithLargestResponse() const;

  /**
   * At each pixel, coherency is calculated based on the relative value of eigenValues.
   * meanNonPrincipalEV = M = \frac{1}{N-1} \sum_{i=1}^{N-1}\lambda_i
   * coherency = \frac{\lambda_N - M}{\lambda_1 + M}
   * where \(\lambda_N\) is the largest eigen value.
   *
   * @return Image filled with the coherency at each pixel.
   */
  InputImagePointer
  ComputeCoherencyImage() const;

protected:
  StructureTensor();
  ~StructureTensor() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  /** Assuming that row>=column */
  static unsigned int
  LowerTriangleToLinearIndex(unsigned int r, unsigned int c)
  {
    return r + (c + 1) * c / 2;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StructureTensor);
  // User can select value
  unsigned int                         m_GaussianWindowRadius;
  FloatType                            m_GaussianWindowSigma;
  typename GaussianSourceType::Pointer m_GaussianSource;
  std::vector<InputImagePointer>       m_SquareSmoothedImages;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructureTensor.hxx"
#endif

#endif
