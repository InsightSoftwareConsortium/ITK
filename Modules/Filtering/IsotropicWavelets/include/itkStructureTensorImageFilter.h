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
#ifndef itkStructureTensorImageFilter_h
#define itkStructureTensorImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkImageScanlineConstIterator.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkArray.h>
#include <itkVariableSizeMatrix.h>
#include <itkSymmetricSecondRankTensor.h>
#include <itkGaussianImageSource.h>
namespace itk
{
/** \class StructureTensorImageFilter
 * Given an array of inputs, StructureTensor computes the linear combination (or direction) of inputs that maximizes the
response for each location in the image.
 * Instead of only measuring the response at the pixel of interest, it takes into account a local neighborhood.
 *
 * Implementation based on article:
 * Unser, M., Chenouard, N. and Van De Ville, D.
 * Steerable Pyramids and Tight Wavelet Frames in \f$L_{2}(\mathcal{R}^d)\f$,
 * IEEE Transactions on Image Processing,
 * DOI: 10.1109/TIP.2011.2138147
 *
\f[
 \mathbf{u}({\mathbf{x}_0}) = \arg \max_{\Vert\mathbf{u}\Vert =1 }  \int_{\mathbb{R}^d} g(\mathbf{x} - \mathbf{x}_0)
\left| \mathbf{I}_{\mathbf{u}}(\mathbf{x})\right|^2 \f] \f[ \left| \mathbf{I}_{\mathbf{u}}(\mathbf{x})\right|^2 =
 \mathbf{u}^T \cdot \mathbf{I}(\mathbf{x}) \cdot (\mathbf{I}(\mathbf{x}))^T \cdot \mathbf{u}
\f]
 * \f$ \mathbf{I}\f$ is the required std::vector of input images. These images might be the output
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
 * The orientation that maximises the response: \f$u\f$ is the EigenVector with largest EigenValue, which is is the Nth
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
template <typename TInputImage,
          typename TOutputImage = itk::Image<itk::VariableSizeMatrix<double>, TInputImage::ImageDimension>>
class StructureTensorImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StructureTensorImageFilter);

  /** Some convenient type alias. */
  /** Standard class type alias. */
  using Self = StructureTensorImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StructureTensorImageFilter, ImageToImageFilter);

  /** Some convenient type alias. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;

  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using SpacingType = typename InputImageType::SpacingType;
  using SizeType = typename InputImageRegionType::SizeType;

  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using FloatType = double;
  using FloatImageType = itk::Image<FloatType, ImageDimension>;
  using FloatImagePointer = typename FloatImageType::Pointer;

#ifdef ITK_USE_CONCEPT_CHECKING
  // This ensure that PixelType is float||double, and not complex.
  itkConceptMacro(InputPixelTypeIsFloatCheck, (Concept::IsFloatingPoint<typename TInputImage::PixelType>));
#endif
  using EigenMatrixImageType = OutputImageType;
  using EigenMatrixType = OutputImagePixelType;
  using EigenValuesType = itk::Array<typename OutputImagePixelType::ValueType>;
  using SymmetricEigenAnalysisType = itk::SymmetricEigenAnalysis<EigenMatrixType, EigenValuesType>;
  using GaussianSourceType = GaussianImageSource<FloatImageType>;

  using InputsType = typename std::vector<InputImagePointer>;
  // using InputsType = typename itk::VectorContainer<int, InputImagePointer>;
  //
  void
  SetInputs(const InputsType & inputs);

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
   * meanNonPrincipalEV \f$ = M = \frac{1}{N-1} \sum_{i=1}^{N-1}\lambda_i \f$
   * coherency \f$ = \frac{\lambda_N - M}{\lambda_1 + M} \f$
   * where \f$ \lambda_N \f$ is the largest eigen value.
   *
   * @return Image filled with the coherency at each pixel.
   */
  InputImagePointer
  ComputeCoherencyImage() const;

  /**
   * From the output matrix at each index, computes the rotation matrix,
   * which is the transpose of the eigenvector matrix computed in the output of
   * this image filter.
   * If reOrderLargestEigenvectorInFirstRow is true the first row of the rotation
   * matrix will have the largest eigenvector. If false (the default), the largest
   * eigenvector will be in the last row.
   *
   * @param outputMatrix matrix at any index stored in the output of this filter.
   * @param reOrderLargestEigenvectorInFirstRow flag to reorder eigenvectors.
   *
   * @return rotationMatrix
   */
  EigenMatrixType
  GetRotationMatrixFromOutputMatrix(const EigenMatrixType & outputMatrix,
                                    bool                    reOrderLargestEigenvectorInFirstRow = false) const;

protected:
  StructureTensorImageFilter();
  ~StructureTensorImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  BeforeThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  /** Assuming that row>=column */
  static unsigned int
  LowerTriangleToLinearIndex(unsigned int r, unsigned int c)
  {
    return r + (c + 1) * c / 2;
  }

private:
  unsigned int                         m_GaussianWindowRadius{ 2 };
  FloatType                            m_GaussianWindowSigma{ 1.0 };
  typename GaussianSourceType::Pointer m_GaussianSource;
  InputsType                           m_SquareSmoothedImages;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructureTensorImageFilter.hxx"
#endif

#endif
