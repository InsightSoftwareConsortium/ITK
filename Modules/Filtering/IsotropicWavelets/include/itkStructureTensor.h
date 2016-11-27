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
#include <itkVectorImage.h>
#include <itkImageScanlineConstIterator.h>
#include <itkImageRegionConstIterator.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkImage.h>
#include <itkFixedArray.h>
#include <itkArray.h>
#include <itkVariableSizeMatrix.h>
#include <itkSymmetricSecondRankTensor.h>
#include "itkBarrier.h"
namespace itk
{
/** \class StructureTensor
 * The input of StructureTensor is a VectorImage, where each component represents a kind of basis to the same underlying
 image (for example, the basis might be: partial derivatives, Riesz basis, and so on).
 * At each pixel of the images, a gaussian window of radius chosen by user (r=2), is used to delimit the local
 neighborhood of each pixel to look for the structure.
 * At each pixel of each input image, we have a neighborhood of size = radius*2 + 1. This can be seen as a matrix M x M
 where M = NumberOfComponents or size of input vector.
 * \f[
 J_{\mathbf{k}_0} = \sum_{\mathbf{k} \in \mathbb{Z}^d} v[\mathbf{k} - \mathbf{k}_0]
 \mathbf{v}[\mathbf{k}]\mathbf{v}^T[\mathbf{k}]
 * \f]
 * The outputs are:
 * A) Image of Matrices MxM representing eigenvectors in each row.
 * B) Image of itkFixedArray containing eigenvalues
 * \sa itkWaveletFrequencyInverse
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
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef double                               FloatType;

  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  typedef typename InputImageType::SpacingType    SpacingType;
  typedef typename InputImageRegionType::SizeType SizeType;


  typedef typename OutputImageType::Pointer                           OutputImagePointer;
  typedef typename OutputImageType::ConstPointer                      OutputImageConstPointer;
  typedef typename OutputImageType::RegionType                        OutputImageRegionType;
  typedef typename itk::ImageRegionIteratorWithIndex<OutputImageType> OutputImageRegionIterator;
  typedef typename OutputImageType::PixelType                         OutputImagePixelType;
  typedef typename itk::ImageScanlineConstIterator<InputImageType>    InputImageConstIterator;

#ifdef ITK_USE_CONCEPT_CHECKING
  /// This ensure that PixelType is float||double, and not complex.
  // itkConceptMacro( OutputPixelTypeIsFloatCheck,
  //                ( Concept::IsFloatingPoint< typename TOutputImage::PixelType > ) );
#endif
  typedef OutputImageType                                               EigenMatrixImageType;
  typedef itk::VariableSizeMatrix<FloatType>                            EigenMatrixType;
  typedef itk::Array<FloatType>                                         EigenValuesType;
  typedef itk::SymmetricEigenAnalysis<EigenMatrixType, EigenValuesType> SymmetricEigenAnalysisType;

  void
  SetInputs(const std::vector<InputImagePointer> & inputs);
  itkSetMacro(GaussianWindowRadius, FloatType);
  itkGetConstMacro(GaussianWindowRadius, FloatType);
  itkSetMacro(GaussianWindowSigma, FloatType);
  itkGetConstMacro(GaussianWindowSigma, FloatType);

protected:
  StructureTensor();
  ~StructureTensor() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StructureTensor);
  /** Disallow use of SetInput, user should call SetInputs instead */
  virtual void
  SetInput(const InputImageType * input) ITK_OVERRIDE
  {
    Superclass::SetInput(input);
  };
  // User can select value
  unsigned int m_GaussianWindowRadius;
  FloatType    m_GaussianWindowSigma;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStructureTensor.hxx"
#endif

#endif
