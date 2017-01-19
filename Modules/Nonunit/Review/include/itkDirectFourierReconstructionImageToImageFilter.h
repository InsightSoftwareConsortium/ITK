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
#ifndef itkDirectFourierReconstructionImageToImageFilter_h
#define itkDirectFourierReconstructionImageToImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageSliceConstIteratorWithIndex.h"

#include "itkComplexBSplineInterpolateImageFunction.h"

#include <cmath>

namespace itk
{
/**
 * \class DirectFourierReconstructionImageToImageFilter
 * \brief Direct fourier reconstruction filter of a tomographic volume.
 *
 * The algorithm is detailed in the Insight Journal publication on
 * "Direct Fourier Tomographic Reconstruction Image-to-Image Filter"
 * by D. Zosso, M. Bach Cuadra and J. Thiran, August 2007
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/585
 *
 * \ingroup ImageFilters
 * \ingroup ITKReview
 */
template< typename TInputImage, typename TOutputImage=TInputImage >
class ITK_TEMPLATE_EXPORT DirectFourierReconstructionImageToImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef DirectFourierReconstructionImageToImageFilter Self;

  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  /** Standard Superclass typedef */
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;

  /** Standard Pointer typedef */
  typedef SmartPointer< Self > Pointer;
  /** Standard ConstPointer typedef */
  typedef SmartPointer< const Self > ConstPointer;

  itkNewMacro(Self);
  itkTypeMacro(DirectFourierReconstructionImageToImageFilter, ImageToImageFilter);

  /** Class RegionType */
  typedef typename InputImageType::RegionType RegionType;
  /** Class IndexType */
  typedef typename InputImageType::IndexType IndexType;
  /** Class SizeType */
  typedef typename InputImageType::SizeType SizeType;
  /** Class PointType */
  typedef typename InputImageType::PointType PointType;
  /** Class SpacingType */
  typedef typename InputImageType::SpacingType SpacingType;

  /** Standard (const) InputImagePointer */
  typedef typename InputImageType::ConstPointer ConstInputImagePointer;
  /** Special (non-const) InputImagePointer */
  typedef typename InputImageType::Pointer InputImagePointer;
  /** OutputImagePointer */
  typedef typename OutputImageType::Pointer OutputImagePointer;

  itkSetMacro(ZeroPadding, unsigned short int);
  itkGetConstMacro(ZeroPadding, unsigned short int);

  itkSetMacro(OverSampling, unsigned short int);
  itkGetConstMacro(OverSampling, unsigned short int);

  itkSetMacro(Cutoff, double);
  itkGetConstMacro(Cutoff, double);

  itkSetMacro(AlphaRange, double);
  itkGetConstMacro(AlphaRange, double);

  itkSetMacro(AlphaDirection, unsigned short int);
  itkGetConstMacro(AlphaDirection, unsigned short int);

  itkSetMacro(ZDirection, unsigned short int);
  itkGetConstMacro(ZDirection, unsigned short int);

  itkSetMacro(RDirection, unsigned short int);
  itkGetConstMacro(RDirection, unsigned short int);

  itkSetMacro(RadialSplineOrder, unsigned short int);
  itkGetConstMacro(RadialSplineOrder, unsigned short int);

protected:
  /** Constructor */
  DirectFourierReconstructionImageToImageFilter();
  /** Destructor */
  ~DirectFourierReconstructionImageToImageFilter() {}

  /** Output class information */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate metadata for output image */
  void GenerateOutputInformation() ITK_OVERRIDE;

  /** Calculate the required input region */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Actual filter computation */
  void GenerateData() ITK_OVERRIDE;

private:
  /** Const slice iterator type of the input image */
  typedef ImageSliceConstIteratorWithIndex< InputImageType > InputSliceIteratorType;

  /** 1D FFT filter type */
  typedef Image< double, 1 >                                       LineImageType;
  typedef VnlForwardFFTImageFilter< LineImageType > FFTLineFilterType;
  /** Derived 1D FFT image type */
  typedef FFTLineFilterType::OutputImageType FFTLineType;
  /** Derived 1D input image type */
  typedef FFTLineFilterType::InputImageType ProjectionLineType;
  /** 1D FFT line iterator */
  typedef ImageRegionIteratorWithIndex< FFTLineType > FFTLineIteratorType;
  /** 1D FFT line B-Spline interpolator */
  typedef ComplexBSplineInterpolateImageFunction< FFTLineType, double, double > FFTLineInterpolatorType;

  /** 2D inverse FFT filter type */
  typedef Image< std::complex<double>, 2>                          IFFTImageType;
  typedef VnlInverseFFTImageFilter< IFFTImageType > IFFTSliceFilterType;
  /** Derived 2D FFT image type */
  typedef IFFTSliceFilterType::InputImageType FFTSliceType;
  /** Derived 2D output slice type */
  typedef IFFTSliceFilterType::OutputImageType OutputSliceType;
  /** 2D FFT slice iterator */
  typedef ImageRegionIteratorWithIndex< FFTSliceType > FFTSliceIteratorType;
  /** 2D output slice iterator */
  typedef ImageRegionIteratorWithIndex< OutputSliceType > OutputSliceIteratorType;

  unsigned short int m_ZeroPadding;       /**< n-fold zero-padding */
  unsigned short int m_OverSampling;      /**< n-fold oversampling */

  double m_Cutoff;                        /**< Radial lowpass cut-off frequency
                                            */
  double m_AlphaRange;                    /**< Covered angular range */

  unsigned short int m_ZDirection;        /**< Axial index in the input image */
  unsigned short int m_AlphaDirection;    /**< Angular index in the input image
                                            */
  unsigned short int m_RDirection;        /**< Radial index in the input image
                                            */
  unsigned short int m_RadialSplineOrder; /**< Spline order for the radial
                                            BSpline interpolation  */

  double m_PI; /**< The constant pi....  */

  RegionType m_InputRequestedRegion; /**< The region requested from* the input
                                       image   */

  ITK_DISALLOW_COPY_AND_ASSIGN(DirectFourierReconstructionImageToImageFilter);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDirectFourierReconstructionImageToImageFilter.hxx"
#endif

#endif /* itkDirectFourierReconstructionImageToImageFilter_h */
