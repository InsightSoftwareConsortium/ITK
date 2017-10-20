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
#ifndef itkHessianImageFilter_h
#define itkHessianImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{

/**
 * \class HessianImageFilter
 * \brief Computes the Hessian matrix of an image by central differences
 *
 * This filter directly computes the derivatives of an image for
 * the Hessian and does not convolve with a Gaussian kernel.
 *
 * \sa HessianRecursiveGaussianImageFilter
 * \sa SmoothingRecursiveGaussianImageFilter
 *
 * \ingroup Streamed
 * \ingroup SimpleITKFilters
 */
template <typename TInputImage,
          typename TOutputImage = Image< SymmetricSecondRankTensor<
                                           typename NumericTraits< typename TInputImage::PixelType >::RealType,
                                           TInputImage::ImageDimension >,
                                         TInputImage::ImageDimension > >
class HessianImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage>
{
public:

  /** Standard typedefs */
  typedef HessianImageFilter                           Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;


  /** Pixel Type of the input image */
  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType PixelType;

  /** Type of the output Image */
  typedef TOutputImage                                 OutputImageType;
  typedef typename          OutputImageType::PixelType OutputPixelType;
  typedef typename OutputImageType::RegionType         OutputImageRegionType;


 /** Run-time type information (and related methods).   */
  itkTypeMacro( HessianImageFilter, ImageToImageFilter );

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;


#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<PixelType>));
  itkConceptMacro(OutputHasPixelTraitsCheck,
                  (Concept::HasPixelTraits<OutputPixelType>));
  /** End concept checking */
#endif

protected:

  HessianImageFilter( void );

  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HessianImageFilter);

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHessianImageFilter.hxx"
#endif


#endif //itkHessianImageFilter_h
