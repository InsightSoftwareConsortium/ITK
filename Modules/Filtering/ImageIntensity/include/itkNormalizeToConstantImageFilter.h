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
#ifndef itkNormalizeToConstantImageFilter_h
#define itkNormalizeToConstantImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConceptChecking.h"

namespace itk {

/** \class NormalizeToConstantImageFilter
 *
 * \brief Scales image pixel intensities to make the sum of all pixels
 * equal a user-defined constant.
 *
 * The default value of the constant is 1. It can be changed with
 * SetConstant().
 *
 * This transform is especially useful for normalizing a convolution
 * kernel.
 *
 * This code was contributed in the Insight Journal paper: "FFT based
 * convolution" by Lehmann G.
 * https://hdl.handle.net/10380/3154
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa NormalizeImageFilter
 * \sa StatisticsImageFilter
 * \sa DivideImageFilter
 *
 * \ingroup MathematicalImageFilters
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/NormalizeToConstantImageFilter,Scale all pixels so that their sum is a specified constant}
 * \endwiki
 */
template<typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT NormalizeToConstantImageFilter :
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef NormalizeToConstantImageFilter Self;

  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef TOutputImage                             OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename InputImageType::RegionType      RegionType;
  typedef typename InputImageType::IndexType       IndexType;
  typedef typename InputImageType::SizeType        SizeType;

  typedef typename NumericTraits<InputImagePixelType>::RealType RealType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(NormalizeToConstantImageFilter, ImageToImageFilter);

  /** Set/get the normalization constant. */
  itkSetMacro(Constant, RealType);
  itkGetConstMacro(Constant, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasPixelTraitsCheck,
    (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<InputImagePixelType>));
  // End concept checking
#endif

protected:
  NormalizeToConstantImageFilter();
  virtual ~NormalizeToConstantImageFilter() ITK_OVERRIDE {};
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NormalizeToConstantImageFilter);

  RealType m_Constant;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizeToConstantImageFilter.hxx"
#endif

#endif
