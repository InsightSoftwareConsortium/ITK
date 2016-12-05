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
#ifndef itkComposeImageFilter_h
#define itkComposeImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorImage.h"
#include "itkImageRegionConstIterator.h"
#include <vector>

namespace itk
{
/** \class ComposeImageFilter
 * \brief ComposeImageFilter combine several scalar images into a multicomponent image
 *
 * ComposeImageFilter combine several scalar images into an itk::Image of
 * vector pixel (itk::Vector, itk::RGBPixel, ...), of std::complex pixel,
 * or in an itk::VectorImage.
 *
 * \par Inputs and Usage
 * \code
 *    filter->SetInput( 0, image0 );
 *    filter->SetInput( 1, image1 );
 *    ...
 *    filter->Update();
 *    itk::VectorImage< PixelType, dimension >::Pointer = filter->GetOutput();
 * \endcode
 * All input images are expected to have the same template parameters and have
 * the same size and origin.
 *
 * \sa VectorImage
 * \sa VectorIndexSelectionCastImageFilter
 * \ingroup ITKImageCompose
 *
 * \wiki
 * \wikiexample{VectorImages/ImageToVectorImageFilter,Create a vector image from a collection of scalar images}
 * \wikiexample{ImageProcessing/Compose3DCovariantVectorImageFilter,Compose a vector image (with 3 components) from three scalar images}
 * \wikiexample{SpectralAnalysis/RealAndImaginaryToComplexImageFilter,Convert a real image and an imaginary image to a complex image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage=VectorImage<typename TInputImage::PixelType, TInputImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT ComposeImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:

  typedef ComposeImageFilter                               Self;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;
  typedef ImageToImageFilter< TInputImage, TOutputImage >  Superclass;
  itkNewMacro(Self);
  itkTypeMacro(ComposeImageFilter, ImageToImageFilter);

  itkStaticConstMacro(Dimension, unsigned int, TInputImage::ImageDimension);

  typedef TInputImage                          InputImageType;
  typedef TOutputImage                         OutputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename InputImageType::RegionType  RegionType;

  void SetInput1(const InputImageType *image1);
  void SetInput2(const InputImageType *image2);
  void SetInput3(const InputImageType *image3);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputCovertibleToOutputCheck,
                   ( Concept::Convertible< InputPixelType, typename NumericTraits<OutputPixelType>::ValueType > ) );
  // End concept checking
#endif

protected:
  ComposeImageFilter();

  virtual void GenerateOutputInformation(void) ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ComposeImageFilter);


  // we have to specialize the code for complex, because it provides no operator[]
  // method
  typedef ImageRegionConstIterator< InputImageType > InputIteratorType;
  typedef std::vector< InputIteratorType >           InputIteratorContainerType;

  template<typename T>
  void ComputeOutputPixel(std::complex<T> & pix, InputIteratorContainerType & inputItContainer )
    {
    pix = std::complex<T>(inputItContainer[0].Get(), inputItContainer[1].Get());
    ++( inputItContainer[0] );
    ++( inputItContainer[1] );
    }
  template<typename TPixel>
  void ComputeOutputPixel(TPixel & pix, InputIteratorContainerType & inputItContainer)
    {
    for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
      {
      pix[i] = static_cast<typename NumericTraits<OutputPixelType>::ValueType >(inputItContainer[i].Get());
      ++( inputItContainer[i] );
      }
    }
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkComposeImageFilter.hxx"
#endif

#endif
