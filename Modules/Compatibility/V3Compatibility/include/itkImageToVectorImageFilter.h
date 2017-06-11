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
#ifndef itkImageToVectorImageFilter_h
#define itkImageToVectorImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorContainer.h"
#include "itkVectorImage.h"

#ifndef ITKV3_COMPATIBILITY
#error "This file is only valid when ITKV3_COMPATIBILITY is turned on. Users are encouraged to convert to itk::ComposeImageFilter in ITKv4"
#endif

namespace itk
{
/** \class ImageToVectorImageFilter
 * \brief  This class is deprecated. You should use itkComposeImageFilter instead.
 *
 * This class takes as input 'n' itk::Image's and composes them into
 * a single itk::VectorImage.
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
 * \deprecated
 * \ingroup ITKV3Compatibility
 *
 * \wiki
 * \wikiexample{VectorImages/ImageToVectorImageFilter,Stack scalar images into a VectorImage}
 * \endwiki
 */

template< typename TInputImage >
class ITK_TEMPLATE_EXPORT ImageToVectorImageFilter:
  public ImageToImageFilter< TInputImage,
                             VectorImage< typename TInputImage::InternalPixelType,
                                          TInputImage::ImageDimension > >
{
public:

  typedef ImageToVectorImageFilter   Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(ImageToVectorImageFilter, ImageToImageFilter);

  itkStaticConstMacro(Dimension, unsigned int, TInputImage::ImageDimension);

  typedef typename TInputImage::InternalPixelType PixelType;
  typedef VectorImage< PixelType,
                       itkGetStaticConstMacro(Dimension) >   OutputImageType;
  typedef ImageToImageFilter< TInputImage,
                              OutputImageType >     Superclass;

  typedef typename Superclass::InputImageType InputImageType;

  typedef typename Superclass::InputImageRegionType RegionType;

  virtual void SetNthInput(unsigned int idx, const InputImageType *inputImage)
  { this->SetInput(idx, inputImage); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // Check if the pixeltype is a scalar, (native pixel type).
  // End concept checking
#endif

protected:
  ImageToVectorImageFilter();

  virtual void GenerateOutputInformation();

  virtual void BeforeThreadedGenerateData();

  virtual void ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType);

  virtual void SetNthInput(unsigned int num, DataObject *input)
  {
    Superclass::SetNthInput(num, input);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToVectorImageFilter);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToVectorImageFilter.hxx"
#endif

#endif
