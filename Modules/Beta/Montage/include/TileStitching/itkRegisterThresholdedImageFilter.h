/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#ifndef __itkRegisterThresholdedImageFilter_h
#define __itkRegisterThresholdedImageFilter_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkNormalizeImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkRegisterImageTranslationFilter.h"

namespace itk
{
/**
 * This class normalizes. blurrs and thresholds to segment the input fixed and
 * moving images into binary images. It then computes a transform by aligning
 * the two binary images. Only translation is estimated so far. However but
 * replacing RegisterImageTranslationFilter with other types of registration
 * filters, this class can be extended to address full rigid or affine or even
 * deformable registrations.
 */

template < typename ImagePixelType, unsigned int VImageDimension >
class ITK_EXPORT RegisterThresholdedImageFilter  : public ProcessObject
{
public:
  typedef  RegisterThresholdedImageFilter  Self;
  typedef  ProcessObject                   Superclass;
  typedef  SmartPointer<Self>              Pointer;

  itkNewMacro( Self );

  typedef double TransformCoordType;

  typedef typename itk::Image< ImagePixelType, VImageDimension > ImageType;
  typedef typename ImageType::Pointer               ImagePointerType;
  typedef typename ImageType::RegionType            ImageRegionType;
  typedef typename ImageType::IndexType             ImageIndexType;
  typedef typename ImageType::SizeType              ImageSizeType;

  typedef itk::Image<float, VImageDimension>        FloatImageType;
  typedef typename FloatImageType::Pointer          FloatImagePointerType;

  typedef itk::RegisterImageTranslationFilter<ImagePixelType, VImageDimension>
                                                    RegistrationImageFilterType;
  typedef typename RegistrationImageFilterType::Pointer
                                             RegistrationImageFilterPointerType;
  typedef typename RegistrationImageFilterType::ParametersType   ParametersType;

  const ParametersType & GetOutputParameters();

  virtual void Update();

  void PrepareImages();

  itkSetMacro(FixedImage, ImagePointerType);
  itkGetMacro(FixedImage, ImagePointerType);

  itkSetMacro(MovingImage, ImagePointerType);
  itkGetMacro(MovingImage, ImagePointerType);

  itkGetMacro(PreprocessFlag, int);
  itkSetMacro(PreprocessFlag, int);

  RegisterThresholdedImageFilter();
  virtual ~RegisterThresholdedImageFilter();

  ImagePointerType BlurImage(ImagePointerType image);
  ImagePointerType NormalizeImage(ImagePointerType image);
  ImagePointerType ThresholdImage(ImagePointerType image);

protected:
  ImagePointerType                      m_FixedImage;
  ImagePointerType                      m_MovingImage;
  RegistrationImageFilterPointerType    m_Registration;
  int                                   m_PreprocessFlag;

private:
  RegisterThresholdedImageFilter(
    const RegisterThresholdedImageFilter&);               //Not implemented.
  void operator=(const RegisterThresholdedImageFilter&);  //Not implemented.
};

} // end namespace itk

#include "itkRegisterThresholdedImageFilter.txx"

#endif
