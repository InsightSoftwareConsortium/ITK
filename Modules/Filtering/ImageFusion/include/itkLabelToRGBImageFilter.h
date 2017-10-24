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
#ifndef itkLabelToRGBImageFilter_h
#define itkLabelToRGBImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkLabelToRGBFunctor.h"

namespace itk
{
/** \class LabelToRGBImageFilter
 * \brief Apply a colormap to a label image
 *
 * Apply a colormap to a label image. The set of colors
 * is a good selection of distinct colors. The user can choose to use a background
 * value. In that case, a gray pixel with the same intensity than the background
 * label is produced.
 *
 * This code was contributed in the Insight Journal paper:
 * "The watershed transform in ITK - discussion and new developments"
 * by Beare R., Lehmann G.
 * https://hdl.handle.net/1926/202
 * http://www.insight-journal.org/browse/publication/92
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \sa  LabelOverlayImageFilter
 * \sa  LabelMapToRGBImageFilter, LabelToRGBFunctor, ScalarToRGBPixelFunctor
 * \ingroup MultiThreaded
 *
 * \ingroup ITKImageFusion
 */
template< typename TLabelImage, typename  TOutputImage >
class ITK_TEMPLATE_EXPORT LabelToRGBImageFilter:
  public
  UnaryFunctorImageFilter< TLabelImage, TOutputImage,
                           Functor::LabelToRGBFunctor<
                             typename TLabelImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef LabelToRGBImageFilter      Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef UnaryFunctorImageFilter< TLabelImage, TOutputImage,
                                   Functor::LabelToRGBFunctor<
                                     typename TLabelImage::PixelType,
                                     typename TOutputImage::PixelType >   >  Superclass;

  typedef TOutputImage OutputImageType;
  typedef TLabelImage  LabelImageType;

  typedef typename TOutputImage::PixelType                     OutputPixelType;
  typedef typename TLabelImage::PixelType                      LabelPixelType;
  typedef typename NumericTraits< OutputPixelType >::ValueType OutputPixelValueType;

  /** Runtime information support. */
  itkTypeMacro(LabelToRGBImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the background value */
  itkSetMacro(BackgroundValue, LabelPixelType);
  itkGetConstReferenceMacro(BackgroundValue, LabelPixelType);

  /** Set/Get the background color in the output image */
  itkSetMacro(BackgroundColor, OutputPixelType);
  itkGetConstReferenceMacro(BackgroundColor, OutputPixelType);

  /** Empty the color LUT container */
  void ResetColors();

  /** Get number of colors in the LUT container */
  unsigned int GetNumberOfColors() const;

  /** Type of the color component */
  typedef typename OutputPixelType::ComponentType ComponentType;

  /** Add color to the LUT container */
  void AddColor(ComponentType r, ComponentType g, ComponentType b);

protected:
  LabelToRGBImageFilter();
  virtual ~LabelToRGBImageFilter() ITK_OVERRIDE {}

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Print internal ivars */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateOutputInformation() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelToRGBImageFilter);

  OutputPixelType m_BackgroundColor;
  LabelPixelType  m_BackgroundValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelToRGBImageFilter.hxx"
#endif

#endif
