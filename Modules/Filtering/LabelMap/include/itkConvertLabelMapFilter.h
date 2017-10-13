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
#ifndef itkConvertLabelMapFilter_h
#define itkConvertLabelMapFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{
/** \class ConvertLabelMapFilter
 * \brief Converts the LabelObjects of a LabelMap to a differente type of LabelObejct
 *
 * The LabelObjects are copied and not simply dynamically casted so the filter ensures
 * that the type of the label objects are of the type specified with TOutputImage.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapMaskImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup LabeledImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ConvertLabelMapFilter:
  public LabelMapFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ConvertLabelMapFilter                       Self;
  typedef LabelMapFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                                 InputImageType;
  typedef TOutputImage                                OutputImageType;
  typedef typename InputImageType::Pointer            InputImagePointer;
  typedef typename InputImageType::ConstPointer       InputImageConstPointer;
  typedef typename InputImageType::RegionType         InputImageRegionType;
  typedef typename InputImageType::PixelType          InputImagePixelType;
  typedef typename InputImageType::LabelObjectType    LabelObjectType;

  typedef typename OutputImageType::Pointer           OutputImagePointer;
  typedef typename OutputImageType::ConstPointer      OutputImageConstPointer;
  typedef typename OutputImageType::RegionType        OutputImageRegionType;
  typedef typename OutputImageType::PixelType         OutputImagePixelType;
  typedef typename OutputImageType::IndexType         IndexType;
  typedef typename OutputImageType::LabelObjectType   OutputLabelObjectType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ConvertLabelMapFilter, LabelMapFilter);

protected:
  ConvertLabelMapFilter() {}
  ~ConvertLabelMapFilter() ITK_OVERRIDE {}

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConvertLabelMapFilter);
};                                          // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvertLabelMapFilter.hxx"
#endif

#endif
