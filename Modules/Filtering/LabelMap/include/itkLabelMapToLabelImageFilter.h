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
#ifndef itkLabelMapToLabelImageFilter_h
#define itkLabelMapToLabelImageFilter_h

#include "itkLabelMapFilter.h"

namespace itk
{
/** \class LabelMapToLabelImageFilter
 * \brief Converts a LabelMap to a labeled image.
 *
 * LabelMapToBinaryImageFilter to a label image.
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
 *
 * \wiki
 * \wikiexample{ImageProcessing/LabelMapToLabelImageFilter,Convert a LabelMap to a normal image with different values representing each region}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT LabelMapToLabelImageFilter:
  public LabelMapFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef LabelMapToLabelImageFilter                  Self;
  typedef LabelMapFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Some convenient typedefs. */
  typedef typename Superclass::InputImageType          InputImageType;
  typedef typename Superclass::InputImagePointer       InputImagePointer;
  typedef typename Superclass::InputImageConstPointer  InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType    InputImageRegionType;
  typedef typename Superclass::InputImagePixelType     InputImagePixelType;
  typedef typename Superclass::LabelObjectType         LabelObjectType;

  typedef typename Superclass::OutputImageType         OutputImageType;
  typedef typename Superclass::OutputImagePointer      OutputImagePointer;
  typedef typename Superclass::OutputImageConstPointer OutputImageConstPointer;
  typedef typename Superclass::OutputImageRegionType   OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType    OutputImagePixelType;
  typedef typename OutputImageType::IndexType          IndexType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelMapToLabelImageFilter, LabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
#endif

protected:
  LabelMapToLabelImageFilter();
  ~LabelMapToLabelImageFilter() ITK_OVERRIDE {}

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void ThreadedProcessLabelObject(LabelObjectType *labelObject) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelMapToLabelImageFilter);
  OutputImageType *m_OutputImage;
};                                          // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelMapToLabelImageFilter.hxx"
#endif

#endif
