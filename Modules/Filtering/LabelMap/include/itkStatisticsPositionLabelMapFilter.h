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
#ifndef itkStatisticsPositionLabelMapFilter_h
#define itkStatisticsPositionLabelMapFilter_h

#include "itkShapePositionLabelMapFilter.h"

namespace itk {
/** \class StatisticsPositionLabelMapFilter
 * \brief Mark a single pixel in the label object which correspond to a position given by an attribute
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://hdl.handle.net/1926/584
 * http://www.insight-journal.org/browse/publication/176
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa StatisticsLabelObject, BinaryStatisticsOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template<typename TImage>
class ITK_TEMPLATE_EXPORT StatisticsPositionLabelMapFilter :
    public ShapePositionLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef StatisticsPositionLabelMapFilter    Self;
  typedef ShapePositionLabelMapFilter<TImage> Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef typename LabelObjectType::AttributeType AttributeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StatisticsPositionLabelMapFilter,
               InPlaceLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputImagePixelType>));*/
  // End concept checking
#endif

protected:
  StatisticsPositionLabelMapFilter();
  ~StatisticsPositionLabelMapFilter() ITK_OVERRIDE {};

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(StatisticsPositionLabelMapFilter);

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsPositionLabelMapFilter.hxx"
#endif

#endif
