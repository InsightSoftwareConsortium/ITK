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
#ifndef itkRelabelLabelMapFilter_h
#define itkRelabelLabelMapFilter_h

#include "itkAttributeRelabelLabelMapFilter.h"
#include "itkLabelObjectAccessors.h"

namespace itk
{
/** \class RelabelLabelMapFilter
 * \brief This filter relabels the LabelObjects; the new labels are arranged
 * consecutively with consideration for the background value.
 *
 * This filter takes the LabelObjects from the input and reassigns them to the
 * output by calling the PushLabelObject method, which by default, attempts to
 * reorganize the labels consecutively. The user can assign an arbitrary value
 * to the background; the filter will assign the labels consecutively by
 * skipping the background value.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TImage >
class RelabelLabelMapFilter:
  public AttributeRelabelLabelMapFilter< TImage, typename Functor::LabelLabelObjectAccessor<typename TImage::LabelObjectType> >
{
public:
  /** Standard class typedefs. */
  typedef RelabelLabelMapFilter           Self;
  typedef AttributeRelabelLabelMapFilter< TImage,
     typename Functor::LabelLabelObjectAccessor<typename TImage::LabelObjectType> >
                                          Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                                        ImageType;
  typedef typename ImageType::Pointer                   ImagePointer;
  typedef typename ImageType::ConstPointer              ImageConstPointer;
  typedef typename ImageType::PixelType                 PixelType;
  typedef typename ImageType::IndexType                 IndexType;
  typedef typename ImageType::LabelObjectType           LabelObjectType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RelabelLabelMapFilter, AttributeRelabelLabelMapFilter);

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
  RelabelLabelMapFilter()
    {
    this->SetReverseOrdering( true );
    }
  ~RelabelLabelMapFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RelabelLabelMapFilter);
};                                     // end of class
} // end namespace itk


#endif
