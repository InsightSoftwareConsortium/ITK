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
#ifndef itkLabelSelectionLabelMapFilter_h
#define itkLabelSelectionLabelMapFilter_h

#include "itkAttributeSelectionLabelMapFilter.h"
#include "itkLabelObject.h"
#include "itkLabelObjectAccessors.h"
#include <set>


namespace itk {
/** \class LabelSelectionLabelMapFilter
 * \brief remove the objects according to the value of their attribute
 *
 * LabelSelectionLabelMapFilter removes the objects in a label collection image
 * with an attribute value inside or outside a set of attribute values passed by
 * the user.
 * The attribute is provide by an attribute accessor given in template parameter.
 * Contrary to the other filters made to remove some object of a LabelMap, no
 * ordering relation for the attribute is needed in that filter.
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
 * \sa AttributeLabelObject
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template<typename TImage>
class LabelSelectionLabelMapFilter :
    public AttributeSelectionLabelMapFilter<TImage, typename Functor::LabelLabelObjectAccessor< typename TImage::LabelObjectType > >
{
public:
  /** Standard class typedefs. */
  typedef LabelSelectionLabelMapFilter Self;
  typedef AttributeSelectionLabelMapFilter<TImage, typename Functor::LabelLabelObjectAccessor< typename TImage::LabelObjectType > >
                                       Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;

  typedef typename Superclass::AttributeAccessorType AttributeAccessorType;
  typedef typename Superclass::AttributeValueType    AttributeValueType;

  typedef typename Superclass::AttributeSetType AttributeSetType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelSelectionLabelMapFilter,
               AttributeSelectionLabelMapFilter);

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

  const AttributeSetType & GetLabelSet() const
    {
    return this->GetAttributeSet();
    }

  void SetLabelSet( const AttributeSetType & set )
    {
    this->SetAttributeSet( set );
    }

  /** Clear the attribute set, and add the attribute passed in parameter */
  void SetLabel( const AttributeValueType & attr )
    {
    this->SetAttribute( attr );
    }

  void ClearLabelSet()
    {
    this->ClearAttributeSet();
    }

  void AddLabel( const AttributeValueType & attr )
    {
    this->AddAttribute( attr );
    }

protected:
  LabelSelectionLabelMapFilter() {};
  ~LabelSelectionLabelMapFilter() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelSelectionLabelMapFilter);

}; // end of class

} // end namespace itk

#endif
