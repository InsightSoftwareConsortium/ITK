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
#ifndef itkAttributeSelectionLabelMapFilter_h
#define itkAttributeSelectionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"
#include <set>


namespace itk {
/** \class AttributeSelectionLabelMapFilter
 * \brief remove the objects according to the value of their attribute
 *
 * AttributeSelectionLabelMapFilter removes the objects in a label collection image
 * with an attribute value inside or outside a set of attribute values passed by
 * the user.
 * The attribute is provided by an attribute accessor given in template parameter.
 * Contrary to the other filters made to remove some object of a LabelMap, no
 * ordering relation for the attribute is needed in that filter.
 * The filter provides two outputs: the first contains the surviving objects,
 * the second the removed objects (the input LabelMap is the union of these two LabelMaps).
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa AttributeLabelObject
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template<typename TImage, typename TAttributeAccessor=
    typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_TEMPLATE_EXPORT AttributeSelectionLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef AttributeSelectionLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage>    Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef TAttributeAccessor                                 AttributeAccessorType;
  typedef typename AttributeAccessorType::AttributeValueType AttributeValueType;

  typedef typename std::set<AttributeValueType> AttributeSetType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeSelectionLabelMapFilter,
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

  /**
   * Set/Get the threshold used to keep or remove the objects.
   */
  const AttributeSetType & GetAttributeSet() const
    {
    return m_AttributeSet;
    }
  void SetAttributeSet( const AttributeSetType & set )
    {
    m_AttributeSet = set;
    this->Modified();
    }

  /**
   * Set/Get whether the objects with the specified attribute values should be kept
   * or excluded.
   */
  itkGetConstMacro( Exclude, bool );
  itkSetMacro( Exclude, bool );
  itkBooleanMacro( Exclude );

  /** Clear the attribute set, and add the attribute passed in parameter */
  void SetAttribute( const AttributeValueType & attr )
    {
    this->ClearAttributeSet();
    this->AddAttribute( attr );
    }

  void ClearAttributeSet()
    {
    if( ! m_AttributeSet.empty() )
      {
      m_AttributeSet.clear();
      this->Modified();
      }
    }

  void AddAttribute(  const AttributeValueType & attr )
    {
    const typename AttributeSetType::size_type size = m_AttributeSet.size();
    m_AttributeSet.insert( attr );
    if( size != m_AttributeSet.size() )
      {
      this->Modified();
      }
    }

protected:
  AttributeSelectionLabelMapFilter();
  ~AttributeSelectionLabelMapFilter() ITK_OVERRIDE {};

  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AttributeSelectionLabelMapFilter);

  AttributeSetType m_AttributeSet;
  bool             m_Exclude;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAttributeSelectionLabelMapFilter.hxx"
#endif

#endif
