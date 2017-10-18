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
#ifndef itkAttributeUniqueLabelMapFilter_h
#define itkAttributeUniqueLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk {
/** \class AttributeUniqueLabelMapFilter
 * \brief Make sure that the objects are not overlapping
 *
 * AttributeUniqueLabelMapFilter search the overlapping zones in the overlapping
 * objects and keeps only a single object on all the pixels of the image.
 * The object to keep is selected according to the value of the attribute
 * provided by the accessor. If the attribute values are equal for a set of
 * overlapping objects, the label is used to keep the priority consistent in the
 * whole image.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template<typename TImage, typename TAttributeAccessor=
    typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_TEMPLATE_EXPORT AttributeUniqueLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef AttributeUniqueLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage> Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef TAttributeAccessor                                 AttributeAccessorType;
  typedef typename AttributeAccessorType::AttributeValueType AttributeValueType;

  typedef typename LabelObjectType::LineType  LineType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeUniqueLabelMapFilter,
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
   * Set/Get the order of labeling of the objects. By default, the objects with
   * the highest attribute values are labeled first. Set ReverseOrdering to true
   * make the one with the smallest attributes be labeled first.
   */
  itkSetMacro( ReverseOrdering, bool );
  itkGetConstReferenceMacro( ReverseOrdering, bool );
  itkBooleanMacro( ReverseOrdering );

protected:
  AttributeUniqueLabelMapFilter();
  ~AttributeUniqueLabelMapFilter() ITK_OVERRIDE {};

  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  bool m_ReverseOrdering;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AttributeUniqueLabelMapFilter);

  struct LineOfLabelObject
    {
    typedef typename LabelObjectType::LineType LineType;

    LineOfLabelObject( const LineType l, LabelObjectType * lo )
      {
      this->line = l;
      this->labelObject = lo;
      }
    LineType          line;
    LabelObjectType * labelObject;
    };

  class LineOfLabelObjectComparator
    {
    public:
      bool operator()( const LineOfLabelObject & lla, const LineOfLabelObject & llb )
        {
        for( int i=ImageDimension-1; i>=0; i-- )
          {
          if( lla.line.GetIndex()[i] > llb.line.GetIndex()[i] )
            {
            return true;
            }
          else if( lla.line.GetIndex()[i] < llb.line.GetIndex()[i] )
            {
            return false;
            }
          }
        return false;
        }
    };

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAttributeUniqueLabelMapFilter.hxx"
#endif

#endif
