/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeUniqueLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributeUniqueLabelMapFilter_h
#define __itkAttributeUniqueLabelMapFilter_h

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
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TImage, class TAttributeAccessor=
    typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_EXPORT AttributeUniqueLabelMapFilter :
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
  /** Begin concept checking */
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputImagePixelType>));*/
  /** End concept checking */
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
  ~AttributeUniqueLabelMapFilter() {};

  void GenerateData();

  void PrintSelf(std::ostream& os, Indent indent) const;

  bool m_ReverseOrdering;

private:
  AttributeUniqueLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

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
#include "itkAttributeUniqueLabelMapFilter.txx"
#endif

#endif
