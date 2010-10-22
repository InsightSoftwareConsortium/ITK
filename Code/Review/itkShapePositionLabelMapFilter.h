/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkShapePositionLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePositionLabelMapFilter_h
#define __itkShapePositionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk {
/** \class ShapePositionLabelMapFilter
 * \brief Mark a single pixel in the label object which correspond to a position given by an attribute
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, BinaryShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TImage>
class ITK_EXPORT ShapePositionLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ShapePositionLabelMapFilter    Self;
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

  typedef typename LabelObjectType::AttributeType AttributeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShapePositionLabelMapFilter,
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
   * Set/Get the attribute to use to get the object position. The default
   * is "Centroid".
   */
  itkGetConstMacro( Attribute, AttributeType );
  itkSetMacro( Attribute, AttributeType );
  void SetAttribute( const std::string & s )
    {
    this->SetAttribute( LabelObjectType::GetAttributeFromName( s ) );
    }


protected:
  ShapePositionLabelMapFilter();
  ~ShapePositionLabelMapFilter() {};

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject );

  template< class TAttributeAccessor >
  void TemplatedThreadedProcessLabelObject( const TAttributeAccessor & accessor, bool physical, LabelObjectType * labelObject )
  {
    typedef typename TAttributeAccessor::AttributeValueType AttributeValueType;
    AttributeValueType position = accessor( labelObject );
    // change it to an index position if it is physical
    IndexType idx;
    if( physical )
      {
      Point< double, ImageDimension > point;
      // copy the position to a point, required by TransformPhysicalPointToIndex
      for( int i=0; i<ImageDimension; i++ )
        {
        point[i] = (long)position[i];
        }
      this->GetOutput()->TransformPhysicalPointToIndex( point, idx );
      }
    else
      {
      // copy the position to the index, to avoid warnings
      for( int i=0; i<ImageDimension; i++ )
        {
        idx[i] = (long)position[i];
        }
      }
    // clear the label object
    labelObject->GetLineContainer().clear();
    // and mark only the pixel we are interested in
    labelObject->AddIndex( idx );
  }

  void PrintSelf(std::ostream& os, Indent indent) const;

  AttributeType m_Attribute;

private:
  ShapePositionLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapePositionLabelMapFilter.txx"
#endif

#endif
