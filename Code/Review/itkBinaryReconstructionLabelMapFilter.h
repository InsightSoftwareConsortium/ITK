/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBinaryReconstructionLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryReconstructionLabelMapFilter_h
#define __itkBinaryReconstructionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk {
/** \class BinaryReconstructionLabelMapFilter
 * \brief TODO
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TImage, class TMarkerImage, class TAttributeAccessor=
  typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_EXPORT BinaryReconstructionLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef BinaryReconstructionLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage>      Superclass;
  typedef SmartPointer<Self>                 Pointer;
  typedef SmartPointer<const Self>           ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef TMarkerImage                              MarkerImageType;
  typedef typename MarkerImageType::Pointer         MarkerImagePointer;
  typedef typename MarkerImageType::ConstPointer    MarkerImageConstPointer;
  typedef typename MarkerImageType::PixelType       MarkerImagePixelType;

  typedef TAttributeAccessor AttributeAccessorType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryReconstructionLabelMapFilter,
               InPlaceLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<PixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, PixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<PixelType>));*/
  /** End concept checking */
#endif

   /** Set the marker image */
  void SetMarkerImage(TMarkerImage *input)
    {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast<TMarkerImage *>(input) );
    }

  /** Get the marker image */
  MarkerImageType * GetMarkerImage()
    {
    return static_cast<MarkerImageType*>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
    }

   /** Set the input image */
  void SetInput1(TImage *input)
    {
    this->SetInput( input );
    }

  /** Set the marker image */
  void SetInput2(TMarkerImage *input)
    {
    this->SetMarkerImage( input );
    }

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<MaskPixelType>::max().
   */
  itkSetMacro(ForegroundValue, PixelType);
  itkGetConstMacro(ForegroundValue, PixelType);

protected:
  BinaryReconstructionLabelMapFilter();
  ~BinaryReconstructionLabelMapFilter() {};

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject );

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BinaryReconstructionLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  MarkerImagePixelType m_ForegroundValue;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryReconstructionLabelMapFilter.txx"
#endif

#endif
