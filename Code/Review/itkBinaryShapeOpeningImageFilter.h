/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryShapeOpeningImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryShapeOpeningImageFilter_h
#define __itkBinaryShapeOpeningImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkShapeLabelObject.h"
#include "itkLabelMap.h"
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkShapeLabelMapFilter.h"
#include "itkShapeOpeningLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"

namespace itk
{
/** \class BinaryShapeOpeningImageFilter
 * \brief Remove objects based on the value of their shape attribute.
 *
 * The BinaryShapeOpeningImageFilter removes the objects in a binary image
 * with an attribute value smaller or greater than a threshold called Lambda.
 * The attributes are those of the ShapeLabelObject.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, LabelShapeOpeningImageFilter, BinaryStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TInputImage >
class ITK_EXPORT BinaryShapeOpeningImageFilter:
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef BinaryShapeOpeningImageFilter                  Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef TInputImage                            OutputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  typedef ShapeLabelObject< unsigned long, itkGetStaticConstMacro(ImageDimension) > LabelObjectType;
  typedef LabelMap< LabelObjectType >                                               LabelMapType;
  typedef BinaryImageToLabelMapFilter< InputImageType, LabelMapType >               LabelizerType;
  typedef Image< typename OutputImageType::PixelType, itkGetStaticConstMacro(OutputImageDimension) >
  ShapeLabelFilterOutput;
  typedef ShapeLabelMapFilter< LabelMapType, ShapeLabelFilterOutput >  LabelObjectValuatorType;
  typedef typename LabelObjectType::AttributeType                      AttributeType;
  typedef ShapeOpeningLabelMapFilter< LabelMapType >                   OpeningType;
  typedef LabelMapToBinaryImageFilter< LabelMapType, OutputImageType > BinarizerType;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryShapeOpeningImageFilter, ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.
   * Default is FullyConnectedOff.
   * For objects that are 1 pixel wide, use FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputImagePixelType > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputImagePixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputImagePixelType > ) );
  /** End concept checking */
#endif

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<PixelType>::max().
   */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /**
   * Set/Get the threshold used to keep or remove the objects.
   */
  itkGetConstMacro(Lambda, double);
  itkSetMacro(Lambda, double);

  /**
   * Set/Get the ordering of the objects. By default, the objects with
   * an attribute value smaller than Lamba are removed. Turning ReverseOrdering
   * to true make this filter remove objects with an attribute value greater
   * than Lambda instead.
   */
  itkGetConstMacro(ReverseOrdering, bool);
  itkSetMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use to select the object to remove.
   * Default is "Size".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void SetAttribute(const std::string & s)
  {
    this->SetAttribute( LabelObjectType::GetAttributeFromName(s) );
  }

protected:
  BinaryShapeOpeningImageFilter();
  ~BinaryShapeOpeningImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** BinaryShapeOpeningImageFilter needs the entire input to be available.
   * Thus, it needs to provide an implementation of GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** BinaryShapeOpeningImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) );

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData();

private:
  BinaryShapeOpeningImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                //purposely not implemented

  bool                 m_FullyConnected;
  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;
  double               m_Lambda;
  bool                 m_ReverseOrdering;
  AttributeType        m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryShapeOpeningImageFilter.txx"
#endif

#endif
