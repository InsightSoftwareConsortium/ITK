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
#ifndef itkBinaryImageToShapeLabelMapFilter_h
#define itkBinaryImageToShapeLabelMapFilter_h

#include "itkShapeLabelObject.h"
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkShapeLabelMapFilter.h"

namespace itk
{
/** \class BinaryImageToShapeLabelMapFilter
 * \brief Converts a binary image to a label map and valuate the shape attributes.
 *
 * A convenient class that converts a binary image to a label map and valuates the
 * shape attributes at once.
 *
 * The GetOutput() function returns an itk::ShapeLabelMap.
 * A typical use would be to iterate over the ShapeLabelObjects in the map,
 * using something like this:
 * \code
 * for(unsigned int i = 0; i < filter->GetOutput()->GetNumberOfLabelObjects(); ++i)
 *   {
 *   FilterType::OutputImageType::LabelObjectType* shapeLabelObject =
 *     filter->GetOutput()->GetLabelObject(i);
 *   // Here you can get properties of the ShapeLabelObject
 *   std::cout << "Bounding box: " << shapeLabelObject->GetBoundingBox();
 *   }
 * \endcode
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, LabelShapeOpeningImageFilter, BinaryStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 *
 * \wiki
 * \wikiexample{ImageProcessing/BinaryImageToShapeLabelMapFilter,Label binary regions in an image and get their properties}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage =
            LabelMap< ShapeLabelObject< SizeValueType, TInputImage::ImageDimension > > >
class ITK_TEMPLATE_EXPORT BinaryImageToShapeLabelMapFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BinaryImageToShapeLabelMapFilter                Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  typedef TOutputImage                              OutputImageType;
  typedef typename OutputImageType::Pointer         OutputImagePointer;
  typedef typename OutputImageType::ConstPointer    OutputImageConstPointer;
  typedef typename OutputImageType::RegionType      OutputImageRegionType;
  typedef typename OutputImageType::PixelType       OutputImagePixelType;
  typedef typename OutputImageType::LabelObjectType LabelObjectType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  typedef BinaryImageToLabelMapFilter< InputImageType, OutputImageType > LabelizerType;
  typedef Image< typename OutputImageType::PixelType, itkGetStaticConstMacro(OutputImageDimension) >
  ShapeLabelFilterOutput;
  typedef ShapeLabelMapFilter< TOutputImage, ShapeLabelFilterOutput > LabelObjectValuatorType;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryImageToShapeLabelMapFilter, ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by face connectivity or
   * by face+edge+vertex connectivity.  Default is FullyConnectedOff.  For objects that
   * are 1 pixel wide, use FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputImagePixelType > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputImagePixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputImagePixelType > ) );
  // End concept checking
#endif

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(OutputBackgroundValue, OutputImagePixelType);
  itkGetConstMacro(OutputBackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<PixelType>::max().
   */
  itkSetMacro(InputForegroundValue, InputImagePixelType);
  itkGetConstMacro(InputForegroundValue, InputImagePixelType);

  /**
   * Set/Get whether the maximum Feret diameter should be computed or not.
   * Default value is false, because of the high computation time required.
   */
  itkSetMacro(ComputeFeretDiameter, bool);
  itkGetConstReferenceMacro(ComputeFeretDiameter, bool);
  itkBooleanMacro(ComputeFeretDiameter);

  /**
   * Set/Get whether the perimeter should be computed or not.
   * Default value is false, because of the high computation time required.
   */
  itkSetMacro(ComputePerimeter, bool);
  itkGetConstReferenceMacro(ComputePerimeter, bool);
  itkBooleanMacro(ComputePerimeter);

  /**
   * Set/Get whether the oriented bounding box should be
   * computed or not. Default value is false because of potential
   * memory consumption issues with sparse labels.
   */
  itkSetMacro(ComputeOrientedBoundingBox, bool);
  itkGetConstReferenceMacro(ComputeOrientedBoundingBox, bool);
  itkBooleanMacro(ComputeOrientedBoundingBox);

protected:
  BinaryImageToShapeLabelMapFilter();
  ~BinaryImageToShapeLabelMapFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** BinaryImageToShapeLabelMapFilter needs the entire input be available.
   * Thus, it needs to provide an implementation of GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** BinaryImageToShapeLabelMapFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  /** Single-threaded version of GenerateData.
   * This filter delegates to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToShapeLabelMapFilter);

  bool                 m_FullyConnected;
  OutputImagePixelType m_OutputBackgroundValue;
  InputImagePixelType  m_InputForegroundValue;
  bool                 m_ComputeFeretDiameter;
  bool                 m_ComputePerimeter;
  bool                 m_ComputeOrientedBoundingBox;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryImageToShapeLabelMapFilter.hxx"
#endif

#endif
