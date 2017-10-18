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
#ifndef itkBinaryReconstructionLabelMapFilter_h
#define itkBinaryReconstructionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk {
/** \class BinaryReconstructionLabelMapFilter
 * \brief Mark the objects at least partially at the same position as the objects in a binary image
 *
 * The attribute is accessed through the accessor given with TAttributeAccessor.
 * The LabelObjects from the input LabelMap are marked with "true" if at least one of their pixel is
 * at the same position than an object in the binary marker image.
 * In the marker image, the pixels with a value equal to ForegroundValue are considered to be in the
 * objects.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template<typename TImage, typename TMarkerImage, typename TAttributeAccessor=
  typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_TEMPLATE_EXPORT BinaryReconstructionLabelMapFilter :
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
  // Begin concept checking
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<PixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, PixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<PixelType>));*/
  // End concept checking
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
  itkSetMacro(ForegroundValue, MarkerImagePixelType);
  itkGetConstMacro(ForegroundValue, MarkerImagePixelType);

protected:
  BinaryReconstructionLabelMapFilter();
  ~BinaryReconstructionLabelMapFilter() ITK_OVERRIDE {};

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject ) ITK_OVERRIDE;

  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryReconstructionLabelMapFilter);

  MarkerImagePixelType m_ForegroundValue;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryReconstructionLabelMapFilter.hxx"
#endif

#endif
