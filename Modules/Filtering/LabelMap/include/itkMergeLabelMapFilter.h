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
#ifndef itkMergeLabelMapFilter_h
#define itkMergeLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class MergeLabelMapFilter
 * \brief Merges several Label Maps
 *
 * This filter takes one or more input Label Map and merges them.
 *
 * SetMethod() can be used to change how the filter manage the
 * labels from the different label maps.
 *   KEEP (0): MergeLabelMapFilter do its best to keep the label unchanged,
 *             but if a label is already used in a previous label map, a new
 *             label is assigned.
 *   AGGREGATE (1): If the same label is found several times in the label maps,
 *                  the label objects with the same label are merged.
 *   PACK (2): MergeLabelMapFilter relabel all the label objects by order of
 *             processing. No conflict can occur.
 *   STRICT (3): MergeLabelMapFilter keeps the labels unchanged and raises an
 *               exception if the same label is found in several images.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT MergeLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef MergeLabelMapFilter             Self;
  typedef InPlaceLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;
  typedef typename LabelObjectType::Pointer   LabelObjectPointer;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MergeLabelMapFilter, InPlaceLabelMapFilter);

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

#ifdef STRICT
#undef STRICT
#endif
  typedef enum {
    KEEP = 0,
    AGGREGATE = 1,
    PACK = 2,
    STRICT = 3
    } MethodChoice;

  /** Set/Get the method used to merge the label maps */
  itkSetMacro(Method, MethodChoice);
  itkGetConstReferenceMacro(Method, MethodChoice);

protected:
  MergeLabelMapFilter();
  ~MergeLabelMapFilter() ITK_OVERRIDE {}

  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  MethodChoice m_Method;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MergeLabelMapFilter);

  void MergeWithKeep();

  void MergeWithAggregate();

  void MergeWithPack();

  void MergeWithStrict();
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMergeLabelMapFilter.hxx"
#endif

#endif
