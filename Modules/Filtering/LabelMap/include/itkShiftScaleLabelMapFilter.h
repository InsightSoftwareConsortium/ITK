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
#ifndef itkShiftScaleLabelMapFilter_h
#define itkShiftScaleLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class ShiftScaleLabelMapFilter
 * \brief Shifts and scales a label map filter, giving the option to change the background value.
 *
 * This filter takes as input a label map and shift, scale and background values
 * to produce, as output, a rescaled and shifted label map with, when applicable, a
 * new background.
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
class ITK_TEMPLATE_EXPORT ShiftScaleLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ShiftScaleLabelMapFilter        Self;
  typedef InPlaceLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                                        ImageType;
  typedef typename ImageType::Pointer                   ImagePointer;
  typedef typename ImageType::ConstPointer              ImageConstPointer;
  typedef typename ImageType::PixelType                 PixelType;
  typedef typename ImageType::IndexType                 IndexType;
  typedef typename ImageType::LabelObjectType           LabelObjectType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShiftScaleLabelMapFilter, InPlaceLabelMapFilter);

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

  itkSetMacro(Shift, double);
  itkGetConstReferenceMacro(Shift, double);

  itkSetMacro(Scale, double);
  itkGetConstReferenceMacro(Scale, double);

  itkSetMacro(ChangeBackgroundValue, bool);
  itkGetConstMacro(ChangeBackgroundValue, bool);
  itkBooleanMacro(ChangeBackgroundValue);

protected:
  ShiftScaleLabelMapFilter();
  ~ShiftScaleLabelMapFilter() ITK_OVERRIDE {}

  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  double m_Shift;
  double m_Scale;

  bool m_ChangeBackgroundValue;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShiftScaleLabelMapFilter);
};                                        // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShiftScaleLabelMapFilter.hxx"
#endif

#endif
