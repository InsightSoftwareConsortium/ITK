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
#ifndef __itkVariationalRegistrationRegularizer_h
#define __itkVariationalRegistrationRegularizer_h

#include "itkInPlaceImageFilter.h"
#include "itkMultiThreader.h"

namespace itk
{

/** \class itk::VariationalRegistrationRegularizer
 *
 *  \brief Base class for regularization component in the variational registration framework.
 *
 *  This class is templated over the deformation field type. The input of this filter is a
 *  vector field \f$ u \f$ and the output is the regularized (i.e. smoothed) vector field.
 *
 *  Implement a concrete regularization method in a subclass; overwrite the methods
 *  Initialize() and GenerateData().
 *
 *  \sa VariationalRegistrationFilter
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <class TDisplacementField>
class VariationalRegistrationRegularizer : public InPlaceImageFilter<TDisplacementField, TDisplacementField>
{
public:
  /** Standard class typedefs */
  typedef VariationalRegistrationRegularizer                         Self;
  typedef InPlaceImageFilter<TDisplacementField, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                         Pointer;
  typedef SmartPointer<const Self>                                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(itkVariationalRegistrationRegularizer, InPlaceImageFilter);

  itkStaticConstMacro(ImageDimension, unsigned int, TDisplacementField::ImageDimension);

  /** Deformation field type. */
  typedef TDisplacementField                           DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer      DisplacementFieldPointer;
  typedef typename DisplacementFieldType::ConstPointer DisplacementFieldConstPointer;
  typedef typename DisplacementFieldType::PixelType    PixelType;

  typedef typename NumericTraits<PixelType>::ValueType ValueType;

  /** Set whether the image spacing should be considered or not */
  itkSetMacro(UseImageSpacing, bool);

  /** Get whether the image spacing is considered or not */
  itkGetConstMacro(UseImageSpacing, bool);

  /** Set whether the image spacing should be considered or not */
  itkBooleanMacro(UseImageSpacing);

protected:
  VariationalRegistrationRegularizer();
  ~VariationalRegistrationRegularizer() {}

  /** Print information about the filter. */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the filter. */
  virtual void
  Initialize() {};

private:
  VariationalRegistrationRegularizer(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** A boolean that indicates, if image spacing is considered. */
  bool m_UseImageSpacing;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationRegularizer.hxx"
#endif

#endif
