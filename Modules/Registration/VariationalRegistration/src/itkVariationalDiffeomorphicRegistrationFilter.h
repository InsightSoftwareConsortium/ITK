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
#ifndef __itkVariationalDiffeomorphicRegistrationFilter_h
#define __itkVariationalDiffeomorphicRegistrationFilter_h

#include "itkVariationalRegistrationFilter.h"
#include "itkExponentialDisplacementFieldImageFilter.h"


namespace itk
{

/** \class itk::VariationalDiffeomorphicRegistrationFilter
 *
 *  \sa VariationalRegistrationFilter
 *
 *  \ingroup VariationalRegistration
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalDiffeomorphicRegistrationFilter
  : public VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs */
  typedef VariationalDiffeomorphicRegistrationFilter                                   Self;
  typedef VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                           Pointer;
  typedef SmartPointer<const Self>                                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(itkVariationalDiffeomorphicRegistrationFilter, VariationalRegistrationFilter);

  /** Get image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** FixedImage image type. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::Pointer      FixedImagePointer;
  typedef typename FixedImageType::ConstPointer FixedImageConstPointer;

  /** MovingImage image type. */
  typedef TMovingImage                           MovingImageType;
  typedef typename MovingImageType::Pointer      MovingImagePointer;
  typedef typename MovingImageType::ConstPointer MovingImageConstPointer;

  /** Deformation field type. */
  typedef TDisplacementField                      DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer DisplacementFieldPointer;

  /** Types inherited from the superclass */
  typedef typename Superclass::OutputImageType OutputImageType;

  /** VariationalRegistrationFunction type. */
  typedef typename Superclass::RegistrationFunctionType RegistrationFunctionType;

  /** Regularizer type. */
  typedef typename Superclass::RegularizerType RegularizerType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Set initial deformation field. */
  virtual void
  SetInitialDisplacementField(DisplacementFieldType * ptr);

  /** Get output deformation field. */
  virtual DisplacementFieldType *
  GetDisplacementField()
  {
    return m_DisplacementField;
  }

  /** Set initial deformation field. */
  virtual void
  SetInitialVelocityField(DisplacementFieldType * ptr)
  {
    this->SetInput(ptr);
  }

  /** Get output velocity field. */
  virtual DisplacementFieldType *
  GetVelocityField()
  {
    return this->GetOutput();
  }

protected:
  VariationalDiffeomorphicRegistrationFilter();
  ~VariationalDiffeomorphicRegistrationFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** This method is called before iterating the solution. */
  virtual void
  Initialize();

  /** Apply update. */
  virtual void
  ApplyUpdate(const TimeStepType & dt);

  /** Calculates the deformation field by calculating the exponential
   * of the velocity field. */
  virtual void
  CalcDeformationFromVelocityField(const DisplacementFieldType * velocityField);

  /** Exponential field calculator type. */
  typedef itk::ExponentialDisplacementFieldImageFilter<DisplacementFieldType, DisplacementFieldType>
    FieldExponentiatorType;

  typedef typename FieldExponentiatorType::Pointer FieldExponentiatorPointer;

  virtual FieldExponentiatorPointer
  GetExponentiator()
  {
    return m_Exponentiator;
  }

private:
  VariationalDiffeomorphicRegistrationFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** The deformation field. */
  FieldExponentiatorPointer m_Exponentiator;
  DisplacementFieldPointer  m_DisplacementField;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalDiffeomorphicRegistrationFilter.hxx"
#endif

#endif
