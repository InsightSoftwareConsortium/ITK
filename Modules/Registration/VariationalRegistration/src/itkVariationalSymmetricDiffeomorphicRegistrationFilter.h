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
#ifndef __itkVariationalSymmetricDiffeomorphicRegistrationFilter_h
#define __itkVariationalSymmetricDiffeomorphicRegistrationFilter_h

#include "itkVariationalDiffeomorphicRegistrationFilter.h"

namespace itk
{

/** \class itk::VariationalSymmetricDiffeomorphicRegistrationFilter
 *
 *  \sa VariationalDiffeomorphicRegistrationFilter
 *
 *  \ingroup VariationalRegistration
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
class ITK_EXPORT VariationalSymmetricDiffeomorphicRegistrationFilter
  : public VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>
{
public:
  /** Standard class typedefs */
  typedef VariationalSymmetricDiffeomorphicRegistrationFilter                                       Self;
  typedef VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField> Superclass;
  typedef SmartPointer<Self>                                                                        Pointer;
  typedef SmartPointer<const Self>                                                                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(itkVariationalSymmetricDiffeomorphicRegistrationFilter, VariationalDiffeomorphicRegistrationFilter);

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
  typedef OutputImageType                      UpdateBufferType;

  /** VariationalRegistrationFunction type. */
  typedef typename Superclass::RegistrationFunctionType RegistrationFunctionType;

  /** Regularizer type. */
  typedef typename Superclass::RegularizerType RegularizerType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Get output inverse deformation field. */
  virtual DisplacementFieldType *
  GetInverseDisplacementField()
  {
    return m_InverseDisplacementField;
  }

protected:
  VariationalSymmetricDiffeomorphicRegistrationFilter();
  ~VariationalSymmetricDiffeomorphicRegistrationFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** This method is called before iterating the solution. */
  virtual void
  Initialize();

  /** TODO */
  virtual void
  InitializeBackwardIteration();

  /** TODO */
  virtual void
  ApplyUpdate(const TimeStepType & dt);

  /** TODO */
  virtual TimeStepType
  CalculateChange();

  /** Calculates the inverse deformation field by calculating the exponential
   * of the negative velocity field. */
  virtual void
  CalcInverseDeformationFromVelocityField(const DisplacementFieldType * velocityField);

  /** Method to allow subclasses to get direct access to the update
   * buffer */
  virtual UpdateBufferType *
  GetBackwardUpdateBuffer()
  {
    return m_BackwardUpdateBuffer;
  }

  /** The type of region used for multithreading */
  typedef typename UpdateBufferType::RegionType ThreadRegionType;

  /**  TODO */
  virtual void
  ThreadedApplyUpdate(const TimeStepType & dt, const ThreadRegionType & regionToProcess, unsigned int threadId);

private:
  VariationalSymmetricDiffeomorphicRegistrationFilter(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  typedef typename Superclass::FieldExponentiatorType FieldExponentiatorType;
  typedef typename FieldExponentiatorType::Pointer    FieldExponentiatorPointer;

  /** The deformation field. */
  FieldExponentiatorPointer          m_InverseExponentiator;
  DisplacementFieldPointer           m_InverseDisplacementField;
  typename UpdateBufferType::Pointer m_BackwardUpdateBuffer;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalSymmetricDiffeomorphicRegistrationFilter.hxx"
#endif

#endif
