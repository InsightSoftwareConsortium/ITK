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
#ifndef itkMaxPhaseCorrelationOptimizer_h
#define itkMaxPhaseCorrelationOptimizer_h

#include "itkPhaseCorrelationOptimizer.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{

/** \class MaxPhaseCorrelationOptimizer
 *  \brief Implements basic shift estimation from position of maximum peak.
 *
 *  This class is templated over the type of registration method in which it has
 *  to be plugged in.
 *
 *  Operates on real correlation surface, so when set to the registration method,
 *  it should be get back by
 *  PhaseCorrelationImageRegistrationMethod::GetRealOptimizer() method.
 *
 *  The optimizer finds the maximum peak by MinimumMaximumImageCalculator and
 *  estimates the shift with pixel-level precision.
 *
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template < typename TRegistrationMethod >
class ITK_TEMPLATE_EXPORT MaxPhaseCorrelationOptimizer :
  public PhaseCorrelationOptimizer<typename TRegistrationMethod::RealImageType>
{
public:
  typedef MaxPhaseCorrelationOptimizer                                            Self;
  typedef PhaseCorrelationOptimizer< typename TRegistrationMethod::RealImageType> Superclass;
  typedef SmartPointer<Self>                                                      Pointer;
  typedef SmartPointer<const Self>                                                ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaxPhaseCorrelationOptimizer, PhaseCorrelationOptimizer);

  /**  Type of the input image. */
  typedef typename TRegistrationMethod::RealImageType   ImageType;
  typedef typename ImageType::ConstPointer              ImageConstPointer;

  /** Dimensionality of input and output data. */
  itkStaticConstMacro( ImageDimension, unsigned int, ImageType::ImageDimension );

  /** Type for the output parameters.
  *  It defines a position in the optimization search space. */
  typedef typename Superclass::OffsetType               OffsetType;
  typedef typename Superclass::OffsetScalarType         OffsetScalarType;

protected:
  MaxPhaseCorrelationOptimizer();
  virtual ~MaxPhaseCorrelationOptimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const override;

  /** This method is executed by superclass to execute the computation. */
  void ComputeOffset() override;

  typedef MinimumMaximumImageCalculator< ImageType >    MaxCalculatorType;

private:
  MaxPhaseCorrelationOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename MaxCalculatorType::Pointer                   m_MaxCalculator;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaxPhaseCorrelationOptimizer.hxx"
#endif

#endif
