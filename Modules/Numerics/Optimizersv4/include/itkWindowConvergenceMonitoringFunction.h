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
#ifndef itkWindowConvergenceMonitoringFunction_h
#define itkWindowConvergenceMonitoringFunction_h

#include "itkConvergenceMonitoringFunction.h"

namespace itk
{
namespace Function
{
/**
 * \class WindowConvergenceMonitoringFunction
 * \brief Class which monitors convergence during the course of optimization.
 *
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKOptimizersv4
 */

template<typename TScalar = double>
class ITK_TEMPLATE_EXPORT WindowConvergenceMonitoringFunction
: public ConvergenceMonitoringFunction<TScalar, TScalar>
{
public:

  typedef WindowConvergenceMonitoringFunction                     Self;
  typedef ConvergenceMonitoringFunction<TScalar, TScalar>         Superclass;
  typedef SmartPointer<Self>                                      Pointer;
  typedef SmartPointer<const Self>                                ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( WindowConvergenceMonitoringFunction, ConvergenceMonitoringFunction );

  typedef TScalar                                            ScalarType;
  typedef typename NumericTraits<ScalarType>::RealType       RealType;

  typedef typename Superclass::EnergyValueType               EnergyValueType;
  typedef typename Superclass::EnergyValueContainerType      EnergyValueContainerType;
  typedef typename Superclass::EnergyValueContainerSizeType  EnergyValueContainerSizeType;
  typedef typename EnergyValueContainerType::iterator        EnergyValueIterator;
  typedef typename EnergyValueContainerType::const_iterator  EnergyValueConstIterator;

  /** Add energy value */
  virtual void AddEnergyValue( const EnergyValueType ) ITK_OVERRIDE;

  /* Clear energy values and set total energy to 0 */
  virtual void ClearEnergyValues() ITK_OVERRIDE;

  /** Set/Get window size over which the convergence value is calculated */
  itkSetMacro( WindowSize, EnergyValueContainerSizeType );
  itkGetConstMacro( WindowSize, EnergyValueContainerSizeType );

  /** Calculate convergence value by fitting to a window of the enrgy profile */
  virtual RealType GetConvergenceValue() const ITK_OVERRIDE;

protected:
  WindowConvergenceMonitoringFunction();

  ~WindowConvergenceMonitoringFunction() ITK_OVERRIDE;

  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WindowConvergenceMonitoringFunction);

  EnergyValueContainerSizeType                   m_WindowSize;

  RealType                                       m_TotalEnergy;

};
} // end namespace function
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWindowConvergenceMonitoringFunction.hxx"
#endif

#endif
