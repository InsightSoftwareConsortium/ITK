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
#ifndef __itkVariationalRegistrationLogger_h
#define __itkVariationalRegistrationLogger_h

#include "itkCommand.h"
#include "itkEventObject.h"

namespace itk
{

/** \class itk::VariationalRegistrationLogger
 * \brief A simple callback/observer class to print information during the registration process.
 *
 * VariationalRegistrationLogger is an implementation of the Command design pattern that is
 * invoked every iteration of the registration process. Use AddObserver() to connect the logger
 * with VariationalRegistrationFilter and/or VariationalRegistrationMultiResolutionFilter.
 * VariationalRegistrationLogger prints levels or metric values on IterationEvent or InitializeEvent.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationMultiResolutionFilter
 *
 *  \ingroup VariationalRegistration
 */
template <class TRegistrationFilter, class TMRFilter>
class ITK_EXPORT VariationalRegistrationLogger : public Command
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationLogger Self;
  typedef Command                       Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Registration and MR filter types */
  typedef TRegistrationFilter RegistrationFilterType;
  typedef TMRFilter           MRFilterType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** print iterations, levels or metric values on IterationEvent or InitializeEvent */
  void
  Execute(itk::Object * caller, const itk::EventObject & event)
  {
    Execute((const itk::Object *)caller, event);
  }

  /** print iterations, levels or metric values on IterationEvent or InitializeEvent */
  void
  Execute(const itk::Object * caller, const itk::EventObject & event);

protected:
  VariationalRegistrationLogger();
  ~VariationalRegistrationLogger();

  void
  PrintSelf(std::ostream & os, Indent indent) const;

private:
  VariationalRegistrationLogger(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationLogger.hxx"
#endif

#endif
