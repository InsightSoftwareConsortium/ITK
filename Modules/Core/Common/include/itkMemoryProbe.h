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
#ifndef itkMemoryProbe_h
#define itkMemoryProbe_h

#include "itkResourceProbe.h"
#include "itkMemoryUsageObserver.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class MemoryProbe
 *
 *  \brief Computes the memory allocated between two points in code.
 *
 *   This class allows the user to trace the memory charge between the execution
 *   of two pieces of code. It can be started and stopped in order to evaluate
 *   the execution over multiple passes. The values of memory are taken from
 *   GetProcessMemoryInfo() for Windows, the SMAPS file for Linux
 *   and getrusage() otherwise.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MemoryProbe:
  public ResourceProbe< SizeValueType, double >
{
public:

  MemoryProbe();
  ~MemoryProbe() ITK_OVERRIDE;

  /** Type for measuring memory. */
  typedef SizeValueType MemoryLoadType;

  /** Type for measuring the average memory. */
  typedef double MeanMemoryLoadType;

protected:
  virtual MemoryLoadType GetInstantValue(void) const ITK_OVERRIDE;

private:
  mutable MemoryUsageObserver m_MemoryObserver;
};
} // end namespace itk

#endif //itkMemoryProbe_h
