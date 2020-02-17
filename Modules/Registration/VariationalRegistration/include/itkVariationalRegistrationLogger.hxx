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
#ifndef itkVariationalRegistrationLogger_hxx
#define itkVariationalRegistrationLogger_hxx

#include "itkMath.h"
#include "itkVariationalRegistrationLogger.h"

#include "itkVariationalRegistrationFilter.h"
#include "itkVariationalRegistrationMultiResolutionFilter.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TRegistrationFilter, typename TMRFilter>
VariationalRegistrationLogger<TRegistrationFilter, TMRFilter>::VariationalRegistrationLogger() = default;

/**
 * Default destructor
 */
template <typename TRegistrationFilter, typename TMRFilter>
VariationalRegistrationLogger<TRegistrationFilter, TMRFilter>::~VariationalRegistrationLogger() = default;

/**
 *
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationLogger<TRegistrationFilter, TMRFilter>::Execute(const itk::Object *      caller,
                                                                       const itk::EventObject & event)
{
  // If event is an iteration event, check if thrown by registration
  // or multi resolution filter
  if (itk::IterationEvent().CheckEvent(&event))
  {
    // Cast caller for subsequent check
    const auto * regFilter = dynamic_cast<const RegistrationFilterType *>(caller);

    const auto * mrFilter = dynamic_cast<const MRFilterType *>(caller);

    // If caller is MR filter, set mode for next level according to
    // MR policy
    if (mrFilter)
    {
      std::cout << "Finished level " << mrFilter->GetElapsedLevels() << std::endl;
    }

    // If caller is registration filter, log metric of last iteration
    // and check, if stop criterion is fulfilled
    else if (regFilter)
    {
      std::cout << "  " << regFilter->GetElapsedIterations() << " - Metric: " << regFilter->GetMetric()
                << " - RMS-Change: " << regFilter->GetRMSChange() << std::endl;
    }
  }

  // If initialize event called by MR filter, set MR mode for first
  // level according to MR policy
  else if (itk::InitializeEvent().CheckEvent(&event))
  {
    const auto * mrFilter = dynamic_cast<const MRFilterType *>(caller);

    if (mrFilter)
    {
      // TODO what should happen here?
    }
  }
}

/**
 * Standard "PrintSelf" method.
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationLogger<TRegistrationFilter, TMRFilter>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
