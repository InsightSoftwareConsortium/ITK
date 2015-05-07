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
#include "itkProbabilityDistribution.h"

namespace itk
{
namespace Statistics
{
void ProbabilityDistribution::SetParameters(const ParametersType & params)
{
    if ( ( params.GetSize() != m_Parameters.GetSize() )
        || ( params != m_Parameters ) )
    {
        m_Parameters = params;
        this->Modified();
    }
}

ProbabilityDistribution::ProbabilityDistribution(void) {}

ProbabilityDistribution::~ProbabilityDistribution(void) {}

void ProbabilityDistribution::PrintSelf(std::ostream & os, Indent indent) const
{
    Superclass::PrintSelf(os, indent);
    os << indent << "Parameters: " << m_Parameters << std::endl;
}
}
}
