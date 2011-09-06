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
#include "itkObjectToObjectOptimizerBase.h"
#include "itkMultiThreader.h"

namespace itk
{

//-------------------------------------------------------------------
ObjectToObjectOptimizerBase
::ObjectToObjectOptimizerBase()
{
  this->m_Metric = NULL;
  this->m_Value = 0;
  // Initialize, but w/out calling SetNumberOfThreads, to avoid
  // valgrind warning.
  this->m_NumberOfThreads = MultiThreader::GetGlobalDefaultNumberOfThreads();
}

//-------------------------------------------------------------------
ObjectToObjectOptimizerBase
::~ObjectToObjectOptimizerBase()
{}

void
ObjectToObjectOptimizerBase
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of threads: " << this->m_NumberOfThreads << std::endl;
  os << indent << "Number of scales:  " << this->m_Scales.Size() << std::endl;
  os << indent << "Metric: " << std::endl;
  m_Metric->Print( os, indent.GetNextIndent() );
}

//-------------------------------------------------------------------
void
ObjectToObjectOptimizerBase
::SetNumberOfThreads( ThreadIdType number )
{
  if( number < 1 )
    {
    itkExceptionMacro("Number of threads must be > 0");
    }
  if( number != this->m_NumberOfThreads )
    {
    this->m_NumberOfThreads = number;
    this->Modified();
    }
}

//-------------------------------------------------------------------
void
ObjectToObjectOptimizerBase
::StartOptimization()
{
  /* Validate some settings */
  if( this->m_Metric.IsNull() )
    {
    itkExceptionMacro("m_Metric must be set.");
    return;
    }

  /* If m_Scales hasn't been set, we'll just ignore it. */
  if( this->m_Scales.Size() > 0 )
    {
    if( this->m_Scales.Size() != this->m_Metric->GetNumberOfParameters() )
      {
      itkExceptionMacro("Size of scales (" << this->m_Scales.Size()
                        << ") must be 0 or match size of parameters (" <<
                        this->m_Metric->GetNumberOfParameters() << ").");
      }
    /* Check that all values in m_Scales are > machine epsilon, to avoid
     * division by zero/epsilon */
    typedef ScalesType::size_type     SizeType;
    typedef ScalesType::ValueType     ValueType;
    for( SizeType i=0; i < this->m_Scales.Size(); i++ )
      {
      if( this->m_Scales[i] <= NumericTraits<ValueType>::epsilon() )
        {
        itkExceptionMacro("m_Scales values must be > epsilon.");
        }
      }
    }
}

}//namespace itk
