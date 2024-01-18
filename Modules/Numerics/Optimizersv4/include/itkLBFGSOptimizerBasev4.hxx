/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLBFGSOptimizerBasev4_hxx
#define itkLBFGSOptimizerBasev4_hxx

namespace itk
{

template <typename TInternalVnlOptimizerType>
bool
LBFGSOptimizerBaseHelperv4<TInternalVnlOptimizerType>::report_iter()
{
  Superclass::report_iter();

  m_ItkObj->InvokeEvent(IterationEvent());
  m_ItkObj->m_CurrentIteration = this->num_iterations_;

  // Return true to terminate the optimization loop.
  if (this->num_iterations_ >= m_ItkObj->m_NumberOfIterations)
  {
    return true;
  }
  else
  {
    return false;
  }
}

} // namespace itk


#endif
