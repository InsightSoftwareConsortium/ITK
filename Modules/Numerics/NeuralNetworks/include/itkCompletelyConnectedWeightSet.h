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
#ifndef itkCompletelyConnectedWeightSet_h
#define itkCompletelyConnectedWeightSet_h

#include "itkWeightSetBase.h"

namespace itk
{
namespace Statistics
{
/** \class CompletelyConnectedWeightSet
 * \brief This is the itkCompletelyConnectedWeightSet class.
 *
 * \ingroup ITKNeuralNetworks
 */

 template<typename TMeasurementVector, typename TTargetVector>
class ITK_TEMPLATE_EXPORT CompletelyConnectedWeightSet : public WeightSetBase<TMeasurementVector, TTargetVector>
{
public:
  #define MAX_SIZE 1000

  typedef CompletelyConnectedWeightSet                     Self;
  typedef WeightSetBase<TMeasurementVector, TTargetVector> Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef SmartPointer<const Self>                         ConstPointer;

  itkTypeMacro(CompletelyConnectedWeightSet, WeightSetBase);
  itkNewMacro(Self);

  void SetCompleteConnectivity();
  void SetRandomConnectivity(int[][MAX_SIZE]);

protected:

  CompletelyConnectedWeightSet();
  virtual ~CompletelyConnectedWeightSet() ITK_OVERRIDE {};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompletelyConnectedWeightSet.hxx"
#endif

#endif
