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
#ifndef itkLayerBase_h
#define itkLayerBase_h

#include <iostream>
#include "itkTransferFunctionBase.h"
#include "itkInputFunctionBase.h"

#include "itkWeightSetBase.h"

namespace itk
{
namespace Statistics
{
/** \class LayerBase
 * \brief This is the itkLayerBase class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename TTargetVector>
class ITK_TEMPLATE_EXPORT LayerBase : public LightProcessObject
{
public:
  typedef LayerBase                Self;
  typedef LightProcessObject       Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(LayerBase, LightProcessObject);

  typedef TMeasurementVector InputVectorType;
  typedef TTargetVector      OutputVectorType;

  typedef typename TMeasurementVector::ValueType ValueType;
  typedef ValueType*                             ValuePointer;
  typedef const ValueType*                       ValueConstPointer;
  typedef vnl_vector<ValueType>                  NodeVectorType;
  typedef Array<ValueType>                       InternalVectorType;

  typedef LayerBase                                       LayerInterfaceType;
  typedef WeightSetBase<TMeasurementVector,TTargetVector> WeightSetType;
  typedef WeightSetBase<TMeasurementVector,TTargetVector> WeightSetInterfaceType;
  typedef InputFunctionBase<ValueType*, ValueType>        InputFunctionInterfaceType;
  typedef TransferFunctionBase<ValueType>                 TransferFunctionInterfaceType;

  //The only valid layer types
  typedef enum {  INVALIDLAYER=0, INPUTLAYER=1, HIDDENLAYER=2, OUTPUTLAYER=3 } LayerTypeCode;

  virtual void SetNumberOfNodes(unsigned int);
  unsigned int GetNumberOfNodes() const;

  virtual ValueType GetInputValue(unsigned int) const = 0;
  virtual ValueType GetOutputValue(unsigned int) const = 0;
  virtual ValuePointer GetOutputVector() = 0;

  virtual void ForwardPropagate(){};
  virtual void ForwardPropagate(TMeasurementVector){};

  virtual void BackwardPropagate(){};
  virtual void BackwardPropagate(InternalVectorType){};

  virtual ValueType GetOutputErrorValue(unsigned int) const = 0;
  virtual void SetOutputErrorValues(TTargetVector) {};

  virtual ValueType GetInputErrorValue(unsigned int) const = 0;
  virtual ValuePointer GetInputErrorVector() = 0;
  virtual void SetInputErrorValue(ValueType, unsigned int) {};

  //itkSetObjectMacro(InputWeightSet, WeightSetInterfaceType);
  void SetInputWeightSet(WeightSetInterfaceType*);
  itkGetModifiableObjectMacro(InputWeightSet, WeightSetInterfaceType);

  //itkSetObjectMacro(OutputWeightSet, WeightSetInterfaceType);
  void SetOutputWeightSet(WeightSetInterfaceType*);
  itkGetModifiableObjectMacro(OutputWeightSet, WeightSetInterfaceType);

  void SetNodeInputFunction(InputFunctionInterfaceType* f);
  itkGetModifiableObjectMacro(NodeInputFunction, InputFunctionInterfaceType);

  void SetTransferFunction(TransferFunctionInterfaceType* f);
  itkGetModifiableObjectMacro(ActivationFunction, TransferFunctionInterfaceType);

  virtual ValueType Activation(ValueType) = 0;
  virtual ValueType DActivation(ValueType) = 0;

  itkSetEnumMacro(LayerTypeCode, LayerTypeCode);
  itkGetEnumMacro(LayerTypeCode, LayerTypeCode);

  //#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
#ifdef __USE_OLD_INTERFACE
  void SetLayerType(const LayerTypeCode value) { SetLayerTypeCode(value); }
  LayerTypeCode GetLayerType(void) { return GetLayerTypeCode(); }
  //For backwards compatibility
  void SetLayerType(const unsigned int value)
    {
    switch(value)
      {
      case 0:
        SetLayerType(INVALIDLAYER);
        break;
      case 1:
        SetLayerType(INPUTLAYER);
        break;
      case 2:
        SetLayerType(HIDDENLAYER);
        break;
      case 3:
        SetLayerType(OUTPUTLAYER);
        break;
      default:
        //Throw Exception Here
        break;
      }
    }
#endif
  itkSetMacro(LayerId,unsigned int);
  itkGetConstReferenceMacro(LayerId,unsigned int);

  //virtual void SetBias(const ValueType) = 0;
  //virtual const ValueType & GetBias() const = 0;

protected:
  LayerBase();
  ~LayerBase() ITK_OVERRIDE;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  LayerTypeCode m_LayerTypeCode; //input, hidden, output
  unsigned int  m_LayerId;
  unsigned int  m_NumberOfNodes;

  typename WeightSetInterfaceType::Pointer m_InputWeightSet;
  typename WeightSetInterfaceType::Pointer m_OutputWeightSet;

  typename TransferFunctionInterfaceType::Pointer m_ActivationFunction;
  typename InputFunctionInterfaceType::Pointer    m_NodeInputFunction;

}; //class layer base

} //namespace itk
} //namespace statistics

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLayerBase.hxx"
#endif

#endif
