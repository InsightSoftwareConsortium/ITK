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
#ifndef itkNeuralNetworkFileWriter_h
#define itkNeuralNetworkFileWriter_h

#include <metaTypes.h>
#include <metaUtils.h>
#include <typeinfo>

#include "itkOneHiddenLayerBackPropagationNeuralNetwork.h"
#include "itkTwoHiddenLayerBackPropagationNeuralNetwork.h"

#include "itkCompletelyConnectedWeightSet.h"


#include "itkSymmetricSigmoidTransferFunction.h"

namespace itk
{
/** \class NeuralNetworkFileWriter
 * \brief Writer for Neural Network
 *
 * This class will enable a user to save a trained neural network to a text
 * file. The user can also change the network topology by editing the network
 * configuration file.
 *
 * This class was contributed to the Insight Journal by  Raghu Venkatram
 * The original paper can be found at
 *   https://hdl.handle.net/1926/203
 *
 *
 * \author Raghu Venkatram
 *
 * \sa NeuralNetworkFileReader
 * \sa MultilayerNeuralNetworkBase
 *
 * \ingroup ITKReview
 */
template< typename TNetwork >
class ITK_TEMPLATE_EXPORT NeuralNetworkFileWriter:public Object
{
public:

  /** SmartPointer typedef support */
  typedef NeuralNetworkFileWriter    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NeuralNetworkFileWriter, Object);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  typedef typename TNetwork::MeasurementVectorType MeasurementVectorType;
  typedef typename TNetwork::TargetVectorType      TargetVectorType;

  typedef Statistics::LayerBase< MeasurementVectorType, TargetVectorType > LayerBaseType;
  typedef typename LayerBaseType::Pointer                                  LayerBasePointer;
  typedef typename LayerBaseType::ConstPointer                             LayerBaseConstPointer;

//   typedef typename TNetwork::Pointer                       NetworkPointer;
//   typedef typename TNetwork::ConstPointer
//                  NetworkConstPointer;

// typedef typename TNetwork::LayerType                     LayerType;
// typedef typename LayerType::Pointer                      LayerPointer;
// typedef typename LayerType::ConstPointer                 LayerConstPointer;
// typedef typename LayerType::TransferFunctionType::Pointer
//      TransferFunctionPointer;
// typedef typename LayerType::TransferFunctionType::ConstPointer
// TransferFunctionConstPointer;

// typedef typename LayerType::InputFunctionType::Pointer
//         InputFunctionPointer;
// typedef typename LayerType::InputFunctionType::ConstPointer
//    InputFunctionConstPointer;

// typedef typename LayerType::WeightSetType                WeightSetType;
// typedef typename LayerType::WeightSetPointer             WeightSetPointer;
// typedef typename LayerType::WeightSetConstPointer
//        WeightSetConstPointer;
//  typedef typename LayerType::ValueType                    ValueType;

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set/Get the input transform to write */
  //Avoiding VS6 compiler error void SetInput( const TNetwork* network );
  void SetInput(TNetwork *network);

  const TNetwork * GetInput() const;

  /** Read NeuralNetwork */
  void Update();

#ifdef IGNORE
#undef IGNORE
#endif
  //ASCII only works for very small networks (i.e. less than 256 weights),
  //and the MetaIO mechanism is not desigend for the way that this is used
  //to write these files out.
  // Comment this code out until it can be robustly written.
  typedef enum { IGNORE = 0, ASCII = 1, BINARY = 2 } NetworkWriteWeightsType;
  itkSetEnumMacro(WriteWeightValuesType, NetworkWriteWeightsType);
  itkGetEnumMacro(WriteWeightValuesType, NetworkWriteWeightsType);

protected:
  NeuralNetworkFileWriter();
  ~NeuralNetworkFileWriter();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  void ClearFields();

  typedef std::vector< MET_FieldRecordType * > FieldsContainerType;

  //Attempting to avoid VS 6 compiler error typename TNetwork::ConstPointer
  // m_Network;
  typename TNetwork::Pointer m_Network;

  NetworkWriteWeightsType m_WriteWeightValuesType;

  std::string         m_FileName;
  FieldsContainerType m_Fields;

  std::ofstream m_OutputFile;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeuralNetworkFileWriter.hxx"
#endif

#endif
