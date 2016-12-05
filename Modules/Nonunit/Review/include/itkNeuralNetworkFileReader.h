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
#ifndef itkNeuralNetworkFileReader_h
#define itkNeuralNetworkFileReader_h

#include "metaTypes.h"
#include "metaUtils.h"

#include "itkMultilayerNeuralNetworkBase.h"

#include "itkBackPropagationLayer.h"


#include "itkSumInputFunction.h"
#include "itkProductInputFunction.h"

#include "itkIdentityTransferFunction.h"
#include "itkLogSigmoidTransferFunction.h"
#include "itkSigmoidTransferFunction.h"
#include "itkTanSigmoidTransferFunction.h"
#include "itkSymmetricSigmoidTransferFunction.h"

namespace itk
{
/** \class NeuralNetworkFileReader
 * \brief Reader for Neural Network
 *
 * This class will enable the user to load neural network and define
 * weights and other network parameters from a text file.
 *
 * This class was contributed to the Insight Journal by Raghu Venkatram.
 * The original paper can be found at
 *  https://hdl.handle.net/1926/203
 *
 * \author Raghu Venkatram
 *
 * \sa NeuralNetworkFileWriter
 * \sa MultilayerNeuralNetworkBase
 * \ingroup ITKReview
 */
template< typename TNetwork >
class ITK_TEMPLATE_EXPORT NeuralNetworkFileReader:public Object
{
public:

  /** SmartPointer typedef support */
  typedef NeuralNetworkFileReader    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NeuralNetworkFileReader, Object);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  typedef typename TNetwork::MeasurementVectorType MeasurementVectorType;
  typedef typename TNetwork::TargetVectorType      TargetVectorType;
  typedef Statistics::BackPropagationLayer<
    MeasurementVectorType, TargetVectorType >           BackPropagationLayerType;
  typedef typename BackPropagationLayerType::Pointer BackPropagationLayerPointer;
  typedef typename MeasurementVectorType::ValueType  MeasurementVectorValueType;

  typedef typename TNetwork::LayerInterfaceType      LayerInterfaceType;
  typedef typename LayerInterfaceType::WeightSetType WeightSetType;
  typedef typename WeightSetType::Pointer            WeightSetPointer;

// typedef typename TNetwork::Pointer                       NetworkPointer;
// typedef typename TNetwork::ConstPointer                  NetworkConstPointer;

//  typedef typename LayerInterfaceType::TransferFunctionType::Pointer
//      TransferFunctionPointer;
//  typedef typename LayerInterfaceType::TransferFunctionType::ConstPointer
// TransferFunctionConstPointer;

//  typedef typename LayerInterfaceType::InputFunctionType::Pointer
//         InputFunctionPointer;
//  typedef typename LayerInterfaceType::InputFunctionType::ConstPointer
//    InputFunctionConstPointer;

//  typedef typename LayerInterfaceType::ValueType                    ValueType;

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Read NeuralNetwork */
  void Update();

  TNetwork * GetOutput() const;

#ifdef IGNORE
#undef IGNORE
#endif
  typedef enum { IGNORE = 0, ASCII = 1, BINARY = 2 } NetworkWriteWeightsType;
  itkSetEnumMacro(ReadWeightValuesType, NetworkWriteWeightsType);
  itkGetEnumMacro(ReadWeightValuesType, NetworkWriteWeightsType);

protected:
  NeuralNetworkFileReader();
  ~NeuralNetworkFileReader();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  void ClearFields();

  typedef std::vector< MET_FieldRecordType * >                                                  FieldsContainerType;
  typedef std::vector< typename TNetwork::LayerInterfaceType::Pointer >                         LayersContainer;
  typedef std::vector< typename TNetwork::LayerInterfaceType::WeightSetInterfaceType::Pointer > WeightsContainer;

  typedef struct {
    std::string name;
    std::string value;
  }                                 LineType;

  typedef std::list< LineType > LinesContainer;

  typename TNetwork::Pointer m_Network;

  NetworkWriteWeightsType m_ReadWeightValuesType;
  LayersContainer         m_Layers;
  WeightsContainer        m_Weights;
  LinesContainer          m_NameValue;
  std::string             m_FileName;
  FieldsContainerType     m_Fields;
  std::ifstream           m_InputFile;
  bool                    m_BinaryDataByteOrderMSB;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeuralNetworkFileReader.hxx"
#endif

#endif
