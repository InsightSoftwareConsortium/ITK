/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNeuralNetworkFileWriter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkNeuralNetworkFileWriter_h
#define __itkNeuralNetworkFileWriter_h

#include <metaTypes.h>
#include <metaUtils.h>
#include <typeinfo>

#include "itkMultilayerNeuralNetworkBase.h"
#include "itkOneHiddenLayerBackPropagationNeuralNetwork.h"
#include "itkTwoHiddenLayerBackPropagationNeuralNetwork.h"

#include "itkBackPropagationLayer.h"
#include "itkCompletelyConnectedWeightSet.h"

#include "itkSumInputFunction.h"
#include "itkProductInputFunction.h"

#include "itkIdentityTransferFunction.h"
#include "itkLogSigmoidTransferFunction.h"
#include "itkSigmoidTransferFunction.h"
#include "itkTanSigmoidTransferFunction.h"
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
 *   http://insight-journal.org/midas/handle.php?handle=1926/203
 *
 *
 * \author Raghu Venkatram
 *
 * \sa NeuralNetworkFileReader
 * \sa MultilayerNeuralNetworkBase
 *
 *
 * \group IOFilter
 *
 */
template< class TNetwork >
class NeuralNetworkFileWriter:public Object
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
  void Update(void);

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
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

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
#include "itkNeuralNetworkFileWriter.txx"
#endif

#endif
