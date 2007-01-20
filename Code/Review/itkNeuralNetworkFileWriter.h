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
 *   http://hdl.handle.net/1926/203
 *
 * \sa NeuralNetworkFileReader
 * \sa MultilayerNeuralNetworkBase
 *  
 *
 * \group IOFilter
 *
 */
template<class TVector, class TOutput>
class NeuralNetworkFileWriter : public Object
{
public:
  
  /** SmartPointer typedef support */
  typedef NeuralNetworkFileWriter           Self;
  typedef Object                            Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>          ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NeuralNetworkFileWriter,Object);
  
  /** Method for creation through the object factory */
  itkNewMacro(Self);

  typedef Statistics::MultilayerNeuralNetworkBase<TVector,TOutput> 
                                                        NetworkType;

  typedef typename NetworkType::Pointer        NetworkPointer;
  typedef typename NetworkType::ConstPointer   NetworkConstPointer;
  
  typedef Statistics::LayerBase<TVector,TOutput>                  LayerType;

  typedef typename LayerType::Pointer                      LayerPointer;
  typedef typename LayerType::ConstPointer                 LayerConstPointer;

  typedef typename LayerType::TransferFunctionPointer 
                                                     TransferFunctionPointer;

  typedef typename LayerType::TransferFunctionConstPointer 
                                              TransferFunctionConstPointer;

  typedef typename LayerType::InputFunctionPointer 
                                                InputFunctionPointer;

  typedef typename LayerType::InputFunctionConstPointer
                                                      InputFunctionConstPointer;

  typedef typename LayerType::WeightSetPointer        WeightSetPointer;
  typedef typename LayerType::WeightSetConstPointer   WeightSetConstPointer;
  typedef typename LayerType::ValueType               ValueType;

  typedef typename NetworkType::WeightSetType               WeightSetType;
  typedef Statistics::BackPropagationLayer<TVector,TOutput> BackPropLayerType;
  typedef typename BackPropLayerType::Pointer               BPLayerPointerType;
  
  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set/Get the input transform to write */
  void SetInput( const NetworkType* network );
  const NetworkType * GetInput() const;

  /** Read NeuralNetwork */
  void Update(void);

  itkSetMacro(WriteWeightValuesType, unsigned int);
  itkGetMacro(WriteWeightValuesType, unsigned int);
  
protected:
  NeuralNetworkFileWriter();
  virtual ~NeuralNetworkFileWriter();
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:
  typedef std::vector<MET_FieldRecordType *> FieldsContainerType;

  NetworkConstPointer    m_Network;
  unsigned int           m_WriteWeightValuesType;
  
  std::string            m_FileName;
  FieldsContainerType    m_Fields;

  std::ofstream          m_OutputFile;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkNeuralNetworkFileWriter.txx"
#endif

#endif 
