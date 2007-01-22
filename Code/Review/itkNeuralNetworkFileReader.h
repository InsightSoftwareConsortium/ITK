/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeuralNetworkFileReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkNeuralNetworkFileReader_h
#define __itkNeuralNetworkFileReader_h

#include "metaTypes.h"
#include "metaUtils.h"

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

/** \class NeuralNetworkFileReader
 * \brief Reader for Neural Network
 *
 * This class will enable the user to load neural network and define
 * weights and other network parameters from a text file.
 * 
 * This class was contributed to the Insight Journal by  Raghu Venkatram  
 *   http://hdl.handle.net/1926/203
 *
 * \sa NeuralNetworkFileWriter
 * \sa MultilayerNeuralNetworkBase
 *  
 *
 * \group IOFilter
 *
 */
template<class TVector, class TOutput>
class NeuralNetworkFileReader : public Object
{
public:
  
  /** SmartPointer typedef support */
  typedef NeuralNetworkFileReader                       Self;
  typedef Object                                        Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NeuralNetworkFileReader,Object);
  
  /** Method for creation through the object factory */
  itkNewMacro(Self);

  typedef Statistics::MultilayerNeuralNetworkBase<TVector,TOutput> NetworkType;
  typedef typename NetworkType::Pointer NetworkPointer;
  
  typedef Statistics::LayerBase<TVector,TOutput>        LayerType;
  typedef typename LayerType::Pointer                   LayerPointer;
  
  typedef typename LayerType::ValueType ValueType;

  typedef typename NetworkType::WeightSetType WeightSetType;
  typedef Statistics::CompletelyConnectedWeightSet<TVector, TOutput> 
                                          CompletelyConnectedWeighttype;
 
  typedef Statistics::BackPropagationLayer<TVector,TOutput> BackPropLayerType;
  typedef typename BackPropLayerType::Pointer               BPLayerPointerType;
   
  typedef typename CompletelyConnectedWeighttype::Pointer 
                                          CompletelyWeightSetPointer;

  typedef typename WeightSetType::Pointer WeightSetPointer;
 
  /** Set the filename  */
  itkSetStringMacro( FileName );

  /** Get the filename */
  itkGetStringMacro( FileName );

  /** Read NeuralNetwork */
  void Update(void);

  NetworkType * GetOutput() const;

  itkSetMacro( ReadWeightValuesType, unsigned int );
  itkGetConstMacro( ReadWeightValuesType, unsigned int );

 
protected:
  NeuralNetworkFileReader();
  virtual ~NeuralNetworkFileReader();
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
  
private:

  typedef std::vector<MET_FieldRecordType *>      FieldsContainerType;
  typedef std::vector< LayerPointer >             LayersContainer;
  typedef std::vector<WeightSetPointer>           WeightsContainer;

  typedef struct 
    { 
    std::string name;
    std::string value;
    }                                              LineType;

  typedef std::list< LineType >                    LinesContainer;


  NetworkPointer         m_Network;
  int                    m_ReadWeightValuesType;
  LayersContainer        m_Layers;
  WeightsContainer       m_Weights;
  LinesContainer         m_NameValue;  
  std::string            m_FileName;
  FieldsContainerType    m_Fields;
  std::ifstream          m_InputFile;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeuralNetworkFileReader.txx"
#endif

#endif 
