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

#include <metaTypes.h>
#include <metaUtils.h>

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

template<class TVector, class TOutput>
class NeuralNetworkFileReader : public Object
{
public:
  
  /** SmartPointer typedef support */
  typedef NeuralNetworkFileReader Self;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(NeuralNetworkFileReader,Object);
  
  /** Method for creation through the object factory */
  itkNewMacro(Self);

  typedef Statistics::MultilayerNeuralNetworkBase<TVector,TOutput> NetworkType;
  typedef typename NetworkType::Pointer NetworkPointer;
  
  typedef Statistics::LayerBase<TVector,TOutput> LayerType;
  typedef typename LayerType::Pointer LayerPointer;
  
  typedef typename LayerType::ValueType ValueType;

  typedef typename NetworkType::WeightSetType WeightSetType;
  typedef Statistics::CompletelyConnectedWeightSet<TVector, TOutput> 
                                                             CompletelyConnectedWeighttype;
 
  typedef Statistics::BackPropagationLayer<TVector,TOutput> BackPropLayerType;
  typedef typename BackPropLayerType::Pointer BPLayerPointerType;
   
  typedef typename CompletelyConnectedWeighttype::Pointer CompletelyWeightSetPointer;
  typedef typename WeightSetType::Pointer WeightSetPointer;
 
  std::vector<LayerPointer> m_layers;
  std::vector<WeightSetPointer> m_weights;
  struct lines{ std::string name;
                std::string value;
                };
  std::list<lines> m_namevalue;  

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Read NeuralNetwork */
  void Update(void);

  NetworkType* GetNetwork();

  itkSetMacro(ReadWeightValuesType, unsigned int);
  itkGetMacro(ReadWeightValuesType, unsigned int);

 
protected:
  std::ifstream netInputfile;
  typedef std::vector<MET_FieldRecordType *> FieldsContainerType;
  FieldsContainerType m_Fields;
  std::string m_FileName;
  NeuralNetworkFileReader();
  virtual ~NeuralNetworkFileReader();
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
  
private:
  NetworkPointer    m_Network;
  int m_ReadWeightValuesType;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkNeuralNetworkFileReader.txx"
#endif

#endif 

