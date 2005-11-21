/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultilayerNeuralNetworkBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MultiLayerNeuralNetworkBase_h
#define __MultiLayerNeuralNetworkBase_h

#include "itkNeuralNetworkObject.h"
#include "itkErrorBackPropagationLearningFunctionBase.h"
#include "itkErrorBackPropagationLearningWithMomentum.h"
#include "itkQuickPropLearningRule.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
class MultilayerNeuralNetworkBase : public NeuralNetworkObject<TVector, TOutput>
{
public:

  typedef MultilayerNeuralNetworkBase Self;
  typedef NeuralNetworkObject<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkTypeMacro(MultilayerNeuralNetworkBase, NeuralNetworkObject);

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self ); 

  typedef typename Superclass::ValueType ValueType;

  typedef typename Superclass::LayerType LayerType;
  typedef typename Superclass::WeightSetType WeightSetType;
  typedef typename Superclass::WeightSetPointer WeightSetPointer;
  typedef typename Superclass::LayerPointer LayerPointer;
  typedef typename Superclass::LearningFunctionType LearningFunctionType;
  typedef typename Superclass::LearningFunctionPointer LearningFunctionPointer;

  typedef std::vector<WeightSetPointer> WeightVectorType;
  typedef std::vector<LayerPointer> LayerVectorType;

  itkSetMacro(NumOfLayers, int);
  itkGetConstReferenceMacro(NumOfLayers, int);

  void AddLayer(LayerType*);

  void AddWeightSet(WeightSetType*);

  void SetLearningFunction(LearningFunctionType* f);

  virtual ValueType* GenerateOutput(TVector samplevector);

  virtual void BackwardPropagate(TOutput errors);

  virtual void UpdateWeights(ValueType);

  void SetLearningRule(LearningFunctionType*);

  void SetLearningRate(ValueType learningrate);

  void InitializeWeights();

protected:
  MultilayerNeuralNetworkBase();
  ~MultilayerNeuralNetworkBase(); 

  LayerVectorType           m_Layers;
  WeightVectorType          m_Weights;
  LearningFunctionPointer   m_LearningFunction;
  ValueType                 m_LearningRate;
  int                       m_NumOfLayers;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMultilayerNeuralNetworkBase.txx"
#endif

#endif
