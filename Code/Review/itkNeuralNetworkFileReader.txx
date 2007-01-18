/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeuralNetworkFileReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _NeuralNetworkFileReader_txx
#define _NeuralNetworkFileReader_txx

#include <itksys/ios/sstream>
#include "itkNeuralNetworkFileReader.h"

namespace itk
{
/** Constructor */
template<class TVector, class TOutput>
NeuralNetworkFileReader<TVector,TOutput>
::NeuralNetworkFileReader()
{
  m_FileName = "";
  m_ReadWeightValuesType = 0;
}

template<class TVector, class TOutput>
typename NeuralNetworkFileReader<TVector,TOutput>::NetworkType*
NeuralNetworkFileReader<TVector,TOutput>
::GetNetwork() 
{
 return m_Network.GetPointer();
}

/** Destructor */
template<class TVector, class TOutput>
NeuralNetworkFileReader<TVector,TOutput>
::~NeuralNetworkFileReader()
{
}


template<class TVector, class TOutput>
void
NeuralNetworkFileReader<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  Superclass::PrintSelf( os, indent ); 
} 



/** Update the Reader */
template<class TVector, class TOutput>
void
NeuralNetworkFileReader<TVector,TOutput>
::Update()
{  
  //std::ifstream in;
  netInputfile.open(m_FileName.c_str(), std::ios::binary | std::ios::in);
  netInputfile.seekg(0,std::ios::beg);

   if(!netInputfile.is_open())
    {
    std::cout << "NeuralNetworkFileReader Read: Cannot open file" << std::endl;
    return;
    }
 
  unsigned int num_layers=0;
  unsigned int num_weights=0;

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectType", MET_STRING, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NLayers", MET_UINT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NWeightSets", MET_UINT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "WeightValuesType", MET_UINT, true);
  mF->terminateRead=true; 
  m_Fields.push_back(mF);
 
  if(!MET_Read(netInputfile, & m_Fields,'='))
    {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    //return false;
    }

  mF = MET_GetFieldRecord("ObjectType", &m_Fields);
  std::cout<<"Network type = "<<(char *)(mF->value)<<std::endl;
  std::cout<<"desired type = "<<"MultilayerNeuralNetwork"<<std::endl;
  if(!strcmp((char *)(mF->value),"MultilayerNeuralNetworkBase"))
  {  
  
    m_Network= NetworkType::New();
    std::cout<<"Network declared"<<std::endl;
  }

  mF = MET_GetFieldRecord("NLayers", &m_Fields);
  num_layers=(unsigned int)mF->value[0];

  mF = MET_GetFieldRecord("NWeightSets", &m_Fields);
  num_weights=(unsigned int)mF->value[0];
 
  mF = MET_GetFieldRecord("WeightValuesType", &m_Fields);
  m_ReadWeightValuesType=(unsigned int)mF->value[0];
 
  std::cout<<"Num layers = "<<num_layers<<std::endl;
  std::cout<<"Num weights = "<<num_weights<<std::endl;
  
  
  m_Network->SetNumOfLayers(num_layers);
  
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Layer_Id", MET_UINT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NumNodes", MET_UINT, true);
  m_Fields.push_back(mF);
  int num_nodes = (int)mF->value[0];

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "LayerType", MET_STRING, true);
  m_Fields.push_back(mF);
  
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "TransferFunction", MET_STRING, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "InputFunction", MET_STRING, true);
  mF->terminateRead=true; 
  m_Fields.push_back(mF);
  

  // define the layers
  for(int i=0; i<num_layers; i++)
  {
    if(!MET_Read(netInputfile, & m_Fields,'='))
    {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    return;
    }

    mF = MET_GetFieldRecord("LayerType", &m_Fields);
    if(!strcmp((char*)mF->value,"BackPropagationLayer"))
    {
     BPLayerPointerType layerptr = BackPropLayerType::New();
     layerptr->SetBias(1.0);

     mF = MET_GetFieldRecord("Layer_Id", &m_Fields);
     layerptr->SetLayerId((int)mF->value[0]);

     mF = MET_GetFieldRecord("NumNodes", &m_Fields);
     layerptr->SetNumberOfNodes((int)mF->value[0]);
     LayerPointer layer = static_cast<LayerPointer>(layerptr);
     m_layers.push_back(layer);

     mF = MET_GetFieldRecord("TransferFunction", &m_Fields);
     if(!strcmp((char*)mF->value,"IdentityTransferFunction"))
     {
         typedef Statistics::IdentityTransferFunction<ValueType> tfType;
         typename tfType::Pointer tf=tfType::New();
         layerptr->SetTransferFunction(tf);
     }
     else if(!strcmp((char*)mF->value,"LogSigmoidTransferFunction"))
     {
         typedef Statistics::LogSigmoidTransferFunction<ValueType> tfType;
         typename tfType::Pointer tf=tfType::New();
         layerptr->SetTransferFunction(tf);
     }
     else if(!strcmp((char*)mF->value,"SigmoidTransferFunction"))
     {
         typedef Statistics::SigmoidTransferFunction<ValueType> tfType;
         typename tfType::Pointer tf=tfType::New();
         layerptr->SetTransferFunction(tf);
     }
     else if(!strcmp((char*)mF->value,"TanSigmoidTransferFunction"))
     {
         std::cout<<"Tansigmoid"<<std::endl;
         typedef Statistics::TanSigmoidTransferFunction<ValueType> tfType;
         typename tfType::Pointer tf=tfType::New();
         layerptr->SetTransferFunction(tf);
     }
     else if(!strcmp((char*)mF->value,"SymmetricSigmoidTransferFunction"))
     {
         std::cout<<"SymmetricSigmoidTransferFunction"<<std::endl;
         typedef Statistics::SymmetricSigmoidTransferFunction<ValueType> tfType;
         typename tfType::Pointer tf=tfType::New();
         layerptr->SetTransferFunction(tf);
     }

     mF = MET_GetFieldRecord("InputFunction", &m_Fields);
     if(!strcmp((char*)(mF->value),"SumInputFunction"))
     {             
          typedef Statistics::SumInputFunction
                       <ValueType*,ValueType>
                                                ifType;
          typename  ifType::Pointer ifcn= ifType::New(); 
          layerptr->SetNodeInputFunction(ifcn);
     }
 
     m_Network->AddLayer(layerptr);      
   }

  } 
 

  m_Fields.clear();
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "WeightSet_Id", MET_UINT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "WeightSetType", MET_STRING, true);
  m_Fields.push_back(mF);
  
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "SRC_Layer", MET_UINT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "DEST_Layer", MET_UINT, true);
  mF->terminateRead=true; 
  m_Fields.push_back(mF);

  // define the weightsets
  for(int i=0; i<num_weights; i++)
  {  
   if(!MET_Read(netInputfile, & m_Fields,'='))
   {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    //return false;
   }
   mF = MET_GetFieldRecord("WeightSetType", &m_Fields);
  
   if(!strcmp((char*)(mF->value),"CompletelyConnectedWeightSet"))
   {    
     typename CompletelyConnectedWeighttype::Pointer w =CompletelyConnectedWeighttype::New();
     mF = MET_GetFieldRecord("WeightSet_Id", &m_Fields); 
     unsigned int weightsetid = (unsigned int)mF->value[0];
     mF = MET_GetFieldRecord("SRC_Layer", &m_Fields); 
     unsigned int slayer=(unsigned int)mF->value[0];
     mF = MET_GetFieldRecord("DEST_Layer", &m_Fields); 
     unsigned int dlayer=(unsigned int)mF->value[0];
     
     w->SetWeightSetId(weightsetid);    
     w->SetNumberOfInputNodes(m_layers[slayer]->GetNumberOfNodes());
     w->SetNumberOfOutputNodes(m_layers[dlayer]->GetNumberOfNodes());
     w->SetCompleteConnectivity();
     w->SetRange(1.0);  //0.5
     w->Initialize(); 
     WeightSetPointer weightset=static_cast<WeightSetPointer>(w);
     m_Network->AddWeightSet(w);
     m_weights.push_back(weightset); 
     m_layers[slayer]->SetOutputWeightSet(w);
     m_layers[dlayer]->SetInputWeightSet(w);         
   }        
  }
   std::cout<<"Weightsets defined"<<std::endl;
  
  //Read Weight Values
  WeightSetPointer weightset;
  if(m_ReadWeightValuesType>0)
  {
    m_Network->Initialize();
    for(int j=0; j<m_Network->GetNumOfWeightSets(); j++)
    {
     m_Fields.clear();
     weightset = m_Network->GetWeightSet(j);
     unsigned int rows =weightset->GetNumberOfOutputNodes();
     unsigned int cols =weightset->GetNumberOfInputNodes();
     
     mF = new MET_FieldRecordType;
     MET_InitReadField(mF, "WeightValues",MET_FLOAT_ARRAY, true,-1,
                                                         rows*cols);
     mF->required = true;
     mF->terminateRead=true; 
     m_Fields.push_back(mF);
     
     if(m_ReadWeightValuesType==1) // Read ASCII weights
     {
       if(!MET_Read(netInputfile, & m_Fields,'='))
       {
         std::cout << "MetaObject: Read: MET_Read Failed Weight Values missing" << std::endl;
         return;
        }
       weightset->SetWeightValues(mF->value);
     }
     else if (m_ReadWeightValuesType==2) // Read Binary Weights
     {
      vnl_matrix<ValueType>WeightMatrix;
      WeightMatrix.set_size(rows, cols);
      netInputfile.read((char *)WeightMatrix.data_block(), rows*cols*sizeof(double));
      std::cout<<"WeightValues = "<<WeightMatrix<<std::endl; 
      weightset->SetWeightValues(WeightMatrix.data_block());
     }
    }
  }
  netInputfile.close();
}


} // namespace itk

#endif
