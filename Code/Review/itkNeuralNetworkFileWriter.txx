/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeuralNetworkFileWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _NeuralNetworkFileWriter_txx
#define _NeuralNetworkFileWriter_txx

#include <itksys/ios/sstream>
#include "itkNeuralNetworkFileWriter.h"

namespace itk
{
/** Constructor */
template<class TVector, class TOutput>
NeuralNetworkFileWriter<TVector,TOutput>
::NeuralNetworkFileWriter()
{
  m_FileName = "";
  m_WriteWeightValuesType =2; //Default: binary output
}


template<class TVector, class TOutput>
void
NeuralNetworkFileWriter<TVector,TOutput>
::SetInput(NetworkType* network) 
{
  m_Network = network; 
}

template<class TVector, class TOutput>
typename NeuralNetworkFileWriter<TVector,TOutput>::NetworkType*
NeuralNetworkFileWriter<TVector,TOutput>
::GetNetwork() 
{
 return m_Network.GetPointer();
}

/** Destructor */
template<class TVector, class TOutput>
NeuralNetworkFileWriter<TVector,TOutput>
::~NeuralNetworkFileWriter()
{
}


template<class TVector, class TOutput>
void
NeuralNetworkFileWriter<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  Superclass::PrintSelf( os, indent ); 
} 


template<class TVector, class TOutput>
void
NeuralNetworkFileWriter<TVector,TOutput>
::WriteWeights(int flag, float* values, int n)
{
   if(flag ==1) //Write ASCII
   {

   }
   else if (flag==2) //Write Binary
   {
   
   }
}

/** Update the Writer */
template<class TVector, class TOutput>
void
NeuralNetworkFileWriter<TVector,TOutput>
::Update()
{  
  netOutputfile.open(m_FileName.c_str(), std::ios::binary | std::ios::out);
  
  if(!netOutputfile.is_open())
  {
    std::cout << "NeuralNetworkFileReader Write: Cannot open file" << std::endl;
    return;
    }
 
  MET_FieldRecordType * mF;
  
  
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ObjectType", MET_STRING,
                     strlen(m_Network->GetNameOfClass()),m_Network->GetNameOfClass()); 
  m_Fields.push_back(mF);
  
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NLayers", MET_UINT,m_Network->GetNumOfLayers());
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NWeightSets", MET_UINT,m_Network->GetNumOfWeightSets());
  m_Fields.push_back(mF);
 
  std::cout<<"Num of Weights = "<<m_Network->GetNumOfWeightSets()<<std::endl;
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "WeightValuesType", MET_UINT,m_WriteWeightValuesType);
  mF->terminateRead=true; 
  m_Fields.push_back(mF);

/*
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Values", MET_STRING,0, "Random");  //not taken into account
  mF->terminateRead=true; 
  m_Fields.push_back(mF);
*/  
  if(!MET_Write(netOutputfile, & m_Fields,'='))
    {
    std::cout << "MetaObject: Write: MET_Write Failed" << std::endl;
    //return false;
    }
  
  m_Fields.clear();
  LayerPointer layer;
  //Get Layer Information for each layer
  for(int i=0; i<m_Network->GetNumOfLayers(); i++)
  {
   layer=m_Network->GetLayer(i);
   mF = new MET_FieldRecordType;
   MET_InitWriteField(mF, "Layer_Id", MET_INT, i);
   m_Fields.push_back(mF);

   mF = new MET_FieldRecordType;
   MET_InitWriteField(mF, "NumNodes", MET_INT, layer->GetNumberOfNodes());
   m_Fields.push_back(mF);

   mF = new MET_FieldRecordType;
   MET_InitWriteField(mF, "LayerType", MET_STRING,
                      strlen(layer->GetNameOfClass ()), layer->GetNameOfClass ()); 
   m_Fields.push_back(mF);
 
   TransferFunctionPointer tf = layer->GetActivationFunction();
   mF = new MET_FieldRecordType;
   MET_InitWriteField(mF, "TransferFunction", MET_STRING, 
                     strlen(tf->GetNameOfClass()), tf->GetNameOfClass());
   m_Fields.push_back(mF);

   InputFunctionPointer inputf = layer->GetNodeInputFunction();
   mF = new MET_FieldRecordType;
   MET_InitWriteField(mF, "InputFunction", MET_STRING, 
                     strlen(inputf->GetNameOfClass()), inputf->GetNameOfClass());
   mF->terminateRead=true; 
   m_Fields.push_back(mF);

  }

  if(!MET_Write(netOutputfile, & m_Fields,'='))
    {
    std::cout << "MetaObject: Write: MET_Write Failed" << std::endl;
    //return false;
    }
  
  m_Fields.clear();
  WeightSetPointer weightset;
  for(int j=0; j<m_Network->GetNumOfWeightSets(); j++)
  {
    weightset = m_Network->GetWeightSet(j);
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "WeightSet_Id", MET_INT,weightset->GetWeightSetId());
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "WeightSetType", MET_STRING,
                      strlen(weightset->GetNameOfClass ()),weightset->GetNameOfClass ()); 
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "SRC_Layer", MET_INT,weightset->GetInputLayerId());
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "DEST_Layer", MET_INT,weightset->GetOutputLayerId());
    m_Fields.push_back(mF);

  }   
  
  if(!MET_Write(netOutputfile, & m_Fields,'='))
    {
    std::cout << "MetaObject: Write: MET_Write Failed" << std::endl;
    //return false;
    }
  
  //Writeout the weight values
  m_Fields.clear();
  for(int j=0; j<m_Network->GetNumOfWeightSets(); j++)
  {
    weightset = m_Network->GetWeightSet(j);
    unsigned int rows =weightset->GetNumberOfOutputNodes();
    unsigned int cols =weightset->GetNumberOfInputNodes();
   
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "WeightValues", MET_FLOAT_ARRAY,
                      weightset->GetNumberOfOutputNodes()*weightset->GetNumberOfInputNodes(),
                      weightset->GetWeightValues());
    m_Fields.push_back(mF);

    if(m_WriteWeightValuesType==2)
    {    
       //WriteWeights(2,(float*)weightset->GetWeightValues(),
       //               weightset->GetNumberOfOutputNodes()*weightset->GetNumberOfInputNodes());
       netOutputfile.write( (char *)weightset->GetWeightValues(),
                           rows * cols * sizeof(double)); 
    }
 
  } 
  if(m_WriteWeightValuesType==1)
  {    
    if(!MET_Write(netOutputfile, & m_Fields,'='))
    {
      std::cout << "MetaObject: Write: MET_Write Failed" << std::endl;
      return;
    } 
  }
}


} // namespace itk

#endif
