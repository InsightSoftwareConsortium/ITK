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
#ifndef itkNeuralNetworkFileWriter_hxx
#define itkNeuralNetworkFileWriter_hxx

#include "itkNeuralNetworkFileWriter.h"

namespace itk
{
/** Constructor */
template< typename TNetwork >
NeuralNetworkFileWriter< TNetwork >
::NeuralNetworkFileWriter()
{
  this->m_FileName = "";
  this->m_WriteWeightValuesType = Self::BINARY; //Default: binary output
}

template< typename TNetwork >
void
NeuralNetworkFileWriter< TNetwork >
//Avoiding VS6 error::SetInput( const TNetwork* network )
::SetInput(TNetwork *network)
{
  this->m_Network = network;
}

/** Destructor */
template< typename TNetwork >
NeuralNetworkFileWriter< TNetwork >
::~NeuralNetworkFileWriter()
{
  this->ClearFields();
}

template< typename TNetwork >
void
NeuralNetworkFileWriter< TNetwork >
::ClearFields()
{
  for ( FieldsContainerType::size_type i = 0; i <  this->m_Fields.size(); i++ )
    {
    delete this->m_Fields[i];
    }
  this->m_Fields.clear();
}

template< typename TNetwork >
void
NeuralNetworkFileWriter< TNetwork >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TNetwork >
const TNetwork *
NeuralNetworkFileWriter< TNetwork >
::GetInput() const
{
  return this->m_Network.GetPointer();
}

/** Update the Writer */
template< typename TNetwork >
void
NeuralNetworkFileWriter< TNetwork >
::Update()
{
  this->m_OutputFile.open(this->m_FileName.c_str(), std::ios::binary | std::ios::out);

  if ( !this->m_OutputFile.is_open() )
    {
    itkExceptionMacro("NeuralNetworkFileReader Write: Cannot open file");
    return;
    }

  MET_FieldRecordType *mF;
  mF = new MET_FieldRecordType;
  if ( MET_SystemByteOrderMSB() )
    {
    MET_InitWriteField(mF, "BinaryDataByteOrderMSB", MET_STRING,
                       strlen("True"), "True");
    }
  else
    {
    MET_InitWriteField(mF, "BinaryDataByteOrderMSB", MET_STRING,
                       strlen("False"), "False");
    }
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField( mF, "ObjectType", MET_STRING,
                      strlen( this->m_Network->GetNameOfClass() ), this->m_Network->GetNameOfClass() );

  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField( mF, "NLayers", MET_UINT, this->m_Network->GetNumOfLayers() );
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField( mF, "NWeightSets",
                      MET_UINT, this->m_Network->GetNumOfWeightSets() );
  this->m_Fields.push_back(mF);

  std::cout << "Num of Weights = " << this->m_Network->GetNumOfWeightSets() << std::endl;
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "WeightValuesType", MET_UINT, this->m_WriteWeightValuesType);
  mF->terminateRead = true;
  this->m_Fields.push_back(mF);

  if ( !MET_Write(this->m_OutputFile, &this->m_Fields, '=') )
    {
    itkExceptionMacro("MetaObject: Write: MET_Write Failed");
    }

  this->ClearFields();

  //Get Layer Information for each layer
  for ( int i = 0; i < this->m_Network->GetNumOfLayers(); i++ )
    {
    LayerBaseConstPointer layerPtr = this->m_Network->GetLayer(i);
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Layer_Id", MET_INT, i);
    this->m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField( mF, "NumNodes", MET_INT, layerPtr->GetNumberOfNodes() );
    this->m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField( mF, "LayerType", MET_STRING,
                        strlen( layerPtr->GetNameOfClass () ), layerPtr->GetNameOfClass () );
    this->m_Fields.push_back(mF);

    typename TNetwork::TransferFunctionInterfaceType::ConstPointer tf = layerPtr->GetActivationFunction();
      {
      mF = new MET_FieldRecordType;
      if ( tf.IsNotNull() )
        {
        MET_InitWriteField( mF, "TransferFunction", MET_STRING,
                            strlen( tf->GetNameOfClass() ), tf->GetNameOfClass() );
        }
      else
        {
        MET_InitWriteField(mF, "TransferFunction", MET_STRING,
                           strlen("ITK_NULLPTR"), "ITK_NULLPTR");
        }
      this->m_Fields.push_back(mF);
      }

    //NOTE:  One and Two HiddenLayerBackpropogation networks don't have
    // InputFunction Nodes.
    typename TNetwork::InputFunctionInterfaceType::ConstPointer inputf = layerPtr->GetNodeInputFunction();
      {
      mF = new MET_FieldRecordType;
      if ( inputf.IsNotNull() )
        {
        MET_InitWriteField( mF, "InputFunction", MET_STRING,
                            strlen( inputf->GetNameOfClass() ), inputf->GetNameOfClass() );
        }
      else
        {
        MET_InitWriteField(mF, "InputFunction", MET_STRING,
                           strlen("ITK_NULLPTR"), "ITK_NULLPTR");
        }
      mF->terminateRead = true;
      this->m_Fields.push_back(mF);
      }
    }

  if ( !MET_Write(this->m_OutputFile, &this->m_Fields, '=') )
    {
    itkExceptionMacro("MetaObject: Write: MET_Write Failed");
    }

  this->ClearFields();
  for ( int j = 0; j < this->m_Network->GetNumOfWeightSets(); j++ )
    {
    //typename Statistics::WeightSetBase<typename
    // TNetwork::MeasurementVectorType, typename
    // TNetwork::TargetVectorType>::ConstPointer
    const typename TNetwork::LayerInterfaceType::WeightSetInterfaceType * const
    weightset =  this->m_Network->GetWeightSet(j);
    mF = new MET_FieldRecordType;
    MET_InitWriteField( mF, "WeightSet_Id", MET_INT, weightset->GetWeightSetId() );
    this->m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField( mF, "WeightSetType", MET_STRING,
                        strlen( weightset->GetNameOfClass () ), weightset->GetNameOfClass () );
    this->m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField( mF, "SRC_Layer", MET_INT, weightset->GetInputLayerId() );
    this->m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField( mF, "DEST_Layer", MET_INT, weightset->GetOutputLayerId() );
    this->m_Fields.push_back(mF);
    }

  if ( !MET_Write(this->m_OutputFile, &this->m_Fields, '=') )
    {
    itkExceptionMacro("MetaObject: Write: MET_Write Failed");
    }

  //Writeout the weight values
  this->ClearFields();
  for ( int j = 0; j < this->m_Network->GetNumOfWeightSets(); j++ )
    {
    //typename Statistics::WeightSetBase<typename
    // TNetwork::MeasurementVectorType, typename
    // TNetwork::TargetVectorType>::ConstPointer
    const typename TNetwork::LayerInterfaceType::WeightSetInterfaceType * const
    weightset =  this->m_Network->GetWeightSet(j);
    unsigned int rows = weightset->GetNumberOfOutputNodes();
    unsigned int cols = weightset->GetNumberOfInputNodes();
    switch ( this->m_WriteWeightValuesType )
      {
      //ASCII only works for very small networks (i.e. less than 256 weights),
      //and the MetaIO mechanism is not desigend for the way that this is used
      //to write these files out.
      // Comment this code out until it can be robustly written.
      case ASCII:
        {
        // create local scope
          {
          std::cout << "UNSUPPORTED: ASCII only works for very small network types" << std::endl;
          mF = new MET_FieldRecordType;
          MET_InitWriteField( mF, "WeightValues", MET_FLOAT_ARRAY,
                              weightset->GetNumberOfOutputNodes() * weightset->GetNumberOfInputNodes(),
                              weightset->GetWeightValues() );
          this->m_Fields.push_back(mF);
          }
        // create local scope
          {
          if ( !MET_Write(this->m_OutputFile, &this->m_Fields, '=') )
            {
            itkExceptionMacro("MetaObject: Write: MET_Write Failed");
            return;
            }
          }
        }
        break;
      case BINARY:
        {
        //
        // TODO: This is hardcoded to double for the weight values.
        // Do the ITK Neural Nets allow single precision NN?
        this->m_OutputFile.write( (char *)weightset->GetWeightValues(),
                                  rows * cols * sizeof( double ) );
        }
        break;
      default:
        itkExceptionMacro("Unsupported type given");
        break;
      }
    }
}
} // namespace itk

#endif
