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
#ifndef itkNeuralNetworkFileReader_hxx
#define itkNeuralNetworkFileReader_hxx

#include "itkNeuralNetworkFileReader.h"

namespace itk
{
/** Constructor */
template< typename TNetwork >
NeuralNetworkFileReader< TNetwork >
::NeuralNetworkFileReader()
{
  this->m_FileName = "";
  this->m_ReadWeightValuesType = Self::IGNORE;
  this->m_Network = TNetwork::New();
  this->m_BinaryDataByteOrderMSB = true;
}

template< typename TNetwork >
TNetwork *
NeuralNetworkFileReader< TNetwork >
::GetOutput() const
{
  return this->m_Network.GetPointer();
}

/** Destructor */
template< typename TNetwork >
NeuralNetworkFileReader< TNetwork >
::~NeuralNetworkFileReader()
{
  this->ClearFields();
}

template< typename TNetwork >
void
NeuralNetworkFileReader< TNetwork >
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
NeuralNetworkFileReader< TNetwork >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName:"           <<  this->m_FileName << std::endl;
  os << indent << "ReadWeightValuesType:" <<  this->m_ReadWeightValuesType
     << std::endl;
}

/** Update the Reader */
template< typename TNetwork >
void
NeuralNetworkFileReader< TNetwork >
::Update()
{
  //std::ifstream in;
  this->m_InputFile.open(this->m_FileName.c_str(), std::ios::binary | std::ios::in);
  this->m_InputFile.seekg(0, std::ios::beg);

  if ( !this->m_InputFile.is_open() )
    {
    itkExceptionMacro("NeuralNetworkFileReader Read: Cannot open file");
    }

  unsigned int num_layers = 0;
  unsigned int num_weights = 0;

  MET_FieldRecordType *mF;

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "BinaryDataByteOrderMSB", MET_STRING, false);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectType", MET_STRING, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NLayers", MET_UINT, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NWeightSets", MET_UINT, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "WeightValuesType", MET_UINT, true);
  mF->terminateRead = true;
  this->m_Fields.push_back(mF);

  if ( !MET_Read(this->m_InputFile, &this->m_Fields, '=') )
    {
    itkExceptionMacro("MetaObject: Read: MET_Read Failed");
    }
  mF = MET_GetFieldRecord("BinaryDataByteOrderMSB", &this->m_Fields);
  if ( strcmp( (char *)( mF->value ), "True" ) == 0 )
    {
    this->m_BinaryDataByteOrderMSB = true;
    }
  else
    {
    this->m_BinaryDataByteOrderMSB = false;
    }

  mF = MET_GetFieldRecord("ObjectType", &this->m_Fields);

  if ( !strcmp( (char *)( mF->value ), "MultilayerNeuralNetworkBase" ) )
    {
    this->m_Network = TNetwork::New();
    }

  mF = MET_GetFieldRecord("NLayers", &this->m_Fields);
  num_layers = (unsigned int)mF->value[0];

  mF = MET_GetFieldRecord("NWeightSets", &this->m_Fields);
  num_weights = (unsigned int)mF->value[0];

  mF = MET_GetFieldRecord("WeightValuesType", &this->m_Fields);
  switch ( static_cast< unsigned int >( mF->value[0] ) )
    {
    case ASCII:
      this->m_ReadWeightValuesType = ASCII;
      break;
    case BINARY:
      this->m_ReadWeightValuesType = BINARY;
      break;
    default:
      itkExceptionMacro("Invalid Weight Type Read");
      break;
    }

  //#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
  #ifdef __USE_OLD_INTERFACE
  this->m_Network->SetNumOfLayers(num_layers);
  #endif

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Layer_Id", MET_UINT, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NumNodes", MET_UINT, true);
  this->m_Fields.push_back(mF);
  //int num_nodes = (int)mF->value[0];

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "LayerType", MET_STRING, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "TransferFunction", MET_STRING, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "InputFunction", MET_STRING, true);
  mF->terminateRead = true;
  this->m_Fields.push_back(mF);

  // define the layers
  for ( unsigned int i = 0; i < num_layers; i++ )
    {
    if ( !MET_Read(this->m_InputFile, &this->m_Fields, '=') )
      {
      itkExceptionMacro("MetaObject: Read: MET_Read Failed");
      }

    mF = MET_GetFieldRecord("LayerType", &this->m_Fields);
    if ( !strcmp( (char *)mF->value, "BackPropagationLayer" ) )
      {
      BackPropagationLayerPointer layerptr = BackPropagationLayerType::New();
      layerptr->SetBias(1.0);

      mF = MET_GetFieldRecord("Layer_Id", &this->m_Fields);
      layerptr->SetLayerId( (int)mF->value[0] );

      mF = MET_GetFieldRecord("NumNodes", &this->m_Fields);
      layerptr->SetNumberOfNodes( (int)mF->value[0] );
      //          typename TNetwork::LayerInterfaceType::Pointer layer =
      //            dynamic_cast<typename TNetwork::LayerInterfaceType *>(
      // layerptr.GetPointer() );
      this->m_Layers.push_back( layerptr.GetPointer() );

      mF = MET_GetFieldRecord("TransferFunction", &this->m_Fields);
      if ( !strcmp( (char *)mF->value, "IdentityTransferFunction" ) )
        {
        typedef Statistics::IdentityTransferFunction< MeasurementVectorValueType > tfType;
        typename tfType::Pointer tf = tfType::New();
        layerptr->SetTransferFunction(tf);
        }
      else if ( !strcmp( (char *)mF->value, "LogSigmoidTransferFunction" ) )
        {
        typedef Statistics::LogSigmoidTransferFunction< MeasurementVectorValueType > tfType;
        typename tfType::Pointer tf = tfType::New();
        layerptr->SetTransferFunction(tf);
        }
      else if ( !strcmp( (char *)mF->value, "SigmoidTransferFunction" ) )
        {
        typedef Statistics::SigmoidTransferFunction< MeasurementVectorValueType > tfType;
        typename tfType::Pointer tf = tfType::New();
        layerptr->SetTransferFunction(tf);
        }
      else if ( !strcmp( (char *)mF->value, "TanSigmoidTransferFunction" ) )
        {
        std::cout << "Tansigmoid" << std::endl;
        typedef Statistics::TanSigmoidTransferFunction< MeasurementVectorValueType > tfType;
        typename tfType::Pointer tf = tfType::New();
        layerptr->SetTransferFunction(tf);
        }
      else if ( !strcmp( (char *)mF->value, "SymmetricSigmoidTransferFunction" ) )
        {
        std::cout << "SymmetricSigmoidTransferFunction" << std::endl;
        typedef Statistics::SymmetricSigmoidTransferFunction< MeasurementVectorValueType > tfType;
        typename tfType::Pointer tf = tfType::New();
        layerptr->SetTransferFunction(tf);
        }
      else if ( !strcmp( (char *)mF->value, "ITK_NULLPTR" ) )
        {
        std::cout << "ITK_NULLPTR" << std::endl;
        layerptr->SetTransferFunction(ITK_NULLPTR);
        }

      mF = MET_GetFieldRecord("InputFunction", &this->m_Fields);
      if ( !strcmp( (char *)( mF->value ), "SumInputFunction" ) )
        {
        std::cout << "SumInputFunction" << std::endl;
        typedef Statistics::SumInputFunction
        < MeasurementVectorValueType *, MeasurementVectorValueType >
        ifType;
        typename  ifType::Pointer ifcn = ifType::New();
        layerptr->SetNodeInputFunction(ifcn);
        }
      else if ( !strcmp( (char *)( mF->value ), "ITK_NULLPTR" ) )
        {
        std::cout << "ITK_NULLPTR" << std::endl;
        layerptr->SetNodeInputFunction(ITK_NULLPTR);
        }
      this->m_Network->AddLayer(layerptr);
      }
    }

  this->ClearFields();
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "WeightSet_Id", MET_UINT, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "WeightSetType", MET_STRING, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "SRC_Layer", MET_UINT, true);
  this->m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "DEST_Layer", MET_UINT, true);
  mF->terminateRead = true;
  this->m_Fields.push_back(mF);

  // define the weightsets
  for ( unsigned int i = 0; i < num_weights; i++ )
    {
    if ( !MET_Read(this->m_InputFile, &this->m_Fields, '=') )
      {
      itkExceptionMacro("MetaObject: Read: MET_Read Failed");
      }
    mF = MET_GetFieldRecord("WeightSetType", &this->m_Fields);

    if ( !strcmp( (char *)( mF->value ), "CompletelyConnectedWeightSet" ) )
      {
      mF = MET_GetFieldRecord("WeightSet_Id", &this->m_Fields);
      unsigned int weightsetid = (unsigned int)mF->value[0];
      mF = MET_GetFieldRecord("SRC_Layer", &this->m_Fields);
      unsigned int slayer = (unsigned int)mF->value[0];
      mF = MET_GetFieldRecord("DEST_Layer", &this->m_Fields);
      unsigned int dlayer = (unsigned int)mF->value[0];

      WeightSetPointer weightset;

      // Create a local scope
        {
        typename Statistics::CompletelyConnectedWeightSet< MeasurementVectorType, TargetVectorType >::Pointer
        w = Statistics::CompletelyConnectedWeightSet< MeasurementVectorType, TargetVectorType >::New();
        w->SetWeightSetId(weightsetid);
        w->SetNumberOfInputNodes( this->m_Layers[slayer]->GetNumberOfNodes() );
        w->SetNumberOfOutputNodes( this->m_Layers[dlayer]->GetNumberOfNodes() );
        w->SetCompleteConnectivity();
        w->SetRange(1.0);
        w->Initialize();
        weightset =
          dynamic_cast< WeightSetType * >( w.GetPointer() );
        }

      // Create a local scope
        {
        //Network Intialize will add the weight sets!
        this->m_Network->AddWeightSet(weightset);
        this->m_Weights.push_back(weightset);
        this->m_Layers[slayer]->SetOutputWeightSet(weightset);
        this->m_Layers[dlayer]->SetInputWeightSet(weightset);
        }
      }
    else
      {
      itkExceptionMacro("Unsupportd WeightSetType");
      }
    }

  //Read Weight Values
  if ( this->m_ReadWeightValuesType > 0 )
    {
    //Network should be constructed already, no need to initialize! the
    // itkMultiLayerNetworkBase does no have an initialize!
    // this->m_Network->Initialize();
    for ( int j = 0; j < this->m_Network->GetNumOfWeightSets(); j++ )
      {
      this->ClearFields();
      typename Statistics::WeightSetBase< MeasurementVectorType,
                                          TargetVectorType >::Pointer
      weightset =  this->m_Network->GetWeightSet(j);
      const unsigned int rows = weightset->GetNumberOfOutputNodes();
      const unsigned int cols = weightset->GetNumberOfInputNodes();

      mF = new MET_FieldRecordType;
      MET_InitReadField(
        mF, "WeightValues", MET_FLOAT_ARRAY, true, -1, rows * cols);
      mF->required = true;
      mF->terminateRead = true;
      this->m_Fields.push_back(mF);

      if ( this->m_ReadWeightValuesType == ASCII ) // Read ASCII weights
        {
        if ( !MET_Read(this->m_InputFile, &this->m_Fields, '=') )
          {
          itkExceptionMacro("MET_Read Failed Weight Values missing");
          return;
          }
        weightset->SetWeightValues(mF->value);
        }
      else if ( this->m_ReadWeightValuesType == BINARY ) // Read Binary Weights
        {
        vnl_matrix< MeasurementVectorValueType > WeightMatrix;
        WeightMatrix.set_size(rows, cols);

        this->m_InputFile.read(
          (char *)WeightMatrix.data_block(), rows * cols * sizeof( double ) );
        //
        // TODO: This is hardcoded to double in the writer
        // Should that be the case, and will anyone use a single precision NN?
        if ( this->m_BinaryDataByteOrderMSB != MET_SystemByteOrderMSB() )
          {
          char *data = (char *)WeightMatrix.data_block();
          for ( unsigned i = 0; i < rows * cols; i++ )
            {
            MET_ByteOrderSwap8(data);
            data += 8;
            }
          }
        std::cout << "WeightValues = " << WeightMatrix << std::endl;
        weightset->SetWeightValues( WeightMatrix.data_block() );
        }
      }
    }
  this->m_InputFile.close();
}
} // namespace itk

#endif
