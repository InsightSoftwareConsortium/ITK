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

#include "itkPhilipsRECImageIO.h"
#include "itkMetaDataObject.h"

#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL < 20160229
#include "vnl/vnl_matrix_fixed.txx" // Get the templates
#else
#include "vnl/vnl_vector_fixed.hxx" // Get the templates
#endif
VNL_VECTOR_FIXED_INSTANTIATE(int,8);

int itkPhilipsRECImageIOPrintTest( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " InputImage" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::PhilipsRECImageIO  PhilipsRECImageIOType;

  PhilipsRECImageIOType::Pointer imageIO = PhilipsRECImageIOType::New();

  if( !imageIO->CanReadFile( argv[1] ) )
    {
    std::cerr << "Cannot read file " << argv[1] << std::endl;
    return EXIT_FAILURE;
    }
  imageIO->SetFileName(argv[1]);

  try
    {
    imageIO->ReadImageInformation();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Print all of the PAR parameters.
  // Return EXIT_FAILURE if the value cannot be read.
  std::string tempStr = "";
  int         tempInt = -1;
  float       tempFloat = 0;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_Version",tempStr) )
    {
    std::cerr << "Cannot read PAR_Version" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_Version = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_SliceOrientation",tempInt) )
    {
    std::cerr << "Cannot read PAR_SliceOrientation" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_SliceOrientation = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_ExaminationName",tempStr) )
    {
    std::cerr << "Cannot read PAR_ExaminationName" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ExaminationName = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_ProtocolName",tempStr) )
    {
    std::cerr << "Cannot read PAR_ProtocolName" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ProtocolName = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_SeriesType",tempStr) )
    {
    std::cerr << "Cannot read PAR_SeriesType" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_SeriesType = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_AcquisitionNr",tempInt) )
    {
    std::cerr << "Cannot read PAR_AcquisitionNr" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_AcquisitionNr = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_ReconstructionNr",tempInt) )
    {
    std::cerr << "Cannot read PAR_ReconstructionNr" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ReconstructionNr = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_ScanDuration",tempInt) )
    {
    std::cerr << "Cannot read PAR_ScanDuration" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ScanDuration = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxNumberOfCardiacPhases",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxNumberOfCardiacPhases" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxNumberOfCardiacPhases = " << tempInt << std::endl;

  PhilipsRECImageIOType::TriggerTimesContainerType::Pointer
    ptrToTimePoints = ITK_NULLPTR;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::TriggerTimesContainerType
    ::Pointer>(imageIO->GetMetaDataDictionary(), "PAR_TriggerTimes",
    ptrToTimePoints) )
    {
    std::cerr << "Cannot read PAR_TriggerTimes" << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToTimePoints )
    {
    std::cerr << "PAR_TriggerTimes is ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "TriggerTimes =";
  for( PhilipsRECImageIOType::TriggerTimesContainerType::ElementIdentifier iter = 0;
    iter < ptrToTimePoints->Size(); iter++ )
     {
     std::cout << " " << ptrToTimePoints->ElementAt(iter);
     }
  std::cout << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxNumberOfEchoes",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxNumberOfEchoes" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxNumberOfEchoes = " << tempInt << std::endl;

  PhilipsRECImageIOType::EchoTimesContainerType::Pointer
    ptrToEchoes = ITK_NULLPTR;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::EchoTimesContainerType
    ::Pointer>(imageIO->GetMetaDataDictionary(), "PAR_EchoTimes",
    ptrToEchoes) )
    {
    std::cerr << "Cannot read PAR_EchoTimes" << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToEchoes )
    {
    std::cerr << "PAR_EchoTimes is ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "EchoTimes =";
  for( PhilipsRECImageIOType::EchoTimesContainerType::ElementIdentifier iter = 0;
    iter < ptrToEchoes->Size(); iter++ )
    {
    std::cout << " " << ptrToEchoes->ElementAt(iter);
    }
  std::cout << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxNumberOfDynamics",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxNumberOfDynamics" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxNumberOfDynamics = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxNumberOfMixes",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxNumberOfMixes" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxNumberOfMixes = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_PatientPosition",tempStr) )
    {
    std::cerr << "Cannot read PAR_PatientPosition" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_PatientPosition = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_PreparationDirection",tempStr) )
    {
    std::cerr << "Cannot read PAR_PreparationDirection" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_PreparationDirection = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_Technique",tempStr) )
    {
    std::cerr << "Cannot read PAR_Technique" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_Technique = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(),
    "PAR_ScanMode",tempStr) )
    {
    std::cerr << "Cannot read PAR_ScanMode" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ScanMode = " << tempStr << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_NumberOfAverages",tempInt) )
    {
    std::cerr << "Cannot read PAR_NumberOfAverages" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_NumberOfAverages = " << tempInt << std::endl;

  PhilipsRECImageIOType::ScanResolutionType scanRes;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::ScanResolutionType>
    (imageIO->GetMetaDataDictionary(),"PAR_ScanResolution",scanRes) )
    {
    std::cerr << "Cannot read PAR_ScanResolution" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ScanResolution = " << scanRes << std::endl;

  PhilipsRECImageIOType::RepetitionTimesContainerType::Pointer
    ptrToTR = ITK_NULLPTR;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::RepetitionTimesContainerType
    ::Pointer>(imageIO->GetMetaDataDictionary(), "PAR_RepetitionTimes",
    ptrToTR) )
    {
    std::cerr << "Cannot read PAR_RepetitionTimes" << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToTR )
    {
    std::cerr << "PAR_RepetitionTimes is ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "RepetitionTimes =";
  for( PhilipsRECImageIOType::RepetitionTimesContainerType::ElementIdentifier iter = 0;
    iter < ptrToTR->Size(); iter++ )
    {
    std::cout << " " << ptrToTR->ElementAt(iter);
    }
  std::cout << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_ScanPercentage",tempInt) )
    {
    std::cerr << "Cannot read PAR_ScanPercentage" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ScanPercentage = " << tempInt << std::endl;

  PhilipsRECImageIOType::FOVType parFOV;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::FOVType>
    (imageIO->GetMetaDataDictionary(),"PAR_FOV",parFOV) )
    {
    std::cerr << "Cannot read PAR_FOV" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_FOV = " << parFOV << std::endl;

  if( !itk::ExposeMetaData<float>(imageIO->GetMetaDataDictionary(),
    "PAR_WaterFatShiftPixels",tempFloat) )
    {
    std::cerr << "Cannot read PAR_WaterFatShiftPixels" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_WaterFatShiftPixels = " << tempFloat << std::endl;

  PhilipsRECImageIOType::AngulationMidSliceType angMidSlice;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::AngulationMidSliceType>
    (imageIO->GetMetaDataDictionary(),"PAR_AngulationMidSlice",angMidSlice) )
    {
    std::cerr << "Cannot read PAR_AngulationMidSlice" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_AngulationMidSlice = " << angMidSlice << std::endl;

  PhilipsRECImageIOType::OffCentreMidSliceType offMidSlice;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::OffCentreMidSliceType>
    (imageIO->GetMetaDataDictionary(),"PAR_OffCentreMidSlice",offMidSlice) )
    {
    std::cerr << "Cannot read PAR_OffCentreMidSlice" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_OffCentreMidSlice = " << offMidSlice << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_FlowCompensation",tempInt) )
    {
    std::cerr << "Cannot read PAR_FlowCompensation" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_FlowCompensation = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_Presaturation",tempInt) )
    {
    std::cerr << "Cannot read PAR_Presaturation" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_Presaturation = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_CardiacFrequency",tempInt) )
    {
    std::cerr << "Cannot read PAR_CardiacFrequency" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_CardiacFrequency = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MinRRInterval",tempInt) )
    {
    std::cerr << "Cannot read PAR_MinRRInterval" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MinRRInterval = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxRRInterval",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxRRInterval" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxRRInterval = " << tempInt << std::endl;

  PhilipsRECImageIOType::PhaseEncodingVelocityType phaseEncodeVel;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::PhaseEncodingVelocityType>
    (imageIO->GetMetaDataDictionary(),"PAR_PhaseEncodingVelocity",
    phaseEncodeVel) )
    {
    std::cerr << "Cannot read PAR_PhaseEncodingVelocity" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_PhaseEncodingVelocity = " << phaseEncodeVel << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MTC",tempInt) )
    {
    std::cerr << "Cannot read PAR_MTC" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MTC = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_SPIR",tempInt) )
    {
    std::cerr << "Cannot read PAR_SPIR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_SPIR = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_EPIFactor",tempInt) )
    {
    std::cerr << "Cannot read PAR_EPIFactor" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_EPIFactor = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_TurboFactor",tempInt) )
    {
    std::cerr << "Cannot read PAR_TurboFactor" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_TurboFactor = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_DynamicScan",tempInt) )
    {
    std::cerr << "Cannot read PAR_DynamicScan" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_DynamicScan = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_Diffusion",tempInt) )
    {
    std::cerr << "Cannot read PAR_Diffusion" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_Diffusion = " << tempInt << std::endl;

  if( !itk::ExposeMetaData<float>(imageIO->GetMetaDataDictionary(),
    "PAR_DiffusionEchoTime",tempFloat) )
    {
    std::cerr << "Cannot read PAR_DiffusionEchoTime" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_DiffusionEchoTime = " << tempFloat << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxNumberOfDiffusionValues",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxNumberOfDiffusionValues" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxNumberOfDiffusionValues = " << tempInt << std::endl;

  PhilipsRECImageIOType::GradientBvalueContainerType::Pointer
    ptrToBValues = ITK_NULLPTR;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::GradientBvalueContainerType
    ::Pointer>(imageIO->GetMetaDataDictionary(), "PAR_GradientBValues",
    ptrToBValues) )
    {
    std::cerr << "Cannot read PAR_GradientBValues" << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToBValues )
    {
    std::cerr << "PAR_GradientBValues is ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "GradientBValues =";
  for( PhilipsRECImageIOType::GradientBvalueContainerType::ElementIdentifier iter = 0;
    iter < ptrToBValues->Size(); iter++ )
    {
    std::cout << " " << ptrToBValues->ElementAt(iter);
    }
  std::cout << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_MaxNumberOfGradientOrients",tempInt) )
    {
    std::cerr << "Cannot read PAR_MaxNumberOfGradientOrients" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_MaxNumberOfGradientOrients = " << tempInt << std::endl;

  PhilipsRECImageIOType::GradientDirectionContainerType::Pointer
    ptrToGradValues = ITK_NULLPTR;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::GradientDirectionContainerType
    ::Pointer>(imageIO->GetMetaDataDictionary(), "PAR_GradientDirectionValues",
    ptrToGradValues) )
    {
    std::cerr << "Cannot read PAR_GradientDirectionValues" << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToGradValues )
    {
    std::cerr << "PAR_GradientDirectionValues is ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "GradientDirectionValues =";
  for( PhilipsRECImageIOType::GradientDirectionContainerType::ElementIdentifier iter = 0;
    iter < ptrToGradValues->Size(); iter++ )
     {
     std::cout << " " << ptrToGradValues->ElementAt(iter);
     }
  std::cout << std::endl;

  if( !itk::ExposeMetaData<float>(imageIO->GetMetaDataDictionary(),
    "PAR_InversionDelay",tempFloat) )
    {
    std::cerr << "Cannot read PAR_InversionDelay" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_InversionDelay = " << tempFloat << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_NumberOfImageTypes",tempInt) )
    {
    std::cerr << "Cannot read PAR_NumberOfImageTypes" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_NumberOfImageTypes = " << tempInt << std::endl;

  PhilipsRECImageIOType::ImageTypesType imageTypes;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::ImageTypesType>
    (imageIO->GetMetaDataDictionary(),"PAR_ImageTypes",
    imageTypes) )
    {
    std::cerr << "Cannot read PAR_ImageTypes" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ImageTypes = " << imageTypes << std::endl;

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_NumberOfScanningSequences",tempInt) )
    {
    std::cerr << "Cannot read PAR_NumberOfScanningSequences" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_NumberOfScanningSequences = " << tempInt << std::endl;

  PhilipsRECImageIOType::ScanningSequencesType scanningSeq;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::ScanningSequencesType>
    (imageIO->GetMetaDataDictionary(),"PAR_ScanningSequences",
    scanningSeq) )
    {
    std::cerr << "Cannot read PAR_ScanningSequences" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_ScanningSequences = " << scanningSeq << std::endl;

  typedef PhilipsRECImageIOType::ScanningSequenceImageTypeRescaleValuesContainerType::Pointer
  ScanningSequenceImageTypeRescaleValuesContainerTypePtr;

  ScanningSequenceImageTypeRescaleValuesContainerTypePtr
    ptrToRescaleValues = ITK_NULLPTR;
  if( !itk::ExposeMetaData<ScanningSequenceImageTypeRescaleValuesContainerTypePtr>
    (imageIO->GetMetaDataDictionary(),
    "PAR_ScanningSequenceImageTypeRescaleValues",
    ptrToRescaleValues) )
    {
    std::cerr << "Cannot read PAR_ScanningSequenceImageTypeRescaleValues";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToRescaleValues )
    {
    std::cerr << "PAR_ScanningSequenceImageTypeRescaleValues is ITK_NULLPTR";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "ScanningSequenceImageTypeRescaleValues = " << std::endl;
  for( PhilipsRECImageIOType::ScanningSequenceImageTypeRescaleValuesContainerType::ElementIdentifier iter = 0;
    iter < ptrToRescaleValues->Size(); iter++ )
    {
    std::cout << "Scanning Sequence " << iter << " =";
    PhilipsRECImageIOType::ImageTypeRescaleValuesContainerType::Pointer
      rescaleValueVector = ptrToRescaleValues->ElementAt(iter);
    for( PhilipsRECImageIOType::ImageTypeRescaleValuesContainerType::ElementIdentifier iter1 = 0;
      iter1 < rescaleValueVector->Size(); iter1++ )
      {
      std::cout << "  " << rescaleValueVector->ElementAt(iter1);
      }
      std::cout << std::endl;
    }

  if( !itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(),
    "PAR_NumberOfASLLabelTypes",tempInt) )
    {
    std::cerr << "Cannot read PAR_NumberOfASLLabelTypes" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "PAR_NumberOfASLLabelTypes = " << tempInt << std::endl;

  PhilipsRECImageIOType::LabelTypesASLContainerType::Pointer
    ptrToASLLabelTypes = ITK_NULLPTR;
  if( !itk::ExposeMetaData<PhilipsRECImageIOType::LabelTypesASLContainerType
    ::Pointer>(imageIO->GetMetaDataDictionary(), "PAR_ASLLabelTypes",
    ptrToASLLabelTypes) )
    {
    std::cerr << "Cannot read PAR_ASLLabelTypes" << std::endl;
    return EXIT_FAILURE;
    }
  if( !ptrToASLLabelTypes )
    {
    std::cerr << "PAR_ASLLabelTypes is ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "ASLLabelTypes =";
  for( PhilipsRECImageIOType::LabelTypesASLContainerType::ElementIdentifier iter = 0;
    iter < ptrToASLLabelTypes->Size(); iter++ )
    {
    std::cout << " " << ptrToASLLabelTypes->ElementAt(iter);
    }
  std::cout << std::endl;

  return EXIT_SUCCESS;
}
