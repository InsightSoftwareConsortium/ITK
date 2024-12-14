/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "vnl/vnl_vector_fixed.h"
#include "itkTestingMacros.h"

int
itkPhilipsRECImageIOPrintTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage" << '\n';
    return EXIT_FAILURE;
  }

  using PhilipsRECImageIOType = itk::PhilipsRECImageIO;

  auto imageIO = PhilipsRECImageIOType::New();

  if (!imageIO->CanReadFile(argv[1]))
  {
    std::cerr << "Cannot read file " << argv[1] << '\n';
    return EXIT_FAILURE;
  }
  imageIO->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->ReadImageInformation());


  // Print all of the PAR parameters.
  // Return EXIT_FAILURE if the value cannot be read.
  std::string tempStr = "";
  int         tempInt = -1;
  float       tempFloat = 0;

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_Version", tempStr))
  {
    std::cerr << "Cannot read PAR_Version" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_Version = " << tempStr << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_SliceOrientation", tempInt))
  {
    std::cerr << "Cannot read PAR_SliceOrientation" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_SliceOrientation = " << tempInt << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_ExaminationName", tempStr))
  {
    std::cerr << "Cannot read PAR_ExaminationName" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ExaminationName = " << tempStr << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_ProtocolName", tempStr))
  {
    std::cerr << "Cannot read PAR_ProtocolName" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ProtocolName = " << tempStr << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_SeriesType", tempStr))
  {
    std::cerr << "Cannot read PAR_SeriesType" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_SeriesType = " << tempStr << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_AcquisitionNr", tempInt))
  {
    std::cerr << "Cannot read PAR_AcquisitionNr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_AcquisitionNr = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_ReconstructionNr", tempInt))
  {
    std::cerr << "Cannot read PAR_ReconstructionNr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ReconstructionNr = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_ScanDuration", tempInt))
  {
    std::cerr << "Cannot read PAR_ScanDuration" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ScanDuration = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxNumberOfCardiacPhases", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxNumberOfCardiacPhases" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxNumberOfCardiacPhases = " << tempInt << '\n';

  PhilipsRECImageIOType::TriggerTimesContainerType::Pointer ptrToTimePoints = nullptr;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::TriggerTimesContainerType::Pointer>(
        imageIO->GetMetaDataDictionary(), "PAR_TriggerTimes", ptrToTimePoints))
  {
    std::cerr << "Cannot read PAR_TriggerTimes" << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToTimePoints)
  {
    std::cerr << "PAR_TriggerTimes is nullptr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "TriggerTimes =";
  for (PhilipsRECImageIOType::TriggerTimesContainerType::ElementIdentifier iter = 0; iter < ptrToTimePoints->Size();
       iter++)
  {
    std::cout << ' ' << ptrToTimePoints->ElementAt(iter);
  }
  std::cout << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxNumberOfEchoes", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxNumberOfEchoes" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxNumberOfEchoes = " << tempInt << '\n';

  PhilipsRECImageIOType::EchoTimesContainerType::Pointer ptrToEchoes = nullptr;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::EchoTimesContainerType::Pointer>(
        imageIO->GetMetaDataDictionary(), "PAR_EchoTimes", ptrToEchoes))
  {
    std::cerr << "Cannot read PAR_EchoTimes" << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToEchoes)
  {
    std::cerr << "PAR_EchoTimes is nullptr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "EchoTimes =";
  for (PhilipsRECImageIOType::EchoTimesContainerType::ElementIdentifier iter = 0; iter < ptrToEchoes->Size(); ++iter)
  {
    std::cout << ' ' << ptrToEchoes->ElementAt(iter);
  }
  std::cout << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxNumberOfDynamics", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxNumberOfDynamics" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxNumberOfDynamics = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxNumberOfMixes", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxNumberOfMixes" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxNumberOfMixes = " << tempInt << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_PatientPosition", tempStr))
  {
    std::cerr << "Cannot read PAR_PatientPosition" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_PatientPosition = " << tempStr << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_PreparationDirection", tempStr))
  {
    std::cerr << "Cannot read PAR_PreparationDirection" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_PreparationDirection = " << tempStr << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_Technique", tempStr))
  {
    std::cerr << "Cannot read PAR_Technique" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_Technique = " << tempStr << '\n';

  if (!itk::ExposeMetaData<std::string>(imageIO->GetMetaDataDictionary(), "PAR_ScanMode", tempStr))
  {
    std::cerr << "Cannot read PAR_ScanMode" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ScanMode = " << tempStr << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_NumberOfAverages", tempInt))
  {
    std::cerr << "Cannot read PAR_NumberOfAverages" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_NumberOfAverages = " << tempInt << '\n';

  PhilipsRECImageIOType::ScanResolutionType scanRes;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::ScanResolutionType>(
        imageIO->GetMetaDataDictionary(), "PAR_ScanResolution", scanRes))
  {
    std::cerr << "Cannot read PAR_ScanResolution" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ScanResolution = " << scanRes << '\n';

  PhilipsRECImageIOType::RepetitionTimesContainerType::Pointer ptrToTR = nullptr;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::RepetitionTimesContainerType::Pointer>(
        imageIO->GetMetaDataDictionary(), "PAR_RepetitionTimes", ptrToTR))
  {
    std::cerr << "Cannot read PAR_RepetitionTimes" << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToTR)
  {
    std::cerr << "PAR_RepetitionTimes is nullptr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "RepetitionTimes =";
  for (PhilipsRECImageIOType::RepetitionTimesContainerType::ElementIdentifier iter = 0; iter < ptrToTR->Size(); ++iter)
  {
    std::cout << ' ' << ptrToTR->ElementAt(iter);
  }
  std::cout << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_ScanPercentage", tempInt))
  {
    std::cerr << "Cannot read PAR_ScanPercentage" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ScanPercentage = " << tempInt << '\n';

  PhilipsRECImageIOType::FOVType parFOV;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::FOVType>(imageIO->GetMetaDataDictionary(), "PAR_FOV", parFOV))
  {
    std::cerr << "Cannot read PAR_FOV" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_FOV = " << parFOV << '\n';

  if (!itk::ExposeMetaData<float>(imageIO->GetMetaDataDictionary(), "PAR_WaterFatShiftPixels", tempFloat))
  {
    std::cerr << "Cannot read PAR_WaterFatShiftPixels" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_WaterFatShiftPixels = " << tempFloat << '\n';

  PhilipsRECImageIOType::AngulationMidSliceType angMidSlice;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::AngulationMidSliceType>(
        imageIO->GetMetaDataDictionary(), "PAR_AngulationMidSlice", angMidSlice))
  {
    std::cerr << "Cannot read PAR_AngulationMidSlice" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_AngulationMidSlice = " << angMidSlice << '\n';

  PhilipsRECImageIOType::OffCentreMidSliceType offMidSlice;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::OffCentreMidSliceType>(
        imageIO->GetMetaDataDictionary(), "PAR_OffCentreMidSlice", offMidSlice))
  {
    std::cerr << "Cannot read PAR_OffCentreMidSlice" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_OffCentreMidSlice = " << offMidSlice << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_FlowCompensation", tempInt))
  {
    std::cerr << "Cannot read PAR_FlowCompensation" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_FlowCompensation = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_Presaturation", tempInt))
  {
    std::cerr << "Cannot read PAR_Presaturation" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_Presaturation = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_CardiacFrequency", tempInt))
  {
    std::cerr << "Cannot read PAR_CardiacFrequency" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_CardiacFrequency = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MinRRInterval", tempInt))
  {
    std::cerr << "Cannot read PAR_MinRRInterval" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MinRRInterval = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxRRInterval", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxRRInterval" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxRRInterval = " << tempInt << '\n';

  PhilipsRECImageIOType::PhaseEncodingVelocityType phaseEncodeVel;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::PhaseEncodingVelocityType>(
        imageIO->GetMetaDataDictionary(), "PAR_PhaseEncodingVelocity", phaseEncodeVel))
  {
    std::cerr << "Cannot read PAR_PhaseEncodingVelocity" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_PhaseEncodingVelocity = " << phaseEncodeVel << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MTC", tempInt))
  {
    std::cerr << "Cannot read PAR_MTC" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MTC = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_SPIR", tempInt))
  {
    std::cerr << "Cannot read PAR_SPIR" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_SPIR = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_EPIFactor", tempInt))
  {
    std::cerr << "Cannot read PAR_EPIFactor" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_EPIFactor = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_TurboFactor", tempInt))
  {
    std::cerr << "Cannot read PAR_TurboFactor" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_TurboFactor = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_DynamicScan", tempInt))
  {
    std::cerr << "Cannot read PAR_DynamicScan" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_DynamicScan = " << tempInt << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_Diffusion", tempInt))
  {
    std::cerr << "Cannot read PAR_Diffusion" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_Diffusion = " << tempInt << '\n';

  if (!itk::ExposeMetaData<float>(imageIO->GetMetaDataDictionary(), "PAR_DiffusionEchoTime", tempFloat))
  {
    std::cerr << "Cannot read PAR_DiffusionEchoTime" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_DiffusionEchoTime = " << tempFloat << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxNumberOfDiffusionValues", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxNumberOfDiffusionValues" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxNumberOfDiffusionValues = " << tempInt << '\n';

  PhilipsRECImageIOType::GradientBvalueContainerType::Pointer ptrToBValues = nullptr;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::GradientBvalueContainerType::Pointer>(
        imageIO->GetMetaDataDictionary(), "PAR_GradientBValues", ptrToBValues))
  {
    std::cerr << "Cannot read PAR_GradientBValues" << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToBValues)
  {
    std::cerr << "PAR_GradientBValues is nullptr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "GradientBValues =";
  for (PhilipsRECImageIOType::GradientBvalueContainerType::ElementIdentifier iter = 0; iter < ptrToBValues->Size();
       iter++)
  {
    std::cout << ' ' << ptrToBValues->ElementAt(iter);
  }
  std::cout << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_MaxNumberOfGradientOrients", tempInt))
  {
    std::cerr << "Cannot read PAR_MaxNumberOfGradientOrients" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_MaxNumberOfGradientOrients = " << tempInt << '\n';

  PhilipsRECImageIOType::GradientDirectionContainerType::Pointer ptrToGradValues = nullptr;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::GradientDirectionContainerType::Pointer>(
        imageIO->GetMetaDataDictionary(), "PAR_GradientDirectionValues", ptrToGradValues))
  {
    std::cerr << "Cannot read PAR_GradientDirectionValues" << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToGradValues)
  {
    std::cerr << "PAR_GradientDirectionValues is nullptr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "GradientDirectionValues =";
  for (PhilipsRECImageIOType::GradientDirectionContainerType::ElementIdentifier iter = 0;
       iter < ptrToGradValues->Size();
       iter++)
  {
    std::cout << ' ' << ptrToGradValues->ElementAt(iter);
  }
  std::cout << '\n';

  if (!itk::ExposeMetaData<float>(imageIO->GetMetaDataDictionary(), "PAR_InversionDelay", tempFloat))
  {
    std::cerr << "Cannot read PAR_InversionDelay" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_InversionDelay = " << tempFloat << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_NumberOfImageTypes", tempInt))
  {
    std::cerr << "Cannot read PAR_NumberOfImageTypes" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_NumberOfImageTypes = " << tempInt << '\n';

  PhilipsRECImageIOType::ImageTypesType imageTypes;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::ImageTypesType>(
        imageIO->GetMetaDataDictionary(), "PAR_ImageTypes", imageTypes))
  {
    std::cerr << "Cannot read PAR_ImageTypes" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ImageTypes = " << imageTypes << '\n';

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_NumberOfScanningSequences", tempInt))
  {
    std::cerr << "Cannot read PAR_NumberOfScanningSequences" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_NumberOfScanningSequences = " << tempInt << '\n';

  PhilipsRECImageIOType::ScanningSequencesType scanningSeq;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::ScanningSequencesType>(
        imageIO->GetMetaDataDictionary(), "PAR_ScanningSequences", scanningSeq))
  {
    std::cerr << "Cannot read PAR_ScanningSequences" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_ScanningSequences = " << scanningSeq << '\n';

  using ScanningSequenceImageTypeRescaleValuesContainerTypePtr =
    PhilipsRECImageIOType::ScanningSequenceImageTypeRescaleValuesContainerType::Pointer;

  ScanningSequenceImageTypeRescaleValuesContainerTypePtr ptrToRescaleValues = nullptr;
  if (!itk::ExposeMetaData<ScanningSequenceImageTypeRescaleValuesContainerTypePtr>(
        imageIO->GetMetaDataDictionary(), "PAR_ScanningSequenceImageTypeRescaleValues", ptrToRescaleValues))
  {
    std::cerr << "Cannot read PAR_ScanningSequenceImageTypeRescaleValues";
    std::cerr << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToRescaleValues)
  {
    std::cerr << "PAR_ScanningSequenceImageTypeRescaleValues is nullptr";
    std::cerr << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "ScanningSequenceImageTypeRescaleValues = " << '\n';
  for (PhilipsRECImageIOType::ScanningSequenceImageTypeRescaleValuesContainerType::ElementIdentifier iter = 0;
       iter < ptrToRescaleValues->Size();
       iter++)
  {
    std::cout << "Scanning Sequence " << iter << " =";
    const PhilipsRECImageIOType::ImageTypeRescaleValuesContainerType::Pointer rescaleValueVector =
      ptrToRescaleValues->ElementAt(iter);
    for (PhilipsRECImageIOType::ImageTypeRescaleValuesContainerType::ElementIdentifier iter1 = 0;
         iter1 < rescaleValueVector->Size();
         iter1++)
    {
      std::cout << "  " << rescaleValueVector->ElementAt(iter1);
    }
    std::cout << '\n';
  }

  if (!itk::ExposeMetaData<int>(imageIO->GetMetaDataDictionary(), "PAR_NumberOfASLLabelTypes", tempInt))
  {
    std::cerr << "Cannot read PAR_NumberOfASLLabelTypes" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "PAR_NumberOfASLLabelTypes = " << tempInt << '\n';

  PhilipsRECImageIOType::LabelTypesASLContainerType::Pointer ptrToASLLabelTypes = nullptr;
  if (!itk::ExposeMetaData<PhilipsRECImageIOType::LabelTypesASLContainerType::Pointer>(
        imageIO->GetMetaDataDictionary(), "PAR_ASLLabelTypes", ptrToASLLabelTypes))
  {
    std::cerr << "Cannot read PAR_ASLLabelTypes" << '\n';
    return EXIT_FAILURE;
  }
  if (!ptrToASLLabelTypes)
  {
    std::cerr << "PAR_ASLLabelTypes is nullptr" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "ASLLabelTypes =";
  for (PhilipsRECImageIOType::LabelTypesASLContainerType::ElementIdentifier iter = 0; iter < ptrToASLLabelTypes->Size();
       iter++)
  {
    std::cout << ' ' << ptrToASLLabelTypes->ElementAt(iter);
  }
  std::cout << '\n';


  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
