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
#ifndef __itkDCMTKTransformIO_hxx
#define __itkDCMTKTransformIO_hxx

#include "itkDCMTKTransformIO.h"

#include "itkAffineTransform.h"

#include <dcmtk/dcmdata/dcfilefo.h>
#include <dcmtk/dcmdata/dcdeftag.h>
#include <dcmtk/dcmdata/dcdatset.h>
#include <dcmtk/dcmdata/dcuid.h>

namespace itk
{

template <typename TInternalComputationValueType>
DCMTKTransformIO<TInternalComputationValueType>::DCMTKTransformIO()
{}


template <typename TInternalComputationValueType>
DCMTKTransformIO<TInternalComputationValueType>::~DCMTKTransformIO()
{}


template <typename TInternalComputationValueType>
bool
DCMTKTransformIO<TInternalComputationValueType>::CanReadFile(const char * fileName)
{
  DcmFileFormat     fileFormat;
  const OFCondition result = fileFormat.loadFile(fileName, EXS_Unknown);
  if (!result.good())
  {
    return false;
  }

  DcmDataset * dataset = fileFormat.getDataset();
  OFString     sopClass;
  if (!dataset->findAndGetOFString(DCM_SOPClassUID, sopClass).good() || sopClass.empty())
  {
    return false;
  }

  OFString seriesNumber;
  if (sopClass == UID_SpatialRegistrationStorage)
  {
    return true;
  }

  return false;
}


template <typename TInternalComputationValueType>
bool
DCMTKTransformIO<TInternalComputationValueType>::CanWriteFile(const char * itkNotUsed(fileName))
{
  // Write to file has not yet been implemented.
  return false;
}


template <typename TInternalComputationValueType>
void
DCMTKTransformIO<TInternalComputationValueType>::Read()
{
  DcmFileFormat fileFormat;
  OFCondition   result = fileFormat.loadFile(this->GetFileName(), EXS_Unknown);
  if (!result.good())
  {
    itkExceptionMacro("Could not load transform file: " << this->GetFileName());
  }

  DcmDataset *         dataset = fileFormat.getDataset();
  DcmSequenceOfItems * registrationSequence = ITK_NULLPTR;
  result = dataset->findAndGetSequence(DCM_RegistrationSequence, registrationSequence);
  if (result.good())
  {
    const unsigned long numOfRegistrationSequenceItems = registrationSequence->card();
    for (unsigned long ii = 0; ii < numOfRegistrationSequenceItems; ++ii)
    {
      DcmItem * currentRegistrationSequenceItem = registrationSequence->getItem(ii);

      if (currentRegistrationSequenceItem->isEmpty())
      {
        itkDebugMacro("Empty RegistrationSequenceItem in transform file.");
        break;
      }
      DcmSequenceOfItems * matrixRegistrationSequence = ITK_NULLPTR;
      result =
        currentRegistrationSequenceItem->findAndGetSequence(DCM_MatrixRegistrationSequence, matrixRegistrationSequence);
      if (result.good())
      {
        const unsigned long numOfMatrixRegistrationSequenceItems = matrixRegistrationSequence->card();
        for (unsigned long matrixRegistrationSequenceIndex = 0;
             matrixRegistrationSequenceIndex < numOfMatrixRegistrationSequenceItems;
             ++matrixRegistrationSequenceIndex)
        {
          DcmItem * currentmatrixRegistrationSequenceItem =
            matrixRegistrationSequence->getItem(matrixRegistrationSequenceIndex);
          if (currentmatrixRegistrationSequenceItem->isEmpty())
          {
            itkDebugMacro("Empty MatrixRegistrationSequenceItem in transform file.");
            break;
          }
          DcmSequenceOfItems * matrixSequence = NULL;
          result = currentmatrixRegistrationSequenceItem->findAndGetSequence(DCM_MatrixSequence, matrixSequence);
          if (result.good())
          {
            const unsigned long numOfMatrixSequenceItems = matrixSequence->card();
            for (unsigned long matrixSequenceItemIndex = 0; matrixSequenceItemIndex < numOfMatrixSequenceItems;
                 ++matrixSequenceItemIndex)
            {
              DcmItem * currentMatrixSequenceItem = matrixSequence->getItem(matrixSequenceItemIndex);
              if (currentMatrixSequenceItem->isEmpty())
              {
                itkDebugMacro("Empty MatrixSequence in transform file.");
                break;
              }
              OFString matrixType;
              result =
                currentMatrixSequenceItem->findAndGetOFString(DCM_FrameOfReferenceTransformationMatrixType, matrixType);
              if (result.good())
              {
                // TODO switch transform type based on this
                std::cout << "MATRIX TYPE: " << matrixType << std::endl;
              }
              const unsigned int                                                     Dimension = 3;
              typedef itk::AffineTransform<TInternalComputationValueType, Dimension> AffineTransformType;
              typename AffineTransformType::Pointer        affineTransform = AffineTransformType::New();
              typename AffineTransformType::ParametersType transformParameters(Dimension * Dimension + Dimension);
              OFString                                     matrixString;
              unsigned long                                matrixEntryIndex = 0;
              for (unsigned int row = 0; row < Dimension; ++row)
              {
                for (unsigned int col = 0; col < Dimension; ++col)
                {
                  result = currentMatrixSequenceItem->findAndGetOFString(
                    DCM_FrameOfReferenceTransformationMatrix, matrixString, matrixEntryIndex);
                  if (!result.good())
                  {
                    itkExceptionMacro("Could not get expected matrix entry.");
                  }
                  ++matrixEntryIndex;
                  transformParameters[row * Dimension + col] = atof(matrixString.c_str());
                }
                result = currentMatrixSequenceItem->findAndGetOFString(
                  DCM_FrameOfReferenceTransformationMatrix, matrixString, matrixEntryIndex);
                if (!result.good())
                {
                  itkExceptionMacro("Could not get expected matrix entry.");
                }
                ++matrixEntryIndex;
                transformParameters[Dimension * Dimension + row] = atof(matrixString.c_str());
              }
              affineTransform->SetParameters(transformParameters);
              std::cout << affineTransform << std::endl;
            }
          }
        }
      }
    }
  }
  else
  {
    itkExceptionMacro("Could not find RegistrationSequence");
  }
}


template <typename TInternalComputationValueType>
void
DCMTKTransformIO<TInternalComputationValueType>::Write()
{
  itkExceptionMacro("Write to file has not yet been implemented.");
}

} // end namespace itk

#endif
