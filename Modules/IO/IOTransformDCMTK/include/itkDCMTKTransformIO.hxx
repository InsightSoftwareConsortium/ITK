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
#ifndef itkDCMTKTransformIO_hxx
#define itkDCMTKTransformIO_hxx

#include "itkDCMTKTransformIO.h"

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkEuler3DTransform.h"
#include "itkScaleTransform.h"
#include "itkVersorTransform.h"

#include <dcmtk/dcmdata/dcfilefo.h>
#include <dcmtk/dcmdata/dcdeftag.h>
#include <dcmtk/dcmdata/dcdatset.h>
#include <dcmtk/dcmdata/dcuid.h>

namespace itk
{

template <typename TInternalComputationValueType>
DCMTKTransformIO<TInternalComputationValueType>::DCMTKTransformIO() = default;


template <typename TInternalComputationValueType>
DCMTKTransformIO<TInternalComputationValueType>::~DCMTKTransformIO() = default;


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
  TransformListType & transformList = this->GetReadTransformList();
  transformList.clear();

  bool desiredFrameOfReferenceFound = false;
  bool haveDesiredFrameOfReference = false;
  if (!this->m_FrameOfReferenceUID.empty())
  {
    haveDesiredFrameOfReference = true;
  }

  constexpr unsigned int Dimension = 3;
  using CompositeTransformType = CompositeTransform<TInternalComputationValueType, Dimension>;
  typename CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  // In TransformFileReader, all the following transforms will be added to
  // this CompositeTransform
  transformList.push_back(compositeTransform.GetPointer());

  DcmFileFormat fileFormat;
  OFCondition   result = fileFormat.loadFile(this->GetFileName(), EXS_Unknown);
  if (!result.good())
  {
    itkExceptionMacro("Could not load transform file: " << this->GetFileName());
  }

  DcmDataset *         dataset = fileFormat.getDataset();
  DcmSequenceOfItems * registrationSequence = nullptr;
  result = dataset->findAndGetSequence(DCM_RegistrationSequence, registrationSequence);
  if (result.good())
  {
    const unsigned long numOfRegistrationSequenceItems = registrationSequence->card();
    for (unsigned long ii = 0; ii < numOfRegistrationSequenceItems; ++ii)
    {
      DcmItem * currentRegistrationSequenceItem = registrationSequence->getItem(ii);

      if (haveDesiredFrameOfReference)
      {
        OFString frameOfReferenceUID;
        result = currentRegistrationSequenceItem->findAndGetOFString(DCM_FrameOfReferenceUID, frameOfReferenceUID);
        if (result.good())
        {
          if (frameOfReferenceUID.compare(m_FrameOfReferenceUID.c_str()) == 0)
          {
            desiredFrameOfReferenceFound = true;
          }
          else
          {
            continue;
          }
        }
        else
        {
          itkExceptionMacro("Could not get FrameOfReferenceUID");
        }
      }

      if (currentRegistrationSequenceItem->isEmpty())
      {
        itkExceptionMacro("Empty RegistrationSequenceItem in transform file.");
      }
      DcmSequenceOfItems * matrixRegistrationSequence = nullptr;
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
            itkExceptionMacro("Empty MatrixRegistrationSequenceItem in transform file.");
            break;
          }
          DcmSequenceOfItems * matrixSequence = nullptr;
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
                itkExceptionMacro("Empty MatrixSequence in transform file.");
                break;
              }

              using AffineTransformType = itk::AffineTransform<TInternalComputationValueType, Dimension>;
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
                  transformParameters[row * Dimension + col] = std::stod(matrixString.c_str());
                }
                result = currentMatrixSequenceItem->findAndGetOFString(
                  DCM_FrameOfReferenceTransformationMatrix, matrixString, matrixEntryIndex);
                if (!result.good())
                {
                  itkExceptionMacro("Could not get expected matrix entry.");
                }
                ++matrixEntryIndex;
                transformParameters[Dimension * Dimension + row] = std::stod(matrixString.c_str());
              }
              affineTransform->SetParameters(transformParameters);

              OFString matrixType;
              result =
                currentMatrixSequenceItem->findAndGetOFString(DCM_FrameOfReferenceTransformationMatrixType, matrixType);
              if (result.good())
              {
                if (matrixType.find("RIGID_SCALE") != std::string::npos)
                {
                  using ScaleTransformType = ScaleTransform<TInternalComputationValueType, Dimension>;
                  typename ScaleTransformType::Pointer scaleTransform = ScaleTransformType::New();
                  scaleTransform->SetMatrix(affineTransform->GetMatrix());
                  scaleTransform->SetOffset(affineTransform->GetOffset());
                  transformList.push_back(scaleTransform.GetPointer());
                }
                else if (matrixType.find("RIGID") != std::string::npos)
                {
                  using RigidTransformType = Euler3DTransform<TInternalComputationValueType>;
                  typename RigidTransformType::Pointer rigidTransform = RigidTransformType::New();
                  rigidTransform->SetMatrix(affineTransform->GetMatrix(), 1e-5);
                  rigidTransform->SetOffset(affineTransform->GetOffset());
                  transformList.push_back(rigidTransform.GetPointer());
                }
                else if (matrixType.find("AFFINE") != std::string::npos)
                {
                  transformList.push_back(affineTransform.GetPointer());
                }
                else
                {
                  itkExceptionMacro("Unknown TransformationMatrixType");
                }
              }
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

  if (haveDesiredFrameOfReference)
  {
    if (!desiredFrameOfReferenceFound)
    {
      itkExceptionMacro(
        "Could not find the requested transform for frame of reference: " << this->m_FrameOfReferenceUID);
    }
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
