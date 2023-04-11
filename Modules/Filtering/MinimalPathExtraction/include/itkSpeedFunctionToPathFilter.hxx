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
#ifndef itkSpeedFunctionToPathFilter_hxx
#define itkSpeedFunctionToPathFilter_hxx

#include "itkMath.h"
#include "itkFastMarchingUpwindGradientImageFilter.h"


namespace itk
{

template <typename TInputImage, typename TOutputPath>
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::SpeedFunctionToPathFilter()
  : m_CurrentArrivalFunction(nullptr)
{}


template <typename TInputImage, typename TOutputPath>
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::~SpeedFunctionToPathFilter() = default;


template <typename TInputImage, typename TOutputPath>
unsigned int
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::GetNumberOfPathsToExtract() const
{
  return m_Information.size();
}


template <typename TInputImage, typename TOutputPath>
const typename SpeedFunctionToPathFilter<TInputImage, TOutputPath>::PointsContainerType &
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::GetNextEndPoint()
{
  return m_Information[Superclass::m_CurrentOutput]->GetEndPoint();
}


template <typename TInputImage, typename TOutputPath>
typename SpeedFunctionToPathFilter<TInputImage, TOutputPath>::InputImageType *
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::ComputeArrivalFunction()
{
  // Get the speed image
  InputImagePointer speed = const_cast<InputImageType *>(this->GetInput());

  // Set the fast marching method for computing the arrival function
  using FastMarchingType = FastMarchingUpwindGradientImageFilter<TInputImage, TInputImage>;

  using NodeContainer = typename FastMarchingType::NodeContainer;
  using NodeType = typename FastMarchingType::NodeType;
  typename FastMarchingType::Pointer marching = FastMarchingType::New();
  marching->SetInput(speed);
  marching->SetGenerateGradientImage(false);
  marching->SetTargetOffset(2.0 * Superclass::m_TerminationValue);

  // Add next and previous front sources as target points to
  // limit the front propagation to just the required zones
  PointsContainerType PrevFront = m_Information[Superclass::m_CurrentOutput]->PeekPreviousFront();
  PointsContainerType NextFront = m_Information[Superclass::m_CurrentOutput]->PeekNextFront();
  using IndexTypeVec = std::vector<IndexType>;
  IndexTypeVec PrevIndexVec(0);


  typename NodeContainer::Pointer targets = NodeContainer::New();
  targets->Initialize();

  for (auto it = PrevFront.begin(); it != PrevFront.end(); it++)
  {
    NodeType        nodeTargetPrevious;
    const IndexType indexTargetPrevious =
      speed->template TransformPhysicalPointToIndex<typename PointType::ValueType>(*it);
    nodeTargetPrevious.SetValue(0.0);
    nodeTargetPrevious.SetIndex(indexTargetPrevious);
    targets->InsertElement(0, nodeTargetPrevious);
    PrevIndexVec.push_back(indexTargetPrevious);
  }

  for (auto it = NextFront.begin(); it != NextFront.end(); it++)
  {
    NodeType        nodeTargetNext;
    const IndexType indexTargetNext = speed->template TransformPhysicalPointToIndex<typename PointType::ValueType>(*it);
    nodeTargetNext.SetValue(0.0);
    nodeTargetNext.SetIndex(indexTargetNext);
    targets->InsertElement(1, nodeTargetNext);
  }
  marching->SetTargetPoints(targets);

  marching->SetTargetReachedModeToAllTargets();

  // Get the next Front source point and add as trial point
  typename NodeContainer::Pointer trial = NodeContainer::New();
  trial->Initialize();
  PointsContainerType CurrentFront =
    m_Information[Superclass::m_CurrentOutput]->PeekCurrentFront(); // FrontAndAdvance();
  IndexTypeVec CurrentIndexVec(0);

  for (auto it = CurrentFront.begin(); it != CurrentFront.end(); it++)
  {
    NodeType        nodeTrial;
    const IndexType indexTrial = speed->template TransformPhysicalPointToIndex<typename PointType::ValueType>(*it);
    nodeTrial.SetValue(0.0);
    nodeTrial.SetIndex(indexTrial);
    trial->InsertElement(0, nodeTrial);
    CurrentIndexVec.push_back(indexTrial);
  }
  marching->SetTrialPoints(trial);

  // Update the method and set the arrival function
  marching->UpdateLargestPossibleRegion();
  m_CurrentArrivalFunction = marching->GetOutput();
  m_CurrentArrivalFunction->DisconnectPipeline();

  // Only the index with the minimum arrival time should stay in the "Previous" point set
  // This will be used to initialise the optimizer
  if (PrevFront.size() > 1)
  {
    InputImagePixelType MinTime = itk::NumericTraits<InputImagePixelType>::max();
    unsigned            MinPos(0);
    for (unsigned idx = 0; idx < PrevIndexVec.size(); ++idx)
    {
      InputImagePixelType V = m_CurrentArrivalFunction->GetPixel(PrevIndexVec[idx]);
      if (V < MinTime)
      {
        MinPos = idx;
        MinTime = V;
      }
    }
    m_Information[Superclass::m_CurrentOutput]->SetPrevious(PrevFront[MinPos]);
  }

  // Make the arrival function flat inside the seeds, otherwise the
  // optimizer will cross over them. This only matters if the seeds are extended
  if (CurrentIndexVec.size() > 1)
  {
    for (auto vi = CurrentIndexVec.begin(); vi != CurrentIndexVec.end(); vi++)
    {
      m_CurrentArrivalFunction->SetPixel(*vi, 0);
    }
  }

  m_Information[Superclass::m_CurrentOutput]->Advance();
  return m_CurrentArrivalFunction;
}


template <typename TInputImage, typename TOutputPath>
void
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::GenerateData()
{
  // Get the speed function
  InputImagePointer speed = const_cast<InputImageType *>(this->GetInput());
  if (speed.IsNull())
  {
    itkExceptionMacro("Speed function image must be provided");
  }

  // Ensure the user has added at least one path info object
  if (m_Information.empty())
  {
    itkExceptionMacro("No PathInfo objects: at least one must be added.");
  }

  // Extract the path
  Superclass::GenerateData();
}


template <typename TInputImage, typename TOutputPath>
void
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::Execute(const Object *      object,
                                                             const EventObject & itkNotUsed(event))
{
  // Cast object to optmizer
  typename OptimizerType::Pointer optimizer = (OptimizerType *)dynamic_cast<const OptimizerType *>(object);
  if (optimizer.IsNull())
    return;

  // Get current position and value
  typename OptimizerType::ParametersType currentParameters = optimizer->GetCurrentPosition();
  unsigned int                           lenParameters = currentParameters.GetSize();
  if (lenParameters != InputImageDimension)
    return;
  typename OptimizerType::MeasureType currentValue = optimizer->GetValue(currentParameters);

  // Convert parameters to point
  bool         valid = false;
  unsigned int numparams = optimizer->GetCurrentPosition().GetSize();
  PointType    point;
  point.Fill(0.0);
  for (unsigned int i = 0; i < numparams; i++)
  {
    point[i] = optimizer->GetCurrentPosition()[i];
    valid = true;
  }
  if (!valid)
    return;


  // Check if we have reached the termination value
  if (currentValue < Superclass::m_TerminationValue && m_Information[Superclass::m_CurrentOutput]->HasNextFront())
  {
    // We have terminated the current path segment,
    // but there are more fronts to propagate

    // TODO: The path has not actually reached the path point.
    //       Change the next front point to be the current point.
    // only the arrival point reached by the optimizer should be included in
    // the extended point set
    if (m_Information[Superclass::m_CurrentOutput]->PeekPreviousFront().size() > 1)
    {
      m_Information[Superclass::m_CurrentOutput]->SetPrevious(point);
    }
    // Update the arrival function and re-initialise the cost function
    Superclass::m_CostFunction->SetImage(this->ComputeArrivalFunction());
    Superclass::m_CostFunction->Initialize();
  }
  else if (currentValue >= Superclass::m_TerminationValue)
  {
    // Convert point to continuous index
    InputImagePointer         input = const_cast<InputImageType *>(this->GetInput());
    const ContinuousIndexType cindex =
      input->template TransformPhysicalPointToContinuousIndex<typename ContinuousIndexType::ValueType,
                                                              typename PointType::ValueType>(point);

    // Add point as vertex in path
    OutputPathPointer output = this->GetOutput(Superclass::m_CurrentOutput);
    output->AddVertex(cindex);
  }
}


template <typename TInputImage, typename TOutputPath>
void
SpeedFunctionToPathFilter<TInputImage, TOutputPath>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


} // end namespace itk

#endif
