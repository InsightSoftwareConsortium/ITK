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
#ifndef itkMultiphaseDenseFiniteDifferenceImageFilter_hxx
#define itkMultiphaseDenseFiniteDifferenceImageFilter_hxx

#include "itkMultiphaseDenseFiniteDifferenceImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "m_ReinitializeCounter: " << m_ReinitializeCounter << std::endl;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::CopyInputToOutput()
{
  OutputImagePointer output = this->GetOutput();

  output->FillBuffer(0);

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    const InputImagePointer input = this->m_LevelSet[i];
    const InputPointType    origin = input->GetOrigin();
    const InputSizeType     size = input->GetBufferedRegion().GetSize();

    // Find the index of the target image where this Level Set
    // should be pasted.
    OutputIndexType start;
    output->TransformPhysicalPointToIndex(origin, start);

    OutputRegionType region;
    region.SetSize(size);
    region.SetIndex(start);

    if ( !input || !output )
      {
      itkExceptionMacro (<< "Either input and/or output is ITK_NULLPTR.");
      }

    ImageRegionConstIterator< InputImageType > in( input, input->GetBufferedRegion() );
    ImageRegionIterator< OutputImageType >     out(output, region);

    // Fill the output pointer
    OutputPixelType p = static_cast< OutputPixelType >( this->m_Lookup[i] );

    in.GoToBegin();
    out.GoToBegin();

    while ( !out.IsAtEnd() )
      {
      if ( in.Get() < 0 )
        {
        out.Value() =  p;
        }
      ++in;
      ++out;
      }
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::AllocateUpdateBuffer()
{
  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    InputImagePointer input = this->m_LevelSet[i];
    InputRegionType   region = input->GetLargestPossibleRegion();

    m_UpdateBuffers[i]->CopyInformation(input);
    m_UpdateBuffers[i]->SetRegions(region);
    m_UpdateBuffers[i]->Allocate();
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
typename MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                     TOutputImage, TFunction, TIdCell >::TimeStepType
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::CalculateChange()
{
  TimeStepType timeStep = NumericTraits< TimeStepType >::max();

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    InputImagePointer levelset = this->m_LevelSet[i];

    // Get the FiniteDifferenceFunction to use in calculations.
    const FiniteDifferenceFunctionPointer df = this->m_DifferenceFunctions[i];

    const OutputSizeType radius = df->GetRadius();

    // Break the input into a series of regions.  The first region is free
    // of boundary conditions, the rest with boundary conditions.  We operate
    // on the levelset region because input has been copied to output.
    FaceCalculatorType faceCalculator;
    FaceListType       faceList =
      faceCalculator (levelset, levelset->GetLargestPossibleRegion(), radius);

    void *globalData;

    // Ask the function object for a pointer to a data structure it
    // will use to manage any global values it needs.  We'll pass this
    // back to the function object at each calculation and then
    // again so that the function object can use it to determine a
    // time step for this iteration.
    globalData = df->GetGlobalDataPointer();

    typename FaceListType::iterator fIt;
    for ( fIt = faceList.begin(); fIt != faceList.end(); ++fIt )
      {
      // Process the non-boundary region.
      NeighborhoodIteratorType              nD (radius, levelset, *fIt);
      ImageRegionIterator< InputImageType > nU (m_UpdateBuffers[i], *fIt);

      nD.GoToBegin();
      nU.GoToBegin();

      while ( !nD.IsAtEnd() )
        {
        nU.Value() = df->ComputeUpdate (nD, globalData);
        ++nD;
        ++nU;
        }
      }

    // Ask the finite difference function to compute the time step for
    // this iteration.  We give it the global data pointer to use, then
    // ask it to free the global data memory.
    TimeStepType dt = df->ComputeGlobalTimeStep (globalData);
    df->ReleaseGlobalDataPointer (globalData);

    if ( dt < timeStep )
      {
      timeStep = dt;
      }
    }

  timeStep = 0.08;  // FIXME !!! After all this work, assign a constant !!! Why
                    // ??

  return timeStep;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::SetFunctionCount(const IdCellType & n)
{
  this->Superclass::SetFunctionCount(n);

  this->m_UpdateBuffers.resize(n, ITK_NULLPTR);

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_UpdateBuffers[i] = InputImageType::New();
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::ApplyUpdate(TimeStepType dt)
{
  double         rms_change_accumulator = 0;
  double         den = 0;
  IdCellType     i;
  InputPixelType val;

  for ( i = 0; i < this->m_FunctionCount; i++ )
    {
    const double img_size = this->m_LevelSet[i]->GetLargestPossibleRegion().GetNumberOfPixels();
    den += img_size;
    }

  // this must never occur!
  if ( den < itk::Math::eps )
    {
    itkExceptionMacro("den = 0.");
    }

  // Updating the output image
  for ( i = 0; i < this->m_FunctionCount; i++ )
    {
    //NOTE: here this->m_LevelSet[i]->GetRequestedRegion() is used and
    // previously
    // it is this->m_LevelSet[i]->GetLargestPossibleRegion()
    InputRegionType region = this->m_LevelSet[i]->GetRequestedRegion();

    ImageRegionIterator< InputImageType > u(m_UpdateBuffers[i], region);
    ImageRegionIterator< InputImageType > o(this->m_LevelSet[i], region);

    u.GoToBegin();
    o.GoToBegin();

    while ( !u.IsAtEnd() )
      {
      val = static_cast< InputPixelType >( dt ) * u.Get();
      o.Set(o.Value() + val);
      rms_change_accumulator += static_cast< double >( itk::Math::sqr(val) );
      ++u;
      ++o;
      }

    if ( this->GetElapsedIterations() % this->m_ReinitializeCounter == 0 )
      {
      ThresholdFilterPointer thresh = ThresholdFilterType::New();
      thresh->SetLowerThreshold( NumericTraits< InputPixelType >::NonpositiveMin() );
      thresh->SetUpperThreshold(0);
      thresh->SetInsideValue(1);
      thresh->SetOutsideValue(0);
      thresh->SetInput(this->m_LevelSet[i]);
      thresh->Update();

      MaurerPointer maurer = MaurerType::New();
      maurer->SetInput( thresh->GetOutput() );
      maurer->SetSquaredDistance(false);
      maurer->SetUseImageSpacing(this->m_UseImageSpacing);
      maurer->SetInsideIsPositive(false);
      maurer->Update();

      ImageRegionIterator< InputImageType > it (maurer->GetOutput(), region);

      rms_change_accumulator = 0;

      o.GoToBegin();
      it.GoToBegin();

      while ( !o.IsAtEnd() )
        {
        val = it.Value();
        rms_change_accumulator += static_cast< double >( itk::Math::sqr(o.Value() - val) );
        o.Set(val);
        ++o;
        ++it;
        }
      }
    }

  this->SetRMSChange( std::sqrt(rms_change_accumulator / den) );
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage,
          typename TFunction, typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                            TOutputImage, TFunction, TIdCell >
::PostProcessOutput()
{
  this->CopyInputToOutput();
}
} // end namespace itk

#endif
