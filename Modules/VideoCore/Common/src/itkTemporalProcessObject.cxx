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
#ifndef __itkTemporalProcessObject_cxx
#define __itkTemporalProcessObject_cxx

#include "itkTemporalProcessObject.h"
#include "itkTemporalRegion.h"
#include "itkTemporalDataObject.h"

#include <math.h>

namespace itk
{

//-CONSTRUCTOR PRINT-----------------------------------------------------------

//
// Constructor
//
TemporalProcessObject::TemporalProcessObject()
  : m_UnitInputNumberOfFrames(1),
    m_UnitOutputNumberOfFrames(1),
    m_FrameSkipPerOutput(1)
{}

//
// PrintSelf
//
void
TemporalProcessObject::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "TemporalProcessObject" << std::endl;
}

//-PROPAGATE REQUESTED REGION CALLBACKS----------------------------------------

//
// EnlargeOutputRequestedRegion
//
void
TemporalProcessObject::EnlargeOutputRequestedRegion(DataObject* output)
{
  // Check that output is a TemporalDataObject
  TemporalDataObject* tOutput = dynamic_cast<TemporalDataObject*>(output);

  if (tOutput)
    {
    this->EnlargeOutputRequestedTemporalRegion(tOutput);
    }
  else
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::EnlargeOutputRequestedRegion() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
}

//
// GenerateOutputRequestedRegion
//
void
TemporalProcessObject::GenerateOutputRequestedRegion(DataObject* output)
{
  // Check that output is a TemporalDataObject
  TemporalDataObject* tOutput = dynamic_cast<TemporalDataObject*>(output);

  if (tOutput)
    {
    this->GenerateOutputRequestedTemporalRegion(tOutput);
    }
  else
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateOutputRequestedRegion() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
}

//
// GenerateInputRequestedRegion
//
void
TemporalProcessObject::GenerateInputRequestedRegion(DataObject* output)
{
    // Check that output is a TemporalDataObject
  TemporalDataObject* tOutput = dynamic_cast<TemporalDataObject*>(output);

  if (tOutput)
    {
    this->GenerateInputRequestedTemporalRegion(tOutput);
    }
  else
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateInputRequestedRegion() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }
}


//-TEMPORAL STREAMING----------------------------------------------------------

//
// GenerateData
//
void
TemporalProcessObject::GenerateData()
{
  // Call Pre-processing method
  this->BeforeTemporalStreamingGenerateData();

  // Split up the requested output temporal region
  std::vector<TemporalRegion> inputTemporalRegionRequests = this->SplitRequestedTemporalRegion();

  // Get the first output frame location
  TemporalDataObject* output = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  if (!output)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateData() "
                      << "cannot cast " << typeid(output).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }

  //DEBUG -- this should be the GetFrameStart() of the unbuffered region
  unsigned long outputStartFrame = 0;//output->GetRequestedTemporalRegion().GetFrameStart();

  // Process each of the temporal sub-regions in sequence
  for (unsigned int i = 0; i < inputTemporalRegionRequests.size(); ++i)
    {
    // Set Input's requested region to the new reqest at i
    TemporalDataObject* input = dynamic_cast<TemporalDataObject*>(this->GetInput(0));
    if (!input)
      {
      itkExceptionMacro(<< "itk::TemporalProcessObject::GenerateData() "
                        << "cannot cast " << typeid(input).name() << " to "
                        << typeid(TemporalDataObject*).name() );
      }
    input->SetRequestedTemporalRegion(inputTemporalRegionRequests[i]);

    // Call Input's UpdateOutputData()
    input->UpdateOutputData();

    // Call TemporalStreamingGenerateData to process the newly acquired data
    this->TemporalStreamingGenerateData(outputStartFrame);

    // Increment outputStartFrame
    outputStartFrame += this->m_UnitOutputNumberOfFrames;
    }

  // Call post-processing method
  this->AfterTemporalStreamingGenerateData();
}


//
// TemporalStreamingGenerateData
//
void
TemporalProcessObject::TemporalStreamingGenerateData(unsigned long outputFrameStart)
{
  itkExceptionMacro(<< "itk::Error: " << this->GetNameOfClass()
                    << "(" << this << "): Subclass should override this method!!!" );
}


//
// SplitRequestedTemporalRegion
//
std::vector<TemporalRegion>
TemporalProcessObject::SplitRequestedTemporalRegion()
{

  // Get the current output TemporalDataObject
  TemporalDataObject* outputObject = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  if (!outputObject)
    {
    itkExceptionMacro(<< "itk::TemporalProcessObject::SplitRequestedTemporalRegion() "
                      << "cannot cast " << typeid(outputObject).name() << " to "
                      << typeid(TemporalDataObject*).name() );
    }

  // Get the TemporalRegion representing the difference between the output's
  // requested temporal region and its buffered temporal region. This
  // difference is defined as any time that is covered by the requested region
  // but not by the buffered region
  TemporalRegion unbufferedRegion;    //DEBUG

  // Calculate the number of input requests that will be needed
  unsigned long numRequests = (unsigned long)(ceil(
                                              (double)(unbufferedRegion.GetFrameDuration() /
                                              (double)(m_UnitOutputNumberOfFrames)) ));

  // Calculate left extra frames that will be requested (might be unnecessary)
  //unsigned long extraFrames = (numRequests * m_UnitOutputNumberOfFrames) -
  //                              unbufferedRegion.GetFrameDuration();

  // Set up the requested input temporal region set (NOT PROPERLY HANDLING REAL TIME!!!!!!!!)
  std::vector<TemporalRegion> inputTemporalRegionRequests;

  unsigned long regionStartFrame = 1;
  if (this->m_FrameSkipPerOutput > 0)
    {
    regionStartFrame = unbufferedRegion.GetFrameStart();
    }
  else if (this->m_FrameSkipPerOutput < 0)
    {
    regionStartFrame = unbufferedRegion.GetFrameStart() + unbufferedRegion.GetFrameDuration() -
                        this->m_UnitOutputNumberOfFrames;
    }
  for (unsigned int i = 0; i < numRequests; ++i)
    {
    // Create the requested region
    TemporalRegion r;
    r.SetFrameStart(regionStartFrame);
    r.SetFrameDuration(this->m_UnitInputNumberOfFrames);
    inputTemporalRegionRequests.push_back(r);

    // Move the start position for the next requested region
    regionStartFrame += this->m_FrameSkipPerOutput;
    }

  // Return the set of regions
  return inputTemporalRegionRequests;

}



} // end namespace itk

#endif
