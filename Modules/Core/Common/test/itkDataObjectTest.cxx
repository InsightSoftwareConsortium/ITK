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

#include "itkDataObject.h"
#include "itkRealTimeClock.h"

namespace itk {

class DataObjectTestHelper : public DataObject
{
public:
  /** Standard typedefs. */
  typedef DataObjectTestHelper       Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DataObjectTestHelper, DataObject);

protected:
  DataObjectTestHelper() {}
  ~DataObjectTestHelper() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
    {
    this->Superclass::PrintSelf( os, indent );
    }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DataObjectTestHelper);

};

}

int itkDataObjectTest( int , char * [] )
{
  itk::DataObjectTestHelper::Pointer dataObject = itk::DataObjectTestHelper::New();

  itk::RealTimeClock::Pointer clock = itk::RealTimeClock::New();
  dataObject->SetRealTimeStamp(clock->GetRealTimeStamp());
  itk::RealTimeStamp timeStamp = dataObject->GetRealTimeStamp();
  dataObject->DataHasBeenGenerated();
  if( timeStamp != dataObject->GetRealTimeStamp() )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
