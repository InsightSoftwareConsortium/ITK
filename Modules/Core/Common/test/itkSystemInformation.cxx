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
#include <iostream>
#include <fstream>
#include <cstdlib>
#include "itksys/SystemInformation.hxx"

int main(int,char *[])
{
  itksys::SystemInformation mySys;
  mySys.RunCPUCheck();
  mySys.RunOSCheck();
  mySys.RunMemoryCheck();

  std::cout << "---------- System Information ----------" << std::endl;

  std::cout << "VendorString:                 "
            << mySys.GetVendorString() << std::endl;
  std::cout << "VendorID:                     "
            << mySys.GetVendorID() << std::endl;
  std::cout << "TypeID:                       "
            << mySys.GetTypeID() << std::endl;
  std::cout << "FamilyID:                     "
            << mySys.GetFamilyID() << std::endl;
  std::cout << "ModelID:                      "
            << mySys.GetModelID() << std::endl;
  std::cout << "SteppingCode:                 "
            << mySys.GetSteppingCode() << std::endl;
  std::cout << "ExtendedProcessorName:        "
            << mySys.GetExtendedProcessorName() << std::endl;
  std::cout << "DoesCPUSupportCPUID:          "
            << mySys.DoesCPUSupportCPUID() << std::endl;
  std::cout << "ProcessorSerialNumber:        "
            << mySys.GetProcessorSerialNumber() << std::endl;
  std::cout << "ProcessorCacheSize:           "
            << mySys.GetProcessorCacheSize() << std::endl;
  std::cout << "LogicalProcessorsPerPhysical: "
            << mySys.GetLogicalProcessorsPerPhysical() << std::endl;
  std::cout << "ProcessorClockFrequency:      "
            << mySys.GetProcessorClockFrequency() << std::endl;
  std::cout << "ProcessorAPICID:              "
            << mySys.GetProcessorAPICID() << std::endl;

  std::cout << "OSName:                       "
            << mySys.GetOSName() << std::endl;
  std::cout << "Hostname:                     "
            << mySys.GetHostname() << std::endl;
  std::cout << "OSRelease:                    "
            << mySys.GetOSRelease() << std::endl;
  std::cout << "OSVersion:                    "
            << mySys.GetOSVersion() << std::endl;
  std::cout << "OSPlatform:                   "
            << mySys.GetOSPlatform() << std::endl;

  std::cout << "Is64Bits:                     "
            << mySys.Is64Bits() << std::endl;

  std::cout << "NumberOfLogicalCPU:           "
            << mySys.GetNumberOfLogicalCPU() << std::endl;

  std::cout << "NumberOfPhysicalCPU:          "
            << mySys.GetNumberOfPhysicalCPU() << std::endl;

  // Retrieve memory information in megabyte.
  std::cout << "TotalVirtualMemory:           "
            << mySys.GetTotalVirtualMemory() << std::endl;
  std::cout << "AvailableVirtualMemory:       "
            << mySys.GetAvailableVirtualMemory() << std::endl;
  std::cout << "TotalPhysicalMemory:          "
            << mySys.GetTotalPhysicalMemory() << std::endl;
  std::cout << "AvailablePhysicalMemory:      "
            << mySys.GetAvailablePhysicalMemory() << std::endl;

  return EXIT_SUCCESS;
}
