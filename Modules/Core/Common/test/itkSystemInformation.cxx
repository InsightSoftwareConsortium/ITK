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
#include <iostream>
#include <fstream>
#include <cstdlib>
#include "itksys/SystemInformation.hxx"

int
main(int, char *[])
{
  itksys::SystemInformation mySys;
  mySys.RunCPUCheck();
  mySys.RunOSCheck();
  mySys.RunMemoryCheck();

  std::cout << "---------- System Information ----------" << '\n';

  std::cout << "VendorString:                 " << mySys.GetVendorString() << '\n';
  std::cout << "VendorID:                     " << mySys.GetVendorID() << '\n';
  std::cout << "TypeID:                       " << mySys.GetTypeID() << '\n';
  std::cout << "FamilyID:                     " << mySys.GetFamilyID() << '\n';
  std::cout << "ModelID:                      " << mySys.GetModelID() << '\n';
  std::cout << "SteppingCode:                 " << mySys.GetSteppingCode() << '\n';
  std::cout << "ExtendedProcessorName:        " << mySys.GetExtendedProcessorName() << '\n';
  std::cout << "DoesCPUSupportCPUID:          " << mySys.DoesCPUSupportCPUID() << '\n';
  std::cout << "ProcessorSerialNumber:        " << mySys.GetProcessorSerialNumber() << '\n';
  std::cout << "ProcessorCacheSize:           " << mySys.GetProcessorCacheSize() << '\n';
  std::cout << "LogicalProcessorsPerPhysical: " << mySys.GetLogicalProcessorsPerPhysical() << '\n';
  std::cout << "ProcessorClockFrequency:      " << mySys.GetProcessorClockFrequency() << '\n';
  std::cout << "ProcessorAPICID:              " << mySys.GetProcessorAPICID() << '\n';

  std::cout << "OSName:                       " << mySys.GetOSName() << '\n';
  std::cout << "Hostname:                     " << mySys.GetHostname() << '\n';
  std::cout << "OSRelease:                    " << mySys.GetOSRelease() << '\n';
  std::cout << "OSVersion:                    " << mySys.GetOSVersion() << '\n';
  std::cout << "OSPlatform:                   " << mySys.GetOSPlatform() << '\n';

  std::cout << "Is64Bits:                     " << mySys.Is64Bits() << '\n';

  std::cout << "NumberOfLogicalCPU:           " << mySys.GetNumberOfLogicalCPU() << '\n';

  std::cout << "NumberOfPhysicalCPU:          " << mySys.GetNumberOfPhysicalCPU() << '\n';

  // Retrieve memory information in mebibytes.
  std::cout << "TotalVirtualMemory:           " << mySys.GetTotalVirtualMemory() << '\n';
  std::cout << "AvailableVirtualMemory:       " << mySys.GetAvailableVirtualMemory() << '\n';
  std::cout << "TotalPhysicalMemory:          " << mySys.GetTotalPhysicalMemory() << '\n';
  std::cout << "AvailablePhysicalMemory:      " << mySys.GetAvailablePhysicalMemory() << '\n';

  return EXIT_SUCCESS;
}
