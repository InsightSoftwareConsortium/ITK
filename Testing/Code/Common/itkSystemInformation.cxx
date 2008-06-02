/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSystemInformation.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <itksys/SystemInformation.hxx>

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
            << mySys.DoesCPUSupportCPUID() << std::endl;;
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
