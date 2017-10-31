#!/usr/bin/env python

from __future__ import print_function

import os, sys
import re

program = sys.argv[0]
if len(sys.argv) < 3:
    print("""
Usage: WhatModulesITK.py itkSourceTree applicationFiles...
    Generate a FindPackage(ITK COMPONENTS) that lists all modules referenced by a set of files

    For example:
      Running from the ITK source,
        ./Utilities/Maintenance/WhatModulesITK.py . Modules/Filtering/Smoothing/test/itkDiscreteGaussianImageFilterTest.cxx
      Produces
        Find_Package(ITK COMPONENTS
          ITKImageFilterBase
          ITKSmoothing
          ITKTestKernel
          )
       To select many files from an application,
         ./Utilities/Maintenance/WhatModulesITK.py . $(find /path/to/itk/project/ -type f)

NOTE: The modules list is created by looking at the itk include
      files used by the application files. Some programs do not include
      files for every class they use. They assume the include files
      they reference will reference the required includes. For
      example, many applications do not include itkImage.h because
      the itk files they do include reference itkImage.h. itkImage.h is
      in the module ITKCommon and my not be listed.

NOTE: IO modules, other than ITKIOImageBase, are not discovered
      unless their include file is present in the application
      code. If ITKIOImageBase is present, a cmake variable
      ITK_IO_MODULES_USED is created and added to the module list.
""")
    exit(0)

# Build a dict that maps include files to paths
def IncludesToPaths(path):
    includeToPath = dict()
    prog = re.compile(r"(itk.*\.h)")
    for root, dirs, files in os.walk(path):
        for f in files:
            if prog.match(f):
                includeFile = prog.findall(f)[0]
                parts = root.split(os.sep)
                module = parts[len(parts)-3] + parts[len(parts)-2]
                includeToPath[includeFile] = module
    return includeToPath

# Build a dict that maps paths to modules
def FindModules(path):
    pathToModule = dict()
    fileProg = re.compile(r"itk-module.cmake")
    moduleProg = re.compile('.*itk_module[^(]*\(([^ \n]*)',re.S)
    for root, dirs, files in os.walk(path):
        for f in files:
            if fileProg.match(f):
                fid = open(root + os.sep + f,"r")
                contents = fid.read()
                m = moduleProg.match(contents)
                if m:
                    moduleName = m.group(1)
                    parts = root.split(os.sep)
                    pathToModule[parts[len(parts)-2] + parts[len(parts)-1]] = moduleName
                fid.close()
    return pathToModule

# Build a set that contains itk includes
def FindIncludes(path):
    includes = set()
    includeProg = re.compile(r"(itk.*\.h)")
    fid = open(path, "r")
    contents = fid.read()
    incs = includeProg.findall(contents)
    includes.update(incs)
    fid.close()
    return includes

# Start the program

# Generate dict's for mapping includes to modules
includesToPaths = IncludesToPaths(os.path.join(sys.argv[1],  "Modules"))
pathsToModules = FindModules(os.path.join(sys.argv[1], "Modules"))

# Test to see if ITK source is provided
if len(pathsToModules) == 0:
    print(program + ": " + sys.argv[1] + " is not an ITK source directory. It does not contain any itk-module.cmake files.")
    exit(1)

# Build a set of includes for all command line files
allIncludes = set()

sys.argv.pop(0)
sys.argv.pop(0)
for f in sys.argv:
    if os.path.isfile(f):
        allIncludes.update(FindIncludes(f))
    else:
        print(program + ": " + f + " is a directory and is ignored")

# Build a set that contains all modules referenced in command line files
allModules = set()
for inc in allIncludes:
    if inc in includesToPaths:
        module = includesToPaths[inc]
        allModules.add(pathsToModules[includesToPaths[inc]])

# Print a useful cmake command
print("find_package(ITK COMPONENTS")
for module in sorted(allModules):
    print("  " + module)
if "ITKIOImageBase" in allModules:
    print(r"  ITKImageIO")
if "ITKIOTransformBase" in allModules:
    print(r"  ITKTransformIO")
print(")")

print("Your application code includes " + str(len(allModules)) + " of " + str(len(pathsToModules)) + " itk modules.")
