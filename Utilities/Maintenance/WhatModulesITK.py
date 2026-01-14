#!/usr/bin/env python


import os, sys
import re
import argparse

program = sys.argv[0]

# Parse command line arguments
parser = argparse.ArgumentParser(
    description="Generate CMake find_package or target_link_libraries commands for ITK modules",
    epilog="""
Examples:
  Running from the ITK source:
    ./Utilities/Maintenance/WhatModulesITK.py . Modules/Filtering/Smoothing/test/itkDiscreteGaussianImageFilterTest.cxx

  With --link option:
    ./Utilities/Maintenance/WhatModulesITK.py --link . Examples/Filtering/AntiAliasBinaryImageFilter.cxx

  To select many files from an application:
    ./Utilities/Maintenance/WhatModulesITK.py . $(find /path/to/itk/project/ -type f)

NOTE: The modules list is created by looking at the itk include
      files used by the application files. Some programs do not include
      files for every class they use. They assume the include files
      they reference will reference the required includes. For
      example, many applications do not include itkImage.h because
      the itk files they do include reference itkImage.h. itkImage.h is
      in the module ITKCommon and may not be listed.

NOTE: IO modules, other than ITKIOImageBase, are not discovered
      unless their include file is present in the application
      code. If ITKIOImageBase is present, a cmake variable
      ITK_IO_MODULES_USED is created and added to the module list.
""",
    formatter_class=argparse.RawDescriptionHelpFormatter,
)
parser.add_argument(
    "--link",
    action="store_true",
    help="Output target_link_libraries command with Module suffix instead of find_package",
)
parser.add_argument("itkSourceTree", help="Path to ITK source tree")
parser.add_argument(
    "applicationFiles", nargs="+", help="Application source files to analyze"
)

args = parser.parse_args()
itk_source_tree = args.itkSourceTree
application_files = args.applicationFiles
use_link_command = args.link


# Build a dict that maps include files to paths
def IncludesToPaths(path):
    includeToPath = dict()
    prog = re.compile(r"(itk.*\.h)")
    for root, dirs, files in os.walk(path):
        for f in files:
            if prog.match(f):
                includeFile = prog.findall(f)[0]
                parts = root.split(os.sep)
                module = parts[len(parts) - 3] + parts[len(parts) - 2]
                includeToPath[includeFile] = module
    return includeToPath


# Build a dict that maps paths to modules
def FindModules(path):
    pathToModule = dict()
    fileProg = re.compile(r"itk-module.cmake")
    moduleProg = re.compile(r".*itk_module[^(]*\(\s*([^\s)]+)", re.S)
    for root, dirs, files in os.walk(path):
        for f in files:
            if fileProg.match(f):
                fid = open(root + os.sep + f)
                contents = fid.read()
                m = moduleProg.match(contents)
                if m:
                    moduleName = m.group(1)
                    parts = root.split(os.sep)
                    pathToModule[parts[len(parts) - 2] + parts[len(parts) - 1]] = (
                        moduleName
                    )
                fid.close()
    return pathToModule


# Build a set that contains itk includes
def FindIncludes(path):
    includes = set()
    includeProg = re.compile(r"(itk.*\.h)")
    fid = open(path)
    contents = fid.read()
    incs = includeProg.findall(contents)
    includes.update(incs)
    fid.close()
    return includes


# Start the program

# Generate dict's for mapping includes to modules
includesToPaths = IncludesToPaths(os.path.join(itk_source_tree, "Modules"))
pathsToModules = FindModules(os.path.join(itk_source_tree, "Modules"))

# Test to see if ITK source is provided
if len(pathsToModules) == 0:
    print(
        program
        + ": "
        + itk_source_tree
        + " is not an ITK source directory. It does not contain any itk-module.cmake files."
    )
    exit(1)

# Build a set of includes for all command line files
allIncludes = set()

for f in application_files:
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

# Map IO base modules to their corresponding Meta factory modules
io_module_map = [
    ("ITKIOImageBase", "ITKImageIO"),
    ("ITKIOMeshBase", "ITKMeshIO"),
    ("ITKIOTransformBase", "ITKTransformIO"),
]

# Store original count before modifying allModules
original_module_count = len(allModules)

namespace_prefix = "ITK::"

# Print output based on --link option
if use_link_command:
    # Output target_link_libraries with namespaced Module suffix
    print("target_link_libraries(\n YourTarget\n PRIVATE")

    for base_module, io_module in io_module_map:
        if base_module in allModules:
            allModules.discard(base_module)
            print("    " + namespace_prefix + io_module)
    for module in sorted(allModules):
        print("    " + namespace_prefix + module + "Module")
    print(")")
else:
    # Original find_package output
    print("find_package(ITK COMPONENTS")
    for module in sorted(allModules):
        print("  " + module)
    for base_module, io_module in io_module_map:
        if base_module in allModules:
            print("  " + io_module)
    print(")")

print(
    "Your application code includes "
    + str(original_module_count)
    + " of "
    + str(len(pathsToModules))
    + " itk modules."
)
