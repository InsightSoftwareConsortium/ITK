#
# Find the packages required by this module
#

# Needed VTK version
set(VERSION_MIN "5.10.0")

# Look for VTK
find_package(VTK NO_MODULE REQUIRED)
