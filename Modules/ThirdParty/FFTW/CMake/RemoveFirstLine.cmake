# Remove the first line from a file and patch EXPORT names
# This is used to:
# 1. Remove "cmake_minimum_required (VERSION 3.0)" from FFTW's CMakeLists.txt
#    so that ITK's policy settings (via CMAKE_POLICY_DEFAULT_CMP0077) apply.
# 2. Change "EXPORT FFTW3LibraryDepends" to "EXPORT FFTW3LibraryDepends${PREC_SUFFIX}"
#    to avoid conflicts when building both single and double precision.
# 3. Change export() FILE path from "FFTW3LibraryDepends.cmake" to "FFTW3LibraryDepends${PREC_SUFFIX}.cmake"
#    to avoid conflicts when building both single and double precision.
# 4. Add "EXPORT FFTW3LibraryDepends${PREC_SUFFIX}" to the install(TARGETS ${subtarget}) in the foreach loop
#    so all subtargets (threads, omp) are included in the export.

if(NOT DEFINED INFILE)
  message(FATAL_ERROR "INFILE not defined")
endif()

# Read the file
file(READ "${INFILE}" CONTENT)

# Split into lines
string(REGEX REPLACE "\n" ";" LINES "${CONTENT}")

# Remove first line
list(REMOVE_AT LINES 0)

# Join back together
string(REPLACE ";" "\n" NEW_CONTENT "${LINES}")

# Replace EXPORT name to include precision suffix
string(
  REPLACE
  "EXPORT FFTW3LibraryDepends"
  "EXPORT FFTW3LibraryDepends\${PREC_SUFFIX}"
  NEW_CONTENT
  "${NEW_CONTENT}"
)

# Replace export() FILE name to include precision suffix
string(
  REPLACE
  "FILE \${PROJECT_BINARY_DIR}/FFTW3LibraryDepends.cmake"
  "FILE \${PROJECT_BINARY_DIR}/FFTW3LibraryDepends\${PREC_SUFFIX}.cmake"
  NEW_CONTENT
  "${NEW_CONTENT}"
)

# Add EXPORT clause to the install(TARGETS ${subtarget}) in foreach loop
string(
  REPLACE
  "install (TARGETS \${subtarget}\n"
  "install (TARGETS \${subtarget}\n    EXPORT FFTW3LibraryDepends\${PREC_SUFFIX}\n"
  NEW_CONTENT
  "${NEW_CONTENT}"
)

# Write back
file(WRITE "${INFILE}" "${NEW_CONTENT}")

message(
  STATUS
  "Patched ${INFILE}: removed first line, updated EXPORT names, fixed export() FILE path, and added EXPORT to foreach install"
)
