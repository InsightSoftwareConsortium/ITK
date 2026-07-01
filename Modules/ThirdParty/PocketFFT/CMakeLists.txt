project(ITKPocketFFT)
set(ITKPocketFFT_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_POCKETFFT
  "Use an outside build of pocketfft."
  ${ITK_USE_SYSTEM_LIBRARIES}
)
mark_as_advanced(ITK_USE_SYSTEM_POCKETFFT)

# pocketfft is header-only, so no library linking is required.  itk_pocketfft.h
# is the single entry point: it is the only header that includes pocketfft and
# applies ITK's POCKETFFT_NAMESPACE / POCKETFFT_CACHE_SIZE configuration.
set(ITKPocketFFT_LIBRARIES "")

if(ITK_USE_SYSTEM_POCKETFFT)
  find_path(POCKETFFT_INCLUDE_DIR NAMES pocketfft_hdronly.h REQUIRED)
  mark_as_advanced(POCKETFFT_INCLUDE_DIR)
  set(ITKPocketFFT_INCLUDE_DIRS ${POCKETFFT_INCLUDE_DIR})
  set(ITKPocketFFT_NO_SRC 1)
else()
  # The vendored header is reached through the "itkpocketfft/" prefix.
  set(ITKPocketFFT_INCLUDE_DIRS ${ITKPocketFFT_SOURCE_DIR}/src)
endif()

# For the generated itk_pocketfft.h
list(APPEND ITKPocketFFT_INCLUDE_DIRS ${ITKPocketFFT_BINARY_DIR}/src)

itk_module_impl()

configure_file(src/itk_pocketfft.h.in src/itk_pocketfft.h)
install(
  FILES
    ${ITKPocketFFT_BINARY_DIR}/src/itk_pocketfft.h
  DESTINATION ${ITKPocketFFT_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
