# Install the vendored upstream files.  This install lives OUTSIDE the
# itkpocketfft/ subtree because UpdateFromUpstream.sh overwrites that subtree
# wholesale with the upstream-provided file set.
install(
  FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/itkpocketfft/pocketfft_hdronly.h
    ${CMAKE_CURRENT_SOURCE_DIR}/itkpocketfft/LICENSE.md
  DESTINATION ${ITKPocketFFT_INSTALL_INCLUDE_DIR}/itkpocketfft
  COMPONENT Development
)
