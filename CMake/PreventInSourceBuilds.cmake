#
# This function will prevent in-source builds
function(AssureOutOfSourceBuilds)
  # make sure the user doesn't play dirty with symlinks
  get_filename_component(srcdir "${CMAKE_SOURCE_DIR}" REALPATH)
  get_filename_component(bindir "${CMAKE_BINARY_DIR}" REALPATH)

  # disallow in-source builds
  if("${srcdir}" STREQUAL "${bindir}")
    message("######################################################")
    message(
      "# ITK should not be configured & built in the ITK source directory"
    )
    message("# You must run cmake in a build directory.")
    message("# For example:")
    message("# mkdir ITK-Sandbox ; cd ITK-sandbox")
    message(
      "# git clone https://itk.org/ITK.git # or download & unpack the source tarball"
    )
    message("# mkdir ITK-build")
    message("# this will create the following directory structure")
    message("#")
    message("# ITK-Sandbox")
    message("#  +--ITK")
    message("#  +--ITK-build")
    message("#")
    message("# Then you can proceed to configure and build")
    message("# by using the following commands")
    message("#")
    message("# cd ITK-build")
    message("# cmake ../ITK # or ccmake, or cmake-gui ")
    message("# make")
    message("#")
    message("# NOTE: Given that you already tried to make an in-source build")
    message("#       CMake have already created several files & directories")
    message("#       in your source tree. run 'git status' to find them and")
    message("#       remove them by doing:")
    message("#")
    message("#       cd ITK-Sandbox/ITK")
    message("#       git clean -n -d")
    message("#       git clean -f -d")
    message("#       git checkout --")
    message("#")
    message("######################################################")
    message(FATAL_ERROR "Quitting configuration")
  endif()
endfunction()

assureoutofsourcebuilds()
