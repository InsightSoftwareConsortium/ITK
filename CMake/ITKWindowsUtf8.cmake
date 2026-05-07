# Attach the UTF-8 active-code-page manifest to a Windows executable so its
# narrow-char Win32 APIs (CreateFileA, fopen, std::ifstream(const char*), ...)
# treat byte strings as UTF-8 on Windows 10 1903 and later. No-op on
# non-MSVC toolchains and on platforms where the manifest is irrelevant.
#
# Usage: itk_target_attach_windows_utf8_manifest(<target>)
set(
  _itk_windows_utf8_manifest
  "${CMAKE_CURRENT_LIST_DIR}/Windows-utf8-codepage.manifest"
)

function(itk_target_attach_windows_utf8_manifest _target)
  if(MSVC)
    target_sources(${_target} PRIVATE "${_itk_windows_utf8_manifest}")
  endif()
endfunction()
