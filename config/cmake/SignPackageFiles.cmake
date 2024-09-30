# This script signs the targets for the package
message(STATUS "Signing script in ${CPACK_TEMPORARY_INSTALL_DIRECTORY} and ${CPACK_PACKAGE_INSTALL_DIRECTORY}")

# RPM needs ALL_COMPONENTS_IN_ONE added to path between ${CPACK_TEMPORARY_INSTALL_DIRECTORY} and ${CPACK_PACKAGE_INSTALL_DIRECTORY}
if (CPACK_GENERATOR MATCHES "RPM")
    set (CPACK_TARGET_FILE_DIRECTORY "${CPACK_TEMPORARY_INSTALL_DIRECTORY}/ALL_COMPONENTS_IN_ONE/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
elseif (CPACK_GENERATOR MATCHES "WIX" OR CPACK_GENERATOR MATCHES "NSIS")
    set (CPACK_TARGET_FILE_DIRECTORY "${CPACK_TEMPORARY_INSTALL_DIRECTORY}/libraries")
elseif (CPACK_GENERATOR MATCHES "ZIP")
    set (CPACK_TARGET_FILE_DIRECTORY "${CPACK_TEMPORARY_INSTALL_DIRECTORY}")
elseif (CPACK_GENERATOR MATCHES "DragNDrop")
    set (CPACK_TARGET_FILE_DIRECTORY "${CPACK_TEMPORARY_INSTALL_DIRECTORY}/ALL_IN_ONE/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
else ()
    set (CPACK_TARGET_FILE_DIRECTORY "${CPACK_TEMPORARY_INSTALL_DIRECTORY}/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
endif ()
file (GLOB target_list LIST_DIRECTORIES false "${CPACK_TARGET_FILE_DIRECTORY}/lib/*" "${CPACK_TARGET_FILE_DIRECTORY}/bin/*" "${CPACK_TARGET_FILE_DIRECTORY}/lib/plugin/*")
foreach (targetfile IN LISTS target_list)
    if (WIN32)
        # Sign the targets
        execute_process (COMMAND $ENV{SIGNTOOLDIR}/signtool
          sign /v /debug /fd SHA256 /tr http://timestamp.acs.microsoft.com /td SHA256
          /dlib "Microsoft.Trusted.Signing.Client/bin/x64/Azure.CodeSigning.Dlib.dll" /dmdf ${CPACK_ORIG_SOURCE_DIR}/credentials.json
          ${targetfile}
        )
        execute_process (
          COMMAND ${CMAKE_COMMAND} -E echo "Signing the target ${targetfile}"
        )
    elseif (APPLE)
        # Sign the targets
        execute_process (COMMAND codesign
          --force --timestamp --options runtime --entitlements ${CPACK_ORIG_SOURCE_DIR}/config/cmake/distribution.entitlements 
          --verbose=4 --strict --sign "$ENV{SIGNER}"
          ${targetfile}
        )
        execute_process (
          COMMAND ${CMAKE_COMMAND} -E echo "Signing the target ${targetfile}"
        )
    else ()
        execute_process (
          COMMAND ${CMAKE_COMMAND} -E echo "Signing the target ${targetfile}"
        )
    endif ()
endforeach ()
