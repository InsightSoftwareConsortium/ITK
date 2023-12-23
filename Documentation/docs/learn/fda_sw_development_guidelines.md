# FDA guidelines for software development

## FDA guidelines for off-the-shelf software

ITK and VTK are to be considered as off-the-shelf (OTS) products that
are used for supporting a higher level medical application/product. The
developer of such application/product will be responsible for performing
the validation processes described in FDA published guidelines for the
development of software-related medical devices.

ITK and VTK are intended to be used as elements in medical applications,
those medical applications are the ones that can be subject of an FDA
approval. Whoever develop such applications has the responsibility of
validating their application in order to demonstrate that it is
appropriate for the advertised purpose.

For example, if you develop an application for radiation treatment
planning based on ITK, it is your responsibility to validate that
application. In the process you will have to validate that the use you
make of ITK satisfies the requirements of your application. You will
probably be using a limited set of ITK functionalities in such
application, therefore what you have to demonstrate is that those
specific ITK functionalities are behaving as expected in the context of
your application.

Note that not only you need to validate the use of ITK or VTK, but also
the use of any other off-the-shelf (OTS) software product used in the
development of your application. That will included things like:

- Your operating system
- Your compiler
- Your OpenGL library/drivers
- Any other library that your application is linking to.
- Even your processor

## Software development practices

It is worth to point out that the software development process using in
ITK and VTK are already following many of the FDA Guidelines for
software developement. In particular

- Continuous Testing via Dashboard ([CTest](https://cmake.org/cmake/help/latest/manual/ctest.1.html))
- Version control ([git](https://git-scm.com/))
- Configuration standarization ([CMake](https://cmake.org/))
- Bug tracking ([GitHub](https://github.com/features/issues)/[GitLab issues](https://docs.gitlab.com/ee/user/project/issues/))

## General guidelines

For details on the FDA Guidelines for development of software for
medical devices you must look at the following documents:

- [Off-The-Shelf Software Use in Medical Devices](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/shelf-software-use-medical-devices)
- [General Principles of Software Validation](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/general-principles-software-validation)
- [Guidance for the Submission Of Premarket Notifications for Emission Computed Tomography Devices and Accessories (SPECT and PET) and Nuclear Tomography Systems](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/guidance-submission-premarket-notifications-emission-computed-tomography-devices-and-accessories)
- [https://www.fda.gov/regulatory-information/search-fda-guidance-documents/submission-premarket-notifications-magnetic-resonance-diagnostic-devices](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/submission-premarket-notifications-magnetic-resonance-diagnostic-devices)
- [Guidance for the Submission of Premarket Notifications for Photon-Emitting Brachytherapy Sources - Guidance for Industry](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/guidance-submission-premarket-notifications-photon-emitting-brachytherapy-sources-guidance-industry)
- [Guidance for the Submission of Premarket Notifications For Radionuclide Dose Calibrators - Guidance for Industry](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/guidance-submission-premarket-notifications-radionuclide-dose-calibrators-guidance-industry)
- [Content of Premarket Submissions for Device Software Functions](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/content-premarket-submissions-device-software-functions)

These and other relevant documents can be found at the [FDA Guidance Doument search](https://www.fda.gov/regulatory-information/search-fda-guidance-documents)
and the [Medical Devices and Radiation-Emitting Products Guidance Documents](https://www.fda.gov/medical-devices/device-advice-comprehensive-regulatory-assistance/guidance-documents-medical-devices-and-radiation-emitting-products) sites.
