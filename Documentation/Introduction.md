Introduction
============

ITK version tracking and development is hosted by [Git](https://git-scm.com/).
Please select a task for further instructions:

Main Tasks
----------

  * [Install Git](https://itk.org/Wiki/Git/Download) - Git 2.8.9 or greater is
    preferred (required for development)
  * [Download ITK] - Users start here
  * [Develop ITK] - Contributors start here

Other Tasks
-----------

  * [Test ITK] - CDash client setup
  * [Learn Git](https://itk.org/Wiki/Git/Resources) - Third-party documentation

*The remainder of this page provides reference information and links. It is
not intended to provide instructions.*

Repositories
------------

One may browse the repositories online using the [GitHub] search interface at
[InsightSoftwareConsortium/ITK].

| Repository                  | Purpose                                  | Access                                          | URL                          |
| ----------------------------| ---------------------------------------- | ----------------------------------------------- | ---------------------------- |
| <br/>`ITK.git` <br/>        | <br/> Insight Toolkit                    | clone (git) <br/> clone (http) <br/> push (ssh) | git://itk.org/ITK.git <br/> http://itk.org/ITK.git <br/> `git@itk.org:ITK.git` |
| <br/> `stage/ITK.git` <br/> | <br/> ITK Topic Stage <br/>              | clone (git) <br/> clone (http) <br/> push (ssh) | git://itk.org/stage/ITK.git <br/> http://itk.org/stage/ITK.git <br/> `git@itk.org:stage/ITK.git` |
| <br/> `ITKData.git` <br/>   | <br/> ITK `Testing/Data` Submodule <br/> | clone (git) <br/> clone (http) <br/> push (ssh) | git://itk.org/ITKData.git <br/> http://itk.org/ITKData.git <br/> `git@itk.org:ITKData.git` |


Branches
--------

At the time of this writing the `ITK.git` repository has the following
branches:

  * `master`: Development (default)
  * `release`: Maintenance of latest release
  * `release-3.20`: Maintenance of the ITKv3 series
  * `nightly-master`: Follows master, updated at 01:00 UTC
  * `hooks`: Local commit hooks (place in `.git/hooks`)
  * `dashboard`: Dashboard script (setup a CDash client)

Release branches converted from CVS have been artificially merged into
`master`. Actual releases have tags named by the release version number.



[Dashboard]: Documentation/Dashboard.md
[Develop ITK]: CONTRIBUTING.md
[Download ITK]: Documentation/Download.md
[InsightSoftwareConsortium/ITK]: https://github.com/InsightSoftwareConsortium/ITK
[Test ITK]: Documentation/Dashboard.md

[GitHub]: https://github.com/