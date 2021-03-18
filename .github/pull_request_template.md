<!-- The text within this markup is a comment, and is intended to provide
guidelines to open a Pull Request for the ITK repository. This text will not
be part of the Pull Request. -->


<!-- See the CONTRIBUTING (CONTRIBUTING.md) guide. Specifically:

Start ITK commit messages with a standard prefix (and a space):

 * BUG: fix for runtime crash or incorrect result
 * COMP: compiler error or warning fix
 * DOC: documentation change
 * ENH: new functionality
 * PERF: performance improvement
 * STYLE: no logic impact (indentation, comments)
 * WIP: Work In Progress not ready for merge

Provide a short, meaningful message that describes the change you made.

When the PR is based on a single commit, the commit message is usually left as
the PR message.

A reference to a related issue or pull request (https://help.github.com/articles/basic-writing-and-formatting-syntax/#referencing-issues-and-pull-requests)
in your repository. You can automatically
close a related issues using keywords (https://help.github.com/articles/closing-issues-using-keywords/)

@mentions (https://help.github.com/articles/basic-writing-and-formatting-syntax/#mentioning-people-and-teams)
of the person or team responsible for reviewing proposed changes. -->

## PR Checklist
- [ ] No [API changes](https://github.com/InsightSoftwareConsortium/ITK/blob/master/CONTRIBUTING.md#breaking-changes) were made (or the changes have been approved)
- [ ] No [major design changes](https://github.com/InsightSoftwareConsortium/ITK/blob/master/CONTRIBUTING.md#design-changes) were made (or the changes have been approved)
- [ ] Added test (or behavior not changed)
- [ ] Updated API documentation (or API not changed)
- [ ] Added [license](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Utilities/KWStyle/ITKHeader.h) to new files (if any)
- [ ] Added Python wrapping to new files (if any) as described in [ITK Software Guide](https://itk.org/ItkSoftwareGuide.pdf) Section 9.5
- [ ] Added [ITK examples](https://github.com/InsightSoftwareConsortium/ITKSphinxExamples) for all new major features (if any)

Refer to the [ITK Software Guide](https://itk.org/ItkSoftwareGuide.pdf) for
further development details if necessary.

<!-- **Thanks for contributing to ITK!** -->
