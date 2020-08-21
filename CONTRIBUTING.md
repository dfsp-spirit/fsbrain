## Contributing to fsbrain

I am very happy to accept [pull requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request), provided you are fine with publishing your work under the [license of this project](https://github.com/dfsp-spirit/fsbrain/#license). If your PR is not just a fix but changes or adds lots of code, please get in touch by [opening an issue](https://github.com/dfsp-spirit/fsbrain/issues) before starting the project so we can discuss it first. Development currently happens on the *develop* branch.

### Contribution workflow

If you want to contribute something, the general workflow is:

- Log into the Github website, visit the fsbrain repo page, and click *fork* to fork the fsbrain repository to your account.
- Checkout your forked repository to your computer. You will be on the master branch. Make sure to switch to the *develop* branch.
- Create a new branch off *develop* and name it after your feature, e.g., `add_cool_new_feature` or `fix_issue_17`.
- Make changes to the fsbrain code and commit them into your branch. Repeat as needed.
- Make sure the unit tests are all green and that the code passes the R checks. Adding new tests for your code is a great idea, of course.
- When you are happy with your changes, create a pull request, requesting to merge your branch into the *develop* branch of my fsbrain repo.

### Setting up the development environment

Most likely you already have your development environment setup the way you prefer it when you decide to contribute. If not, here is a quick way to get started.

Note that you do not have to use rstudio of follow these suggestions, any editor or IDE will do.

- Make sure you have a recent R version installed.
- Download and install the latest version of rstudio (I use the free RStudio Desktop Open Source Edition).
- In your shell, change into your checkout of your fork of fsbrain (see above) and run `rstudio fsbrain.Rproj`.
- Install the required development packages listed on the fsbrain website.
- In rstudio, make sure all required development packages are installed and you are ready to go:
  * Build the package including the documentation (`Menu > Build > Build Source Package`) and install/load it (`Menu > Build > Clean and Rebuild`).
  * Run the unit tests (`Menu > Build > Check Package`).
  * Generate the test coverage report (`Item bar > Addins > Report test coverage for a package`). Wait until the report shows up in the Viewer.
- If this completed without errors, you are ready to make your changes in a new branch, as explained above.
