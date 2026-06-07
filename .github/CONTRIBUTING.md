# Contributing to verifyr2

Thank you for your interest in contributing to `verifyr2`! This document outlines the guidelines and workflows for proposing changes to the package.

## Fixing Typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation without creating a separate issue ticket. 

Please note that these fixes must be made in the *source* files, and the relevant documentation must be regenerated using [roxygen2](https://roxygen2.r-lib.org/articles/roxygen2.html). You can identify the `.R` source file that generates a specific `.Rd` file by reading the comment on the very first line of the `.Rd` file.

## Bigger Changes

If you want to make a larger change, it is a good idea to first file an issue. This ensures the core team agrees that the change is needed and confirms that the feature idea and specifications fit the scope of the library. 

Alternatively, you can create a pull request (PR) directly with clear documentation and arguments for the new feature's implementation. However, please be aware that this approach can lead to unnecessary work on your behalf if the team disagrees with the implementation strategy or requires substantial rework.

## Reporting Bugs

If you have found a bug, please file a new issue with clear, minimal instructions on how to reproduce the problem. We highly recommend using the `reprex` package to generate a reproducible example.

## Feature Requests

If you have an idea for an improvement or a new feature, please file a new issue containing as much detail and initial specification as possible.

### Pull Request Process

* **Fork and Clone:** Fork the package and clone it onto your computer. If you haven't done this before, we recommend running:
    ```R
    usethis::create_from_github("ejuhjav/verifyr2", fork = TRUE)
    ```
    Alternatively, you can fork the project directly via the GitHub web interface and clone it manually.

* **Install Dependencies and Check:** Install all development dependencies and ensure the package passes R CMD check by running:
    ```R
    devtools::install_dev_deps()
    devtools::check()
    ```
    If the R CMD check does not pass cleanly, it is a good idea to ask for help before continuing. 
    
    *Currently, there are two acceptable `NOTE`s specific to the Linux environment:*
    ```text
    ❯ checking for future file timestamps ... NOTE
      unable to verify current time

    ❯ checking for detritus in the temp directory ... NOTE
      Found the following files/directories:
        ‘com.google.Chrome.b4jO2h’ ‘com.google.Chrome.scoped_dir.cMeqSf’
    ```

* **Create a Branch:** Create a Git branch for your pull request. We recommend using:
    ```R
    usethis::pr_init("brief-description-of-change")
    ```

* **Submit the PR:** Make your changes, commit them to Git, and create a PR by running `usethis::pr_push()`, then follow the prompts in your browser.
    * The **title** of your PR should briefly describe the change.
    * The **body** of your PR should contain `Fixes #issue-number` if it addresses an open issue.

* **Version Numbers and NEWS:** Do not modify the version number or the `NEWS.md` file. The core library team will handle these updates once the pull request has been approved.

### Code Style

* **Consistency:** New code must follow the project's existing code style, file structure, and naming conventions:
    * **Indentation:** Use two spaces. Never use tabs for indentation.
    * **Long Signatures:** For long function calls or signatures, place each parameter on a separate line with an additional 2-space indentation.
    * **Test Files:** Follow existing test naming and division conventions; maintain separate test files for summary and comparison tests.
    * **File Usage:** Adhere to additional file usage conventions; use separate files for Shiny app examples and for testing.

* **Documentation:** We use [roxygen2](https://cran.r-project.org/package=roxygen2) with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html) for documentation. Any additions or changes to code documentation mean that the documentation must be regenerated using `devtools::document()`.

* **Unit Tests:** We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. Contributions that include comprehensive test cases are much easier to accept and merge.

* **Scope of Changes:** Do not modify code that is unrelated to your feature or fix. This can accidentally happen if your local IDE settings automatically reformat code upon saving. If you notice an area outside your task that needs style or code improvement, please open a separate issue for it.

## Practical Example: Creating a New Comparator Class

If you have a use case where you need `verifyr2` to compare a new file type, you can implement a new comparator class yourself! Here is a checklist to guide you through that process:

1.  **Open an Issue:** Create a new issue on [GitHub](https://github.com/ejuhjav/verifyr2/issues) detailing the specifications for the new comparator. Mention any external R libraries required to read the file contents, and outline how those contents will be converted into a string representation if post-processing is required.

2.  **Add Example Files:** Create at least two example files with identical base contents but a small number of differences. Add these files to:
    * Both Shiny example folders (using identical names).
    * The testing folders (using explicitly different, descriptive names).

3.  **Write Tests:** Create two test files: one for summary testing and one for details testing. You can copy initial test structures from existing comparator tests. If the new comparator relies on an optional external R library, implement conditional tests covering scenarios where that package is both available and unavailable.

4.  **Implement the Class:** Implement the new comparator class and its corresponding `vrf_contents` function to extract the file contents as a string. 
    > **Note:** The comparator class name must match the file type it targets (e.g., an extension of `"pdf"` must map to `PdfFileComparator`).
5.  **Verify Locally:** Execute your new comparator tests and manually verify that the expected differences highlight correctly within the Shiny application.

6.  **Document:** Update the documentation for the new comparator class and its dependencies using `roxygen2`.

7.  **Run Final Checks:** Execute `devtools::check()`. This should return 0 errors, 0 warnings, and a maximum of the 2 acceptable notes mentioned in the PR process section.

8.  **Submit:** Create your pull request and assign it to a maintainer for review.
