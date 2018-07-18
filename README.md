# ICJIA R User Group

Welcome to ICJIA R User Group GitHub repository!

This repository is mainly intended as a discussion forum (see [Issues section](#issues)) as well as a collaborative "wiki" document (see [Wiki section](#wiki)) for the User Group. The repository may later include some sample R scripts and simulated or publicly available data files for the User Group members' use. ***No file containing any confidential/protected information should be uploaded to this repository.***

Please read the following sections to find more about how to use this repository.

## Markdown
GitHub allows the use of powerful Markdown syntax for Issues as well as Wiki pages. Please take advantage of this feature to make each of your contribution more effectively communitcated to others. Read ["Mastering Markdown" by GitHub Guides](https://guides.github.com/features/mastering-markdown/) to find more about Markdown syntax for GitHub.

### Code chunk
Adding code chunk is a great way to include a piece of your R code in a document. The basic syntax for creating a code chunk is as follows:
````
```r
# your R code here
```
````

Please note that adding "r" after three backticks (\`\`\`) in the opening line will enable GitHub to highlight the code according to the R syntax and thereby improve the readability of your R code. See below for an example:

```r
# here is an example

a <- 123 + 456

my_function <- function() {
  print("hello world!")
}
```

## Issues
[Issues for the current repository](https://github.com/bobaekang/icjia_r_user_group/issues) is intended for having discussions relevant to R and related topics. Any User Group member can "open" a new Issue by clicking "New issue" button (colored green) on the right top of the Issues page. Read ["Mastering Issues" by GitHub Guides](https://guides.github.com/features/issues/) to find more about working with Issues.

### Title and body
The title of each Issue must be succinctly phrased with informative keywords to express its key idea.

If the body of an Issue cannot be captured in few paragraphs, it must start with a clearly marked summarizing paragraph to help others to quickly grasp its key idea. Also, issues with R code questions must include at least one code chunk in the body for other User Group members to use to reproduce the output and find solutions.

### Label
Adding labels to each Issue can quickly communicate to others what the created Issue is about. Labels can be added by using the "Labels" menu on the right side of the main text area for a new Issue page.

There are currently five labels available:
* `help wanted`: Problems or questions
* `sharing`: Sharing information and/or resources
* `wiki`: Ideas and suggestions for wiki pages
* `meeting`: Ideas for suggestions for meetings
* `suggestion`: Ideas and suggestions in general

You may, of course, open an Issue without any label attached. However, it is strongly recommended to add relevant labels and categorize Issues accordingly so that other User Group members (including future you!) can quickly find and refer to older Issues when needed.

If you beileve that new labels are needed for better categorization of Issues, please open a new Issue with `suggestion` label to make relevant suggestions instead of arbitrarily adding ones.

### Mentioning others
You can mention specific User Group members with `@userid` syntax. Use this to direct a particualr member's attention to the Issue. Please be mindful and know that the mentioned person will be getting a notification email.

## Wiki
[Wiki for the current repository](https://github.com/bobaekang/icjia_r_user_group/wiki) is intended to serve as a collaborative documentation for (1) common procedures to work with datasets at ICJIA as well as their R implementations, (2) useful methods for analyzing data as well as their R implementations, and (3) any other knowledge (related to R or not) that merits being shared with the User Group members. Read ["Documenting your projects on GitHub" by GitHub Guides](https://guides.github.com/features/wikis/) to find more about working with Wiki.

### Adding new pages
Before adding new Wiki pages, you should open an Issue with the `wiki` label to share your idea for new Wiki pages with other User Group members. One key reason for this requirement is to devise and maintain an informative naming scheme for Wiki pages. Also, through discussion, we may find that the suggested content for a new Wiki page can be better incorporated into a subsection of an existing Wiki page.

Once a suggestion is made for a new Wiki page, anyone can offer feedback to the suggestion. With support from more than three people, the original poster (i.e. the person who suggested the new page) may proceed to author the suggested Wiki page.

### Editing existing pages
Anyone can edit any of the exiting Wiki pages. If the edit consists of relatively minor changes (e.g. fixing typos, broken links, wrong R syntax, etc.), you should simply go ahead and make the changes. However, if the edit involves substantial changes, you should open an Issue with the `wiki` label to share your idea for the edit. It is recommended to include the original author of the page in the Issue dicussion possibly by "mentioning" the original author (`@userid`).
