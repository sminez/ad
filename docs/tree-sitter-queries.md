# Writing tree-sitter queries for ad

### Overview

`ad` currently supports a subset of the tree-sitter based syntax highlighting you may be familiar
with from other text editors. The configuration options in your `config.toml` under the `tree_sitter`
key will direct `ad` to search for scheme highlights files under a given directory for the languages
that you configure. These query files can, for the most part, be copied from other editors but there
are a couple of caveats that you should keep in mind.

  1. Support for custom predicates (such as neovim's `#lua-match?` and `#contains?` is not provided.
     Queries using unsupported predicates will be rejected, resulting in tree-sitter not being enabled
     for the language in question.
  2. ad follows the same precedence approach as neovim for overlapping queries, with the _last_ match
     being used in place of previous matches. When writing your `highlights.scm` file you should place
     more specific rules towards the end of the file and more general rules towards the start.
  3. ad does not currently provide any support for indentation, injections, locals or folds. So the
     resulting highlights you obtain may not exactly match what you are used to from another editor if
     you are copying over existing queries.


### Getting started

ad provides some support for tree-sitter parsers that I have been able to test and verify as part of my
local setup. For the most part they are adapted from the ones found in [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries)
with the the modigications outlined below. If you are testing your own queries it is often better to start
with a subset of the `highlights.scm` file from another editor and incrementally add in more queries
while verifying that the results in ad look the way that you expect.

Beyond that simple advice, the tree-sitter documentation on writing queries is the best place to start
if you are looking to configure your own custom queries:
  https://tree-sitter.github.io/tree-sitter/using-parsers/queries/index.html


### Troubleshooting

- **Where are my highlights that I configured?**
  - First make sure that you don't have any typos in your config (we all do it) and that the highlighting
    provided in the ad repo is working correctly.
  - If you are sure that you have things set up correctly then try running `:view-logs` to see if there
    are any warnings about the queries that you are using. You should see a log line asserting that the
    tree-sitter parser has been initialised, and if not there should be an error message explaining why
    setup failed.

- **What is this warning about unsupported predicates?**
  - A common feature of tree-sitter queries is the use of [predicates](https://tree-sitter.github.io/tree-sitter/using-parsers/queries/3-predicates-and-directives.html)
    to conditionally select nodes in the tree based on their content as well as their type. Tree-sitter
    supports providing and running custom predicates but it is up to the program running the query to
    provide the implementation of how custom predicates are applied. `neovim` and other editors tend to
    make extensive use of custom predicates that are not supported inside of ad. If a query you are
    using contains any unsupported predicates it will be rejected with warning logs listing the predicates
    that were found.
  - Typically, you can replace `#lua-match?` with the tree-sitter built-in `#match?` predicate and have
    it produce the expected result.
  - You can also replace `#contains?` with `#match? .. ".*$contained_string.*"`

- **Why are comments not rendering with the correct styling?**
  - Comment queries from neovim frequently have a secondary `@spell` tag associated with them that ad
    will end up using as the tag for the match (in place of, for example, `@comment`). If you remove
    the `@spell` tag from your query you should see your comments receive the correct highlighting.
