# GramMate

GramMate is a small collection of tools for working with TextMate grammars. I felt the need for improved tooling especially when it comes to these two absent features:

- There is no programmatic way to check if the regular expressions you wrote are correct.
- Pattern re-use is limited. For example you can refer to another rule but you are unable to rename rules.

In a typicial obsessive programmar fashion I wrote a bunch of Clojure code that does the following things:

- Pretty prints Clojure data as Texmate flavoured property lists
- Tests regex patterns using generative testing

I have used these to write a TextMate grammar for EDN which is also included. The plan is to extend this to a complete Clojure grammar. I will look into making making the regexes themself more composable since all the testing resulted in abnormally long patterns.

Use at your own peril. I am in no way commited to seeing this project all the way through and work on it whenever I feel like it.

## Usage

SOMEDAY
