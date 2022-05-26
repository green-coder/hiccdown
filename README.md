<img src="doc/hiccdown-logo-web.svg" width="100%"/>

# hiccdown
[![CircleCI](https://circleci.com/gh/green-coder/hiccdown.svg?style=svg)](https://circleci.com/gh/green-coder/hiccdown)

[![Clojars Project](https://img.shields.io/clojars/v/hiccdown.svg)](https://clojars.org/hiccdown)
[![Cljdoc badge](https://cljdoc.org/badge/hiccdown/hiccdown)](https://cljdoc.org/d/hiccdown/hiccdown/CURRENT)
[![Project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C036LVCSL91)
[![Clojars download_badge](https://img.shields.io/clojars/dt/hiccdown?color=opal)](https://clojars.org/hiccdown)

> "Don't look up, look down !"
>
> -- President Orlean

Hiccdown is a pure CLJC library designed to translate documents
back and forth between the Markdown format, an AST data, and the Hiccup data structure.

## Project status

**The implementation is incomplete, do not use.**

See the tests in `test/spec_test.cljs` to see what is working and what is not.

## Features (wip)

- Compatible with the [latest (v0.30) CommonMark specification](https://spec.commonmark.org/0.30/)
- Pure **CLJC implementation**, no JS or Java dependencies
- Conversions between `markdown <-> AST <-> hiccup`

## Installation

[![Clojars Project](http://clojars.org/hiccdown/latest-version.svg)](http://clojars.org/hiccdown)

## Usage (wip)

```clojure
(require '[hiccdown.core :as hd])

;; Transform a markdown document into hiccup data.
(-> markdown-str hd/markdown->ast hd/ast->hiccup)

;; Same as above, in the other direction. (Not implemented yet)
(-> hiccup-data hd/hiccup->ast hd/ast->markdown)
```

## Running the tests

Run `npm i` once, then:

```shell
./bin/kaocha --watch
```

## Alternative Clojure(script) libs

In alphabetical order:

- https://github.com/askonomm/clarktown
- https://github.com/bitterblue/commonmark-hiccup
- https://github.com/chameco/Hitman
- https://github.com/danneu/klobbdown
- https://github.com/genmeblog/rmarkdown-clojure
- https://github.com/kiranshila/cybermonday
- https://github.com/malcolmsparks/clj-markdown
- https://github.com/markwoodhall/marge
- https://github.com/mpcarolin/markdown-to-hiccup
- https://github.com/yogthos/markdown-clj

(Open an issue if you wish to have your library added to the list)

## License

Copyright © 2022 Vincent Cantin

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
